use std::fmt::{self, Debug, Display, Formatter, LowerExp, UpperExp};
use std::mem::take;
use std::ops::*;

use data::*;
use error::*;
use error::DimenError::*;
use expression::*;
use expression::api::expr_tree_to_infix;
use expression::error::ExprError;
use expression::expr_structs::*;
use expression::expr_structs::ExprTree::{Leaf, Operation};
use expression::expr_structs::ExprUnit::{Num, Op, Unk};
use macros::*;
use traits::*;

use self::GeneDimen::*;
use self::SimpDimenBase::*;

pub mod data;
pub mod error;
pub mod utils;
pub mod macros;
pub mod traits;

fn create_simp_dimen(value: ExprTree, unit: String, custom_units: &CustomUnits) -> NewDimenRes<SimpDimen> {
    for custom_unit in custom_units {
        if custom_unit.verify_unit(&unit).is_ok() {
            return Ok(custom_unit.init_clone(value, &unit)?);
        }
    }

    let base = SimpDimen::get_unit_base(&unit)?;
    SimpDimen::init(value, &unit, base)
}

fn split_value_unit(text: &str) -> (&str, &str) {
    if let Some((v, u)) = text.split_once("_u_") {
        (v, u)
    } else if let Some(pos) = text.rfind(|x: char| x.is_numeric()) {
        text.split_at(pos + 1)
    } else {
        if let Ok(_) = SimpDimen::get_unit_base(text) {
            ("1", text)
        } else {
            (text, "")
        }
    }
}

fn get_unit_based_on_op(op: &ExprUnit, l_unit: ExprTree, r_value: &mut ExprTree,
                        r_unit: ExprTree) -> Result<ExprTree, DimenError> {
    match op {
        Op(Tier::Tier1, i) => {
            if l_unit == r_unit {
                Ok(l_unit)
            } else {
                let op_str = if *i { "add" } else { "sub" };

                Err(OperationError(
                    GeneDimen::get_name(),
                    l_unit.to_string(),
                    op_str,
                    GeneDimen::get_name(),
                    r_unit.to_string(),
                ))
            }
        }
        Op(Tier::Tier3, _) => {
            if let Leaf(Unk(r_unit_str)) = &r_unit {
                if !r_unit_str.is_empty() {
                    Err(OperationError(
                        GeneDimen::get_name(),
                        l_unit.to_string(),
                        "pow",
                        GeneDimen::get_name(),
                        r_unit.to_string(),
                    ))
                } else if let Leaf(Unk(l_unit_str)) = &l_unit {
                    if !l_unit_str.is_empty() {
                        let pow = std::mem::replace(r_value, ONE);
                        Ok(ExprTree::make_opr(l_unit, pow, op.clone()))
                    } else {
                        Ok(r_unit)
                    }
                } else { unreachable!() }
            } else { unreachable!() }
        }
        _ => {
            match (l_unit, r_unit) {
                (l_unit, Leaf(Unk(x))) if x.is_empty() => {
                    Ok(l_unit)
                }
                (Leaf(Unk(x)), r_unit) if x.is_empty() => {
                    let incr = op.clone().unwrap_op().1;
                    let op = Op(Tier::Tier1, incr);
                    let exp = ExprTree::clean(ZERO, ONE, op).parse_err()?;
                    ExprTree::clean(r_unit, exp, POW).parse_err()
                }
                (l_unit, r_unit) => Ok(ExprTree::make_opr(l_unit, r_unit, op.clone()))
            }
        }
    }
}

type CustomUnits = Vec<SimpDimen>;

fn separate_value_unit<F1, F2>(
    tree: ExprTree,
    dict: &mut UnitsDict,
    split_value_unit: &F1,
    get_unit: &F2) -> Result<(ExprTree, ExprTree), DimenError>
where
    F1: Fn(&str) -> (&str, &str),
    F2: Fn(&mut UnitsDict, &mut ExprTree, &mut String) -> Result<(), DimenError>,
{
    match tree {
        Operation(left, right, op) => {
            let (l_value, l_unit) = separate_value_unit(*left, dict, split_value_unit, get_unit)?;
            let (mut r_value, r_unit) = separate_value_unit(*right, dict, split_value_unit, get_unit)?;

            let unit = get_unit_based_on_op(&op, l_unit, &mut r_value, r_unit)?;
            let value = ExprTree::make_opr(l_value, r_value, op);

            Ok((value, unit))
        }
        Leaf(Num(value)) => Ok((Leaf(Num(value)), Leaf(Unk(String::new())))),
        Leaf(Unk(unk)) => {
            let (value, unit) = split_value_unit(&unk);

            let mut value = if let Ok(num) = value.parse::<f64>() {
                Leaf(Num(num))
            } else {
                Leaf(Unk(value.to_string()))
            };

            let mut unit = unit.to_string();

            get_unit(dict, &mut value, &mut unit)?;

            Ok((value, Leaf(Unk(unit))))
        }
        _ => unreachable!()
    }
}

fn take_value_unit<T: AsRef<str>>(expr: T, custom_units: &CustomUnits) -> Result<(ExprTree, ExprTree, UnitsDict), DimenError> {
    let expr = expr.as_ref();
    let mut dict = UnitsDict::new();
    let tree = ExprTree::raw_new(expr).parse_err()?;

    let (mut value, mut unit) = separate_value_unit(
        tree, &mut dict, &split_value_unit, &|dict, value, unit| {
            dict.take_unit_with_custom(value, unit, custom_units)
        },
    )?;

    value = simplify(value).parse_err()?;
    unit = simplify(unit).parse_err()?;

    Ok((value, unit, dict))
}

fn take_custom_dimen_name(dimen: &SimpDimen) -> &'static str {
    if let Custom(name) = dimen.base {
        name
    } else {
        panic!("Passed a non-custom SimpDimen to custom_units parameter.")
    }
}

#[derive(Clone, Default, PartialEq)]
struct UnitsDict {
    length: Option<String>,
    time: Option<String>,
    mass: Option<String>,
    el_current: Option<String>,
    temperature: Option<String>,
    am_of_substance: Option<String>,
    luminous_in: Option<String>,
    angle: Option<String>,
    custom: Vec<SimpDimen>,
}

impl UnitsDict {
    fn new() -> UnitsDict {
        UnitsDict::default()
    }

    fn overwrite(&mut self, units: &[&str]) -> Result<(), DimenError> {
        let custom = take(&mut self.custom);
        *self = UnitsDict::new();
        self.custom = custom;

        for unit in units {
            self.take_unit_with_custom_no_write(&mut ZERO, &mut unit.to_string())?;
        }

        Ok(())
    }

    fn take_custom_unit_no_write(&mut self, current_value: &mut ExprTree, current_unit: &mut String) -> Option<()> {
        let mut final_unit = None;

        for unit in &self.custom {
            if unit.verify_unit(current_unit).is_ok() {
                let name = take_custom_dimen_name(unit);

                if let Some(unit) = self.custom.iter().find(|x|
                take_custom_dimen_name(&x) == name) {
                    let mut new_custom = unit.init_clone(take(current_value), current_unit).unwrap();
                    *current_unit = unit.get_unit().to_string();
                    new_custom.bcm_unit_proportion(current_unit).unwrap();
                    *current_value = new_custom.value;

                    final_unit = Some(());
                }
            }
        }

        final_unit
    }

    fn take_custom_unit(&mut self, current_value: &mut ExprTree, current_unit: &mut String, custom_units: &CustomUnits) -> Option<()> {
        let mut final_unit = None;

        for unit in custom_units {
            if unit.verify_unit(current_unit).is_ok() {
                let name = take_custom_dimen_name(unit);

                if let Some(unit) = self.custom.iter().find(|x|
                take_custom_dimen_name(&x) == name) {
                    let mut new_custom = unit.init_clone(take(current_value), current_unit).unwrap();

                    *current_unit = unit.get_unit().to_string();
                    new_custom.bcm_unit_proportion(current_unit).unwrap();
                    *current_value = new_custom.value;

                    final_unit = Some(());
                } else {
                    let mut custom = unit.clone();
                    custom.bcm_unit_proportion(current_unit).unwrap();

                    self.custom.push(custom);
                    final_unit = Some(());
                }
            }
        }

        final_unit
    }

    fn take_field(&mut self, unit_base: SimpDimenBase) -> Option<&mut Option<String>> {
        match unit_base {
            Length => Some(&mut self.length),
            Time => Some(&mut self.time),
            Mass => Some(&mut self.mass),
            ElCurrent => Some(&mut self.el_current),
            Temperature => Some(&mut self.temperature),
            AmOfSubstance => Some(&mut self.am_of_substance),
            LuminousIn => Some(&mut self.luminous_in),
            Angle => Some(&mut self.angle),
            _ => return None
        }
    }

    fn take_unit(&mut self, current_value: &mut ExprTree, current_unit: &mut String) -> Result<(), DimenError> {
        let unit_base = SimpDimen::get_unit_base(current_unit)?;

        let field = if let Some(fild) = self.take_field(unit_base) {
            fild
        } else {
            return Ok(());
        };

        if let Some(unit) = field {
            let mut sd = SimpDimen::init(take(current_value), current_unit, unit_base).unwrap();
            sd.bcm_unit_proportion(unit).unwrap();
            *current_value = sd.value;
            *current_unit = unit.clone();

            Ok(())
        } else {
            *field = Some(current_unit.clone());
            Ok(())
        }
    }

    fn take_unit_with_custom(&mut self, current_value: &mut ExprTree, current_unit: &mut String, custom_units: &CustomUnits) -> Result<(), DimenError> {
        if let Some(()) = self.take_custom_unit(current_value, current_unit, custom_units) {
            Ok(())
        } else {
            self.take_unit(current_value, current_unit)
        }
    }

    fn take_unit_with_custom_no_write(&mut self, current_value: &mut ExprTree, current_unit: &mut String) -> Result<(), DimenError> {
        if let Some(()) = self.take_custom_unit_no_write(current_value, current_unit) {
            Ok(())
        } else {
            self.take_unit(current_value, current_unit)
        }
    }


    fn new_si() -> Self {
        UnitsDict {
            length: Some("m".to_string()),
            time: Some("s".to_string()),
            mass: Some("kg".to_string()),
            el_current: Some("A".to_string()),
            temperature: Some("K".to_string()),
            am_of_substance: Some("mole".to_string()),
            luminous_in: Some("cd".to_string()),
            angle: Some("°".to_string()),
            custom: vec![],
        }
    }

    fn bcm_si(&mut self) {
        for unit in &mut self.custom {
            unit.bcm_si()
        }

        let custom = take(&mut self.custom);
        *self = Self::new_si();
        self.custom = custom
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum SimpDimenBase {
    Length,
    Time,
    Mass,
    ElCurrent,
    Temperature,
    AmOfSubstance,
    LuminousIn,
    Angle,
    ND,
    Custom(&'static str),
}

impl Display for SimpDimenBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub const SIMP_DIMEN_BASES: [SimpDimenBase; 9] = [
    Length,
    Time,
    Mass,
    ElCurrent,
    Temperature,
    AmOfSubstance,
    LuminousIn,
    Angle,
    ND,
];

#[derive(Clone, PartialEq)]
pub struct SimpDimen {
    value: ExprTree,
    prefix: usize,
    unit: usize,
    base: SimpDimenBase,
    si_unit: usize,
    prefixes: &'static [(&'static str, f64)],
    units: &'static [&'static str],
    conv_rates: &'static [(ConvFromSi, ConvToSi)],
    proportions: &'static [(ConvFromSi, ConvToSi)],
}

impl SimpDimen {
    pub fn make_tree(value: &str) -> Result<ExprTree, DimenError> {
        ExprTree::new(value).map_err(|err| DimenError::from(err))
    }

    fn get_dimen_unit_pos(&self, unit: &str) -> usize {
        self.units.iter().position(|x| x == &unit).unwrap()
    }

    fn get_base_info(base: SimpDimenBase) -> SimpDimen {
        let info: (usize, &[&str], &[(ConvFromSi, ConvToSi)], &[(ConvFromSi, ConvToSi)]);

        match base {
            Length => info = (3, &LENGTH_INFO.0, &LENGTH_INFO.1, &LENGTH_INFO.2),
            Time => info = (6, &TIME_INFO.0, &TIME_INFO.1, &TIME_INFO.2),
            Mass => info = (1, &MASS_INFO.0, &MASS_INFO.1, &MASS_INFO.2),
            ElCurrent => info = (0, &EL_CURRENT_INFO.0, &EL_CURRENT_INFO.1, &EL_CURRENT_INFO.2),
            Temperature => info = (2, &TEMPERATURE_INFO.0, &TEMPERATURE_INFO.1, &TEMPERATURE_INFO.2),
            AmOfSubstance => info = (0, &AM_OF_SUBSTANCE_INFO.0, &AM_OF_SUBSTANCE_INFO.1, &AM_OF_SUBSTANCE_INFO.2),
            LuminousIn => info = (0, &LUMINOUS_IN_INFO.0, &LUMINOUS_IN_INFO.1, &LUMINOUS_IN_INFO.2),
            Angle => info = (1, &ANGLE_INFO.0, &ANGLE_INFO.1, &ANGLE_INFO.2),
            ND => info = (0, &ND_INFO.0, &ND_INFO.1, &ND_INFO.2),
            Custom(_) => {
                panic!("Custom dimensions must be created using `SimpDimen::create_custom`.")
            }
        }

        let (prefixes, prefix): (&[(&str, f64)], usize) = match base {
            ND => (&ND_PREFIXES, 0),
            _ => (&PREFIXES, 10),
        };

        SimpDimen {
            value: ZERO,
            prefix,
            unit: info.0,
            base,
            si_unit: info.0,
            prefixes,
            units: info.1,
            conv_rates: info.2,
            proportions: info.3,
        }
    }

    fn separate_unit(&self, unit: &str) -> Option<(usize, usize)> {
        if self.is_nd() || unit.is_empty() {
            return None;
        }

        let mut chars = unit.chars();
        let (prefix, unit) = (chars.next().unwrap(), String::from_iter(chars));

        if let (Some(prefix_pos), Some(unit_pos)) = (
            self.prefixes
                .iter()
                .position(|(x, _)| x == &prefix.to_string()),
            self.units.iter().position(|x| x == &unit),
        ) {
            Some((prefix_pos, unit_pos))
        } else {
            None
        }
    }

    fn verify_unit(&self, unit: &str) -> Result<(usize, usize), DimenError> {
        if self.units.contains(&unit) {
            let prefix = if self.base != ND { 10 } else { 0 };
            Ok((prefix, self.get_dimen_unit_pos(unit)))
        } else {
            self.separate_unit(unit).ok_or(UnitError(
                Self::get_name(),
                Some(self.get_base_to_display()),
                unit.to_string(),
            ))
        }
    }

    pub fn val_to_display<U>(value: &ExprTree, unit: U, show_unit: bool) -> String
    where
        U: Display,
    {
        if show_unit {
            format!("{} {}", expr_tree_to_infix(value, ""), unit)
        } else {
            value.to_string()
        }
    }

    pub fn remove_prefix(&mut self) {
        self.value *= self.prefixes[self.prefix].1;
        self.prefix = if self.base != ND { 10 } else { 0 };
    }

    pub fn bcm_unit_unchecked(&mut self, prefix: usize, unit: usize, is_proportion: bool) {
        if unit != self.unit || prefix != self.prefix {
            let rates = if is_proportion { self.proportions } else { self.conv_rates };
            self.bcm_si_by_rates(is_proportion);

            self.value = rates[unit].0(take(&mut self.value)) * self.prefixes[prefix].1.powi(-1);
            self.prefix = prefix;
            self.unit = unit;
        }
    }

    fn bcm_si_by_rates(&mut self, is_proportion: bool) {
        if (self.unit != self.si_unit || self.prefix != 10) && self.base != ND {
            let rates = if is_proportion { self.proportions } else { self.conv_rates };
            self.remove_prefix();
            let unit = self.si_unit;
            self.value = rates[self.unit].1(take(&mut self.value));
            self.unit = unit;
        }
    }

    fn bcm_unit_proportion(&mut self, unit: &str) -> Result<(), DimenError> {
        let (prefix, unit) = self.verify_unit(unit)?;
        self.bcm_unit_unchecked(prefix, unit, true);
        Ok(())
    }

    pub fn new(base: SimpDimenBase) -> SimpDimen {
        Self::get_base_info(base)
    }

    pub fn create_custom(
        name: &'static str,
        si: &str,
        units: &'static [&'static str],
        conv_rates: &'static [(ConvFromSi, ConvToSi)],
        proportions: &'static [(ConvFromSi, ConvToSi)],
    ) -> NewDimenRes<SimpDimen> {
        if units.len() != conv_rates.len() {
            return Err(DiffLenError(Self::get_name(), "units", "conv_rates"));
        }
        if units.len() != proportions.len() {
            return Err(DiffLenError(Self::get_name(), "units", "proportions"));
        }

        let unit_err = UnitError(
            Self::get_name(),
            Some(Custom(name).to_string()),
            si.to_string(),
        );
        let unit = units.iter().position(|x| x == &si).ok_or(unit_err)?;

        Ok(SimpDimen {
            value: ZERO,
            prefix: 10,
            unit,
            base: Custom(name),
            si_unit: unit,
            prefixes: &PREFIXES,
            units,
            conv_rates,
            proportions,
        })
    }

    fn init(value: ExprTree, unit: &str, base: SimpDimenBase) -> NewDimenRes<SimpDimen> {
        let base = Self::get_base_info(base);
        let (prefix, unit) = base.verify_unit(unit)?;

        Ok(SimpDimen {
            value,
            prefix,
            unit,
            ..base
        })
    }

    pub fn init_nd(value: ExprTree) -> SimpDimen {
        SimpDimen {
            value,
            ..Self::get_base_info(ND)
        }
    }

    pub fn init_clone(&self, value: ExprTree, unit: &str) -> NewDimenRes<SimpDimen> {
        let (prefix, unit) = self.verify_unit(unit)?;

        Ok(SimpDimen {
            value,
            prefix,
            unit,
            ..self.clone()
        })
    }

    pub fn init_unchecked(
        value: ExprTree,
        prefix: usize,
        unit: usize,
        base: SimpDimenBase,
    ) -> SimpDimen {
        let base = Self::get_base_info(base);

        SimpDimen {
            value,
            prefix,
            unit,
            ..base
        }
    }

    pub fn soft_from<T: AsRef<str>>(expr: T) -> NewDimenRes<SimpDimen> {
        Self::soft_from_with_custom(expr, &Vec::new())
    }

    pub fn soft_from_with_custom<T: AsRef<str>>(expr: T, custom_units: &CustomUnits) -> NewDimenRes<SimpDimen> {
        let (value, unit, _) = take_value_unit(expr, custom_units)?;

        if let Leaf(Unk(unit)) = unit {
            create_simp_dimen(value, unit, custom_units)
        } else {
            Err(UnitError(SimpDimen::get_name(), None, unit.to_string()))
        }
    }

    pub fn get_prefix(&self) -> usize {
        self.prefix
    }

    pub fn get_unit_base(unit: &str) -> Result<SimpDimenBase, DimenError> {
        for base in SIMP_DIMEN_BASES {
            let d = Self::get_base_info(base);
            if d.verify_unit(unit).is_ok() {
                return Ok(base);
            }
        }

        Err(UnitError(Self::get_name(), None, unit.to_string()))
    }
}

impl DimenBasics for SimpDimen {
    fn is_nd(&self) -> bool {
        self.base == ND
    }

    fn bcm_si(&mut self) {
        self.bcm_si_by_rates(false)
    }

    fn bcm_unit(&mut self, unit: &[&str]) -> Result<(), DimenError> {
        let (prefix, unit) = self.verify_unit(unit[0])?;
        self.bcm_unit_unchecked(prefix, unit, false);
        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        GeneDimen::from(self)
    }

    fn verified_add(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, ADD)
    }

    fn verified_sub(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, SUB)
    }

    fn verified_mul(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, MUL)
    }

    fn verified_div(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, DIV)
    }

    fn verified_pow(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, POW)
    }
}

impl DimenSetAndGet for SimpDimen {
    fn set_value(&mut self, other: ExprTree) {
        self.value = other;
    }

    fn get_value(&self) -> ExprTree {
        self.value.clone()
    }

    fn get_move_value(&mut self) -> ExprTree {
        take(&mut self.value)
    }

    fn get_unit(&self) -> ExprTree {
        Leaf(Unk(self.prefixes[self.prefix].0.to_string() + self.units[self.unit]))
    }

    fn get_move_unit(&mut self) -> ExprTree {
        self.get_unit()
    }
    
    fn get_num_or_unit(&self) -> ExprTree {
       let unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];

        if &unit == "" {
            self.value.clone()
        } else { Leaf(Unk(unit)) }
    }

    fn get_move_num_or_unit(&mut self) -> ExprTree {
        let unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];

        if &unit == "" {
            take(&mut self.value)
        } else { Leaf(Unk(unit)) }
    }

    fn get_custom_units(&self) -> CustomUnits {
        if let Custom(_) = self.base {
            vec![self.clone()]
        } else { Vec::new() }
    }

    fn get_move_custom_units(&mut self) -> CustomUnits {
        self.get_custom_units()
    }

    fn get_base_to_display(&self) -> String {
        self.unit.to_string()
    }

    fn get_name() -> &'static str {
        "SimpDimen"
    }
}

impl Display for SimpDimen {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            Self::val_to_display(&self.value, &self.get_unit(), true)
        )
    }
}

impl Debug for SimpDimen {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.is_nd() {
            write!(f, "{}", Self::val_to_display(&self.value, &self.get_unit(), true))
        } else {
            write!(f, "{} ND", expr_tree_to_infix(&self.value, ""))
        }
    }
}

impl From<f64> for SimpDimen {
    fn from(value: f64) -> Self {
        Self::init_nd(Leaf(Num(value)))
    }
}

type CompDimenBase = ExprOper;

#[derive(Clone, PartialEq)]
pub struct CompDimen {
    value: ExprTree,
    base: CompDimenBase,
    dict: UnitsDict,
}

impl CompDimen {
    pub fn get_op(&self) -> ExprUnit {
        tuple_to_op(self.base.op)
    }

    pub fn get_ops() -> [ExprUnit; 3] {
        [MUL, DIV, POW]
    }

    fn propagate_dict(tree: ExprTree, dict: &mut UnitsDict, custom_units: &CustomUnits) -> Result<(ExprTree, ExprTree), DimenError> {
        separate_value_unit(
            tree, dict,
            &|unk| {
                if let Ok(_) = unk.parse::<f64>() {
                    (unk, "")
                } else {
                    ("1", unk)
                }
            },
            &|dict, value, unit| {
                dict.take_unit_with_custom(value, unit, custom_units)
            },
        )
    }

    fn propagate_dict_no_write(tree: ExprTree, dict: &mut UnitsDict) -> Result<(ExprTree, ExprTree), DimenError> {
        separate_value_unit(
            tree, dict,
            &|unk| {
                ("1", unk)
            },
            &|dict, value, unit| {
                dict.take_unit_with_custom_no_write(value, unit)
            },
        )
    }

    fn update_base(&mut self, custom_units: &CustomUnits) -> Result<(), DimenError> {
        let tree = take(&mut self.base).as_expr_tree();
        let (mut value, mut unit) = Self::propagate_dict(tree, &mut self.dict, custom_units)?;

        value = simplify(value).parse_err()?;
        unit = simplify(unit).parse_err()?;

        self.base = ExprOper::new(value, unit, MUL.unwrap_op(), false);
        self.update_value().expect("The CompDimen::update_value returned an error in CompDimen::update_base.");

        Ok(())
    }

    fn update_base_no_write(&mut self) {
        let tree = take(&mut self.base).as_expr_tree();
        let (mut value, mut unit) = Self::propagate_dict_no_write(tree, &mut self.dict).unwrap();

        value = simplify(value).unwrap();
        unit = simplify(unit).unwrap();

        self.base = ExprOper::new(value, unit, MUL.unwrap_op(), false);
        self.update_value().expect("The CompDimen::update_value returned an error in CompDimen::update_base_no_write.");
    }

    fn update_value(&mut self) -> Result<(), DimenError> {
        if self.base.left.is_num() {
            let multiplier = simplify(take(&mut self.base.left))?;
            let old_value = take(&mut self.value);

            self.value = apply_simplifications(old_value, multiplier, MUL).parse_err()?;

            self.base = if let Leaf(Unk(_)) = &self.base.right {
                ExprOper::new(
                    take(&mut self.base.right),
                    Leaf(Unk(String::new())),
                    MUL.unwrap_op(),
                    true,
                )
            } else {
                ExprOper::from(take(&mut self.base.right))
            }
        }

        Ok(())
    }

    pub fn new(base: CompDimenBase, custom_units: &CustomUnits) -> NewDimenRes<CompDimen> {
        let dict = UnitsDict::new();
        let mut cd = CompDimen { value: ZERO, base, dict };

        cd.update_base(custom_units)?;

        Ok(cd)
    }

    pub fn soft_from<T: AsRef<str>>(expr: T) -> NewDimenRes<CompDimen> {
        Self::soft_from_with_custom(expr, &Vec::new())
    }

    pub fn soft_from_with_custom<T: AsRef<str>>(expr: T, custom_units: &CustomUnits) -> NewDimenRes<CompDimen> {
        let (value, unit, dict) = take_value_unit(expr, custom_units)?;

        if let Leaf(Unk(unit)) = unit {
            Err(UnitError(CompDimen::get_name(), None, unit.to_string()))
        } else {
            Ok(CompDimen { value, base: ExprOper::from(unit), dict })
        }
    }

    pub fn base_by_unit(unit: &str) -> Result<CompDimenBase, DimenError> {
        let mut base = ExprTree::new(unit).parse_err()?;
        base = base.propagate(
            |c1, c2, op| {
                get_unit_based_on_op(&op, c1, &mut ONE, c2)
            },
            |x| Ok(x))?;

        Ok(ExprOper::from(base))
    }
}

impl DimenBasics for CompDimen {
    fn is_nd(&self) -> bool {
        false
    }

    fn bcm_si(&mut self) {
        self.dict.bcm_si();
        self.update_base_no_write();
    }

    fn bcm_unit(&mut self, units: &[&str]) -> Result<(), DimenError> {
        self.dict.overwrite(units)?;
        self.update_base_no_write();

        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        GeneDimen::from(self)
    }

    fn verified_add(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, ADD)
    }

    fn verified_sub(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, SUB)
    }

    fn verified_mul(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, MUL)
    }

    fn verified_div(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, DIV)
    }

    fn verified_pow(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, POW)
    }
}

impl DimenSetAndGet for CompDimen {
    fn set_value(&mut self, other: ExprTree) {
        self.value = other;
    }

    fn get_value(&self) -> ExprTree {
        self.value.clone()
    }

    fn get_move_value(&mut self) -> ExprTree {
        take(&mut self.value)
    }

    fn get_unit(&self) -> ExprTree {
        self.base.clone().as_expr_tree()
    }

    fn get_move_unit(&mut self) -> ExprTree {
        take(&mut self.base).as_expr_tree()
    }

    fn get_num_or_unit(&self) -> ExprTree {
        self.get_unit()
    }

    fn get_move_num_or_unit(&mut self) -> ExprTree {
        self.get_move_unit()
    }

    fn get_custom_units(&self) -> CustomUnits {
        self.dict.custom.clone()
    }

    fn get_move_custom_units(&mut self) -> CustomUnits {
        take(&mut self.dict.custom)
    }

    fn get_base_to_display(&self) -> String {
        format!("{}", expr_tree_to_infix(&self.base.clone().as_expr_tree(), ""),
        )
    }

    fn get_name() -> &'static str {
        "CompDimen"
    }
}

impl Display for CompDimen {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}", expr_tree_to_infix(&self.value, ""), self.get_base_to_display())
    }
}

impl Debug for CompDimen {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum GeneDimenBase {
    GeneSDBase(SimpDimenBase),
    GeneCDBase(CompDimenBase),
}

#[derive(Clone, PartialEq, Debug)]
pub enum GeneDimen {
    GenSimpDimen(SimpDimen),
    GenCompDimen(CompDimen),
}

impl GeneDimen {
    pub fn init(value: ExprTree, unit: ExprTree, custom_units: CustomUnits) -> NewDimenRes<GeneDimen> {
        let gd = if let Leaf(Unk(unit)) = unit {
            create_simp_dimen(value, unit, &custom_units)?.to_generic()
        } else {
            let mut cd = CompDimen { value, base: ExprOper::from(unit), dict: UnitsDict::new() };
            cd.dict.custom = custom_units;
            cd.update_base_no_write();

            if cd.base.is_default {
                create_simp_dimen(cd.value, cd.base.left.to_string(), &cd.dict.custom)?.to_generic()
            } else {
                cd.to_generic()
            }
        };

        Ok(gd)
    }

    pub fn unwrap_sd(self) -> SimpDimen {
        match self {
            GenSimpDimen(sd) => sd,
            GenCompDimen(_) => {
                panic!("Called `GeneDimen::unwrap_sd()` on a `GenCompDimen`.")
            }
        }
    }

    pub fn unwrap_cd(self) -> CompDimen {
        match self {
            GenSimpDimen(_) => {
                panic!("Called `GeneDimen::unwrap_cd()` on a `GenSimpDimen`.")
            }
            GenCompDimen(cd) => cd,
        }
    }

    pub fn soft_from<T: AsRef<str>>(expr: T) -> NewDimenRes<GeneDimen> {
        Self::soft_from_with_custom(expr, &Vec::new())
    }

    pub fn soft_from_with_custom<T: AsRef<str>>(expr: T, custom_units: &CustomUnits) -> NewDimenRes<GeneDimen> {
        let (value, unit, dict) = take_value_unit(expr, custom_units)?;

        let gd = if let Leaf(Unk(unit)) = unit {
            create_simp_dimen(value, unit, custom_units)?.to_generic()
        } else {
            CompDimen { value, base: ExprOper::from(unit), dict }.to_generic()
        };

        Ok(gd)
    }

    pub fn unwrap_to_debug(&self) -> String {
        match self {
            GenSimpDimen(sd) => format!("{:?}", sd),
            GenCompDimen(cd) => format!("{:?}", cd),
        }
    }
}

impl DimenBasics for GeneDimen {
    fn is_nd(&self) -> bool {
        match self {
            GenSimpDimen(sd) => sd.is_nd(),
            GenCompDimen(_) => false,
        }
    }

    fn bcm_si(&mut self) {
        match self {
            GenSimpDimen(ref mut sd) => sd.bcm_si(),
            GenCompDimen(ref mut cd) => cd.bcm_si(),
        }
    }

    fn bcm_unit(&mut self, unit: &[&str]) -> Result<(), DimenError> {
        match self {
            GenSimpDimen(ref mut sd) => sd.bcm_unit(unit)?,
            GenCompDimen(ref mut cd) => cd.bcm_unit(unit)?,
        }
        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        self
    }

    fn verified_add(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, ADD)
    }

    fn verified_sub(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, SUB)
    }

    fn verified_mul(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, MUL)
    }

    fn verified_div(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, DIV)
    }

    fn verified_pow(&mut self, mut other: GeneDimen) -> OperRes {
        apply_verified_ops!(self, other, POW)
    }
}

impl DimenSetAndGet for GeneDimen {
    fn set_value(&mut self, other: ExprTree) {
        match self {
            GenSimpDimen(sd) => sd.set_value(other),
            GenCompDimen(cd) => cd.set_value(other),
        }
    }

    fn get_value(&self) -> ExprTree {
        match self {
            GenSimpDimen(sd) => sd.get_value(),
            GenCompDimen(cd) => cd.get_value(),
        }
    }

    fn get_move_value(&mut self) -> ExprTree {
        match self {
            GenSimpDimen(sd) => sd.get_move_value(),
            GenCompDimen(cd) => cd.get_move_value(),
        }
    }

    fn get_unit(&self) -> ExprTree {
        match self {
            GenSimpDimen(ref sd) => sd.get_unit(),
            GenCompDimen(ref cd) => cd.get_unit(),
        }
    }

    fn get_num_or_unit(&self) -> ExprTree {
        match self {
            GenSimpDimen(ref sd) => sd.get_num_or_unit(),
            GenCompDimen(ref cd) => cd.get_num_or_unit(),
        }
    }

    fn get_move_num_or_unit(&mut self) -> ExprTree {
        match self {
            GenSimpDimen(sd) => sd.get_move_num_or_unit(),
            GenCompDimen(cd) => cd.get_move_num_or_unit(),
        }
    }

    fn get_move_unit(&mut self) -> ExprTree {
        match self {
            GenSimpDimen(sd) => sd.get_move_unit(),
            GenCompDimen(cd) => cd.get_move_unit(),
        }
    }

    fn get_custom_units(&self) -> CustomUnits {
        match self {
            GenSimpDimen(sd) => sd.get_custom_units(),
            GenCompDimen(cd) => cd.get_custom_units(),
        }
    }

    fn get_move_custom_units(&mut self) -> CustomUnits {
        match self {
            GenSimpDimen(sd) => sd.get_move_custom_units(),
            GenCompDimen(cd) => cd.get_move_custom_units(),
        }
    }

    fn get_base_to_display(&self) -> String {
        match self {
            GenSimpDimen(sd) => sd.get_base_to_display(),
            GenCompDimen(cd) => cd.get_base_to_display(),
        }
    }

    fn get_name() -> &'static str {
        "GeneDimen"
    }
}

impl Display for GeneDimen {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenSimpDimen(sd) => {
                write!(f, "{}", sd)
            }
            GenCompDimen(cd) => {
                write!(f, "{}", cd)
            }
        }
    }
}

impl From<SimpDimen> for GeneDimen {
    fn from(value: SimpDimen) -> Self {
        GenSimpDimen(value)
    }
}

impl From<CompDimen> for GeneDimen {
    fn from(value: CompDimen) -> Self {
        GenCompDimen(value)
    }
}

impl<T> ParseErr<T> for Result<T, ExprError> {
    fn parse_err(self) -> Result<T, DimenError> {
        self.map_err(|err| DimenError::from(err))
    }
}

impl<Dimen1, Dimen2> PowD<Dimen2> for Dimen1
where
    Dimen1: DimenBasics + DimenSetAndGet,
    Dimen2: DimenBasics + DimenSetAndGet,
{
        fn powd(mut self, other: Dimen2) -> GeneDimen {
        self.verified_pow(other.to_generic()).map_err(|err| panic!("{}", err)).unwrap()
    }
}

impl<T, Dimen> traits::Pow<T> for Dimen
where
    T: Into<f64>,
    Dimen: DimenBasics + DimenSetAndGet + PowD<SimpDimen>,
{
    fn pow(self, other: T) -> GeneDimen {
        self.powd(SimpDimen::init_nd(Leaf(Num(other.into()))))
    }
}

pass_ds_to_impl_ops!(SimpDimen, CompDimen, GeneDimen);
impl_ops_asn_gd_for_gd!(AddAssign, add_assign, +; SubAssign, sub_assign, -; MulAssign, mul_assign, *; DivAssign, div_assign, /);
impl_exp_for_ds!(SimpDimen, CompDimen, GeneDimen);
impl_from_str_string_for_ds!(SimpDimen, CompDimen, GeneDimen);
