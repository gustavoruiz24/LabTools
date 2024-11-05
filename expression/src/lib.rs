pub mod api;
pub mod expr_structs;
pub mod macros;
pub mod error;
pub mod traits;

use self::ExprTree::{Leaf, Operation};
use self::ExprUnit::*;
use self::Tier::*;
use error::ExprError;
use expr_structs::*;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, LowerExp, UpperExp};
use std::mem::take;

type Expr = Vec<ExprTree>;
pub type ExprResult<T> = Result<T, ExprError>;
type ExprTreeResult = ExprResult<ExprTree>;
pub type CommonFactor = Option<(ExprTree, ExprUnit)>;

pub const ADD: ExprUnit = Op(Tier1, true);
pub const SUB: ExprUnit = Op(Tier1, false);
pub const MUL: ExprUnit = Op(Tier2, true);
pub const DIV: ExprUnit = Op(Tier2, false);
pub const POW: ExprUnit = Op(Tier3, true);
pub const ROOT: ExprUnit = Op(Tier3, false);

pub const ZERO: ExprTree = Leaf(Num(0.0));
pub const ONE: ExprTree = Leaf(Num(1.0));
pub const TEN: ExprTree = Leaf(Num(10.0));

#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub enum Tier {
    #[default]
    Tier1 = 1,
    Tier2,
    Tier3,
}

impl Tier {
    fn get_num(&self) -> i8 {
        *self as i8
    }

    fn plus_one(self) -> Self {
        match self {
            Tier1 => Tier2,
            Tier2 => Tier3,
            Tier3 => panic!("There is no tier above Tier::Tier3")
        }
    }
}

impl PartialOrd for Tier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get_num().partial_cmp(&other.get_num())
    }
    fn lt(&self, other: &Self) -> bool {
        self.get_num().lt(&other.get_num())
    }
    fn le(&self, other: &Self) -> bool {
        self.get_num().le(&other.get_num())
    }
    fn gt(&self, other: &Self) -> bool {
        self.get_num().gt(&other.get_num())
    }
    fn ge(&self, other: &Self) -> bool {
        self.get_num().ge(&other.get_num())
    }
}

pub fn tuple_to_op(tuple: (Tier, bool)) -> ExprUnit {
    Op(tuple.0, tuple.1)
}

pub fn operate_and_simplify(left: ExprTree, right: ExprTree, op: ExprUnit) -> ExprTreeResult {
    ExprTree::make_opr(left, right, op)
        .propagate(ExprTree::clean, |x| Ok(x))?
        .propagate(apply_simplifications, |x| Ok(x))
}

pub fn simplify(tree: ExprTree) -> ExprTreeResult {
    tree.propagate(ExprTree::clean, |x| Ok(x))?
        .propagate(apply_simplifications, |x| Ok(x))
}

pub fn number_to_sci_notation(leaf: ExprTree) -> ExprTreeResult {
    if let Leaf(Num(num)) = leaf {
        let (man, exp) = take_sci_notation_number(num);
        Ok(make_sci_notation(man, exp))
    } else {
        Ok(leaf)
    }
}

pub fn sci_notation_to_number(tree: ExprTree) -> ExprTree {
    if let Operation(c1, c2, E) = tree {
        Leaf(Num(c1.unwrap_num() * 10_f64.powf(c2.unwrap_num())))
    } else { tree }
}

fn take_sci_notation_number(number: f64) -> (f64, f64) {
    if number == 0.0 {
        return (0.0, 0.0);
    }

    let exponent = number.abs().log10().floor();
    let mut mantissa = number / 10_f64.powf(exponent);

    mantissa = (mantissa * 1e10).round() / 1e10;

    (mantissa, exponent)
}

fn make_sci_notation(mantissa: f64, exponent: f64) -> ExprTree {
    if exponent > -2.0 && exponent < 2.0 {
        let mut number = mantissa * 10_f64.powf(exponent);
        number = (number * 1e10).round() / 1e10;
        Leaf(Num(number))
    } else {
        ExprTree::make_opr(Leaf(Num(mantissa)), Leaf(Num(exponent)), E)
    }
}

pub fn operate_nums(num1: f64, num2: f64, op: (Tier, bool)) -> ExprTree {
    if op.0 == Tier3 {
        let result = if op.1 { num1.powf(num2) } else { num1.powf(1.0 / num2) };
        let (man, pow) = take_sci_notation_number(result);
        return make_sci_notation(man, pow);
    }

    let (man1, exp1) = take_sci_notation_number(num1);
    let (man2, exp2) = take_sci_notation_number(num2);

    operate_sci_notation_nums(man1, exp1, man2, exp2, op)
}

fn operate_sci_notation_nums(man1: f64, exp1: f64, man2: f64, exp2: f64, op: (Tier, bool)) -> ExprTree {
    let (man, mut exp) = match op {
        (Tier1, true) => (man1 * 10_f64.powf(exp1 - exp2) + man2, exp2),
        (Tier1, false) => (man1 * 10_f64.powf(exp1 - exp2) - man2, exp2),
        (Tier2, true) => (man1 * man2, exp1 + exp2),
        (Tier2, false) => (man1 / man2, exp1 - exp2),
        _ => unreachable!()
    };

    let (man, new_exp) = take_sci_notation_number(man);
    exp += new_exp;

    make_sci_notation(man, exp)
}

pub fn apply_simplifications(mut child1: ExprTree, mut child2: ExprTree, mut op: ExprUnit) -> ExprTreeResult {
    let mut changed = true;

    while changed {
        changed = false;
        shorten_expr(&mut child1, &mut child2, op.clone(), true, true)?;
        if let Operation(c1, c2, new_op) = child1 {
            (child1, child2, op) = (*c1, *c2, new_op);
        } else {
            return Ok(child1);
        }
    };

    Ok(ExprTree::make_opr(child1, child2, op))
}

fn take_or_clone(expr: &mut ExprTree, can_move: bool) -> ExprTree {
    if can_move {
        take(expr)
    } else { expr.clone() }
}

fn make_opr_from_ref(left: &mut ExprTree, right: &mut ExprTree, op: ExprUnit, can_move: bool) -> ExprTree {
    let left = take(left);
    let right = take_or_clone(right, can_move);

    ExprTree::make_opr(left, right, op)
}

fn distributive(left: &mut ExprOper, right: &mut ExprTree, op: ExprUnit, can_move: bool) -> ExprResult<bool> {
    let changed1 = shorten_expr(&mut left.left, right, op.clone(), false, false)?;
    let can_move2 = changed1 && can_move;
    let changed2 = shorten_expr(&mut left.right, right, op.clone(), can_move2, changed1)?;

    if !(changed1 || changed2) {
        return Ok(false);
    } else if !changed1 {
        left.left = make_opr_from_ref(&mut left.left, right, op, can_move);
    }
    Ok(true)
}

fn match_equivalent(left: &mut ExprOper, right: &mut ExprOper, op: ExprUnit, invert: bool, can_move: bool) -> ExprResult<bool> {
    let (a, mut b) = (&mut left.left, &mut left.right);
    let (mut x, mut y) = (&mut right.left, &mut right.right);

    let (mut r_left_incr, mut r_right_incr) = (true, right.op.1);
    let mut can_move2 = can_move;

    if invert {
        (x, y) = (y, x);
        (r_left_incr, r_right_incr) = (r_right_incr, r_left_incr);
    };

    let main_op_info = op.clone().unwrap_op();

    let op1_incr = main_op_info.1 == r_left_incr;
    let op1 = Op(main_op_info.0, op1_incr);

    let op2_incr = (left.op.1 == main_op_info.1) == r_right_incr;
    let op2 = Op(main_op_info.0, op2_incr);
    let mut final_op_incr = true;

    if !left.op.1 {
        if op2_incr {
            final_op_incr = false;
        } else {
            can_move2 = true;
            (b, y) = (y, b);
        }
    };

    if invert && right.is_default {
        let changed = shorten_expr(b, y, op2, can_move2, false)?;
        if !changed { return Ok(false); };

        shorten_expr(a, x, op1.clone(), can_move, true)?;
    } else if left.is_default || right.is_default {
        let changed = shorten_expr(a, x, op1, can_move, false)?;
        if !changed { return Ok(false); };

        shorten_expr(b, y, op2.clone(), can_move2, true)?;
    } else {
        let changed1 = shorten_expr(a, x, op1.clone(), can_move, false)?;
        let changed2 = shorten_expr(b, y, op2, can_move2, changed1)?;

        if !(changed1 || changed2) {
            return Ok(false);
        } else if !changed1 {
            *a = make_opr_from_ref(a, x, op1, can_move);
        }
    };

    let (mut a, mut b) = (take(a), take(b));
    let final_op = Op(main_op_info.0, final_op_incr);

    if (!left.op.1 && !main_op_info.1) && right.op.1 {
        if shorten_expr(&mut b, &mut a, final_op.clone(), true, false)? {
            *left = get_expr_opr_by_op(b, final_op, false);
        } else {
            (left.left, left.right) = (b, a);
            left.op = final_op.unwrap_op();
        }
    } else {
        (left.left, left.right) = (a, b);
        left.op = final_op.unwrap_op();
    }
    Ok(true)
}

fn insert_reduced_pow_value(source: &mut ExprTree, val: ExprTree, new_val: &ExprTree,
                            orig_pow: ExprTree, pow: ExprTree) {
    let multiplier1 = ExprTree::make_opr(new_val.clone(), pow, POW);
    let multiplier2 = ExprTree::make_opr(val, orig_pow, POW);

    *source = ExprTree::make_opr(multiplier1, multiplier2, MUL);
}

fn recover(source: &mut ExprTree, mut oper: ExprOper, l: ExprTree, can_recover: bool) {
    if can_recover {
        oper.left = l;
        *source = oper.as_expr_tree();
    }
}

fn common_operation(a: &mut ExprTree, b: &mut ExprTree, op: ExprUnit,
                    x: &mut ExprTree, y: &mut ExprTree) -> ExprResult<CommonFactor> {
    let factor1 = common_factor(a, x, (Tier1, true))?.unwrap_or((ONE, MUL)).0;
    let factor2 = common_factor(b, y, (Tier1, true))?.unwrap_or((ONE, MUL)).0;

    Ok(
        if factor1 != ONE {
            Some((ExprTree::make_opr(factor1, factor2, op), MUL))
        } else if factor2 != ONE {
            Some((factor2, op))
        } else { None }
    )
}

fn common_exp(left: &mut ExprOper, right: &mut ExprOper, op: (Tier, bool), result: &mut CommonFactor) -> ExprResult<()> {
    if op.0 == Tier2 {
        if let Some(mut cf) = common_factor(&mut left.right, &mut right.right, (Tier1, true))? {
            cf.1 = POW;
            *result = Some(cf);
        }
    }

    Ok(())
}

pub fn common_factor(orig_left: &mut ExprTree, orig_right: &mut ExprTree, op: (Tier, bool)) -> ExprResult<CommonFactor> {
    let mut left = get_expr_opr_by_op(take(orig_left), POW, true);
    let mut right = get_expr_opr_by_op(take(orig_right), POW, true);
    let both_are_default = left.is_default && right.is_default;

    let result = match (take(&mut left.left), take(&mut right.left)) {
        (x, y) if x == ONE || y == ONE => {
            left.left = x;
            right.left = y;
            None
        }
        (mut x, mut y) if !both_are_default || op.0 == Tier2 => {
            let mut result = None;
            let (mut recover_left, mut recover_right) = (true, true);
            let (p1, p2) = (&mut left.right, &mut right.right);

            if (p1.is_num() && p2.is_num()) || op == (Tier2, false) {
                if let Some(mut cf) = common_factor(&mut x, &mut y, (Tier1, true))? {
                    let cases = if let (Leaf(Num(n1)), Leaf(Num(n2))) = (&p1, &p2) {
                        (n1 < n2, n1 > n2)
                    } else {
                        (false, true)
                    };

                    if op == (Tier2, true) {
                        let mut cf_oper = get_expr_opr_by_op(cf.0, POW, true);
                        cf_oper.right = ExprTree::make_opr(p1.clone(), p2.clone(), ADD);
                        cf_oper.is_default = false;
                        cf.0 = cf_oper.as_expr_tree();
                    } else if cases.0 {
                        let pow = ExprTree::make_opr(p2.clone(), p1.clone(), SUB);
                        insert_reduced_pow_value(orig_right, take(&mut y), &cf.0, take(p2), pow);
                        recover_right = false;
                    } else if cases.1 {
                        let pow = ExprTree::make_opr(p1.clone(), p2.clone(), SUB);
                        insert_reduced_pow_value(orig_left, take(&mut x), &cf.0, take(p1), pow);
                        recover_left = false;
                    }

                    if op == (Tier2, false) {
                        cf.0 = ONE;
                    }

                    result = Some(cf);
                } else {
                    common_exp(&mut left, &mut right, op, &mut result)?
                }
            } else {
                common_exp(&mut left, &mut right, op, &mut result)?
            }

            recover(orig_left, left, x, recover_left);
            recover(orig_right, right, y, recover_right);

            return Ok(result);
        }
        (x, y) if x == y => {
            left.left = ONE;
            right.left = ONE;

            Some((x, MUL))
        }
        (mut x, mut y) if seek_multiplier(&y, &x) => {
            shorten_expr(&mut y, &mut x, DIV, false, false)?;
            left.left = ONE;
            right.left = y;

            Some((x, MUL))
        }
        (Operation(mut w, mut x, DIV), Operation(mut y, mut z, DIV)) => {
            let result = common_operation(&mut w, &mut x, DIV, &mut y, &mut z)?;

            left.left = Operation(w, x, DIV);
            right.left = Operation(y, z, DIV);

            result
        }
        (Operation(mut x, mut y, o), mut z) => {
            let factor1 = common_factor(&mut x, &mut z, op)?.unwrap_or((ONE, MUL)).0;
            let factor2 = if o == MUL {
                common_factor(&mut y, &mut z, op)?.unwrap_or((ONE, MUL)).0
            } else { ONE };

            left.left = Operation(x, y, o);
            right.left = z;

            if factor1 != ONE || factor2 != ONE {
                Some((ExprTree::make_opr(factor1, factor2, MUL), MUL))
            } else { None }
        }
        (x, y) => {
            left.left = x;
            right.left = y;
            None
        }
    };

    *orig_left = left.as_expr_tree();
    *orig_right = right.as_expr_tree();

    Ok(result)
}

fn get_next_op(op: ExprUnit, always_true: bool) -> ExprUnit {
    match op {
        Op(Tier3, i) => Op(Tier3, i || always_true),
        Op(t, i) => Op(t.plus_one(), i || always_true),
        _ => unreachable!()
    }
}

fn get_expr_opr_by_op(expr_tree: ExprTree, target_op: ExprUnit, strict: bool) -> ExprOper {
    match expr_tree {
        Operation(c1, c2, op)
        if (!strict || op == target_op) && op != E => ExprOper::new(*c1, *c2, op.unwrap_op(), false),
        _ => {
            if let Op(Tier1, _) = target_op {
                ExprOper::new(expr_tree, ZERO, target_op.unwrap_op(), true)
            } else { ExprOper::new(expr_tree, ONE, target_op.unwrap_op(), true) }
        }
    }
}

pub fn seek_multiplier(expr: &ExprTree, target: &ExprTree) -> bool {
    match expr {
        Operation(c1, c2, MUL) => {
            seek_multiplier(c1, target) || seek_multiplier(c2, target)
        }
        Operation(c1, _, POW) => seek_multiplier(c1, target),
        Operation(_, _, E) if target.is_num() => true,
        Leaf(Num(_)) if target.is_num() => true,
        expr if expr == target => true,
        _ => false
    }
}

pub fn seek_divisor(expr: &ExprTree, target: &ExprTree) -> bool {
    if let Operation(_, c2, DIV) = expr {
        seek_multiplier(c2, target)
    } else { false }
}

fn shorten_expr_early(left: &mut ExprTree, right: &mut ExprTree, op: ExprUnit,
                      can_move: bool, ensure_change: bool) -> Option<bool> {
    match (&left, &right) {
        (Leaf(Num(n1)), Leaf(Num(n2))) => {
            *left = operate_nums(*n1, *n2, op.unwrap_op());
            Some(true)
        }
        (Leaf(Unk(_)), Leaf(Num(_)) | Operation(_, _, E)) |
        (Leaf(Num(_)) | Operation(_, _, E), Leaf(Unk(_))) => {
            if ensure_change {
                *left = make_opr_from_ref(left, right, op, can_move);
            }
            Some(false)
        }
        (Operation(_, _, E), Leaf(_) | Operation(_, _, E)) |
        (Leaf(_), Operation(_, _, E)) => {
            *left = ExprTree::clean(take(left), take_or_clone(right, can_move), op).unwrap();
            Some(true)
        }
        (l, r) if l == r && op.unwrap_op().0 == Tier2 => {
            let new_op = POW;

            let mut left_opr = get_expr_opr_by_op(take(left), new_op.clone(), true);
            left_opr.is_default = false;
            *left = left_opr.as_expr_tree();

            let mut right_opr = get_expr_opr_by_op(take(right), new_op, true);
            right_opr.is_default = false;
            *right = right_opr.as_expr_tree();

            None
        }
        _ => None
    }
}

pub fn shorten_expr(orig_left: &mut ExprTree, orig_right: &mut ExprTree,
                    op: ExprUnit, can_move: bool, ensure_change: bool) -> ExprResult<bool> {
    if let Some(changed) = shorten_expr_early(orig_left, orig_right, op.clone(), can_move, ensure_change) {
        return Ok(changed);
    }

    let mut left = if let Some(op) = orig_right.get_op() {
        get_expr_opr_by_op(take(orig_left), op, false)
    } else {
        let next_op = get_next_op(op.clone(), true);
        get_expr_opr_by_op(take(orig_left), next_op, false)
    };

    let tuple_op = op.clone().unwrap_op();
    let mut right = ExprOper::new(ZERO, ZERO, (Tier1, true), true);
    let mut recover_right = false;

    let changed = match tuple_op.0.get_num() - left.op.0.get_num() {
        1 => {
            let will_remove = if op == DIV {
                seek_multiplier(&left.left, orig_right) || seek_multiplier(&left.right, orig_right)
            } else if op == MUL {
                if left.is_default {
                    *orig_left = take_or_clone(orig_right, can_move);
                    *orig_right = left.as_expr_tree();

                    return shorten_expr(orig_left, orig_right, op, true, ensure_change);
                } else {
                    seek_divisor(&left.left, orig_right) || seek_divisor(&left.right, orig_right)
                }
            } else { true };

            if will_remove && !left.is_default {
                distributive(&mut left, orig_right, op.clone(), can_move)?
            } else { false }
        }
        0 => {
            if let Tier3 = left.op.0 {
                false
            } else {
                let left_op = tuple_to_op(left.op);
                let right_copy = take_or_clone(orig_right, can_move);
                right = get_expr_opr_by_op(right_copy, left_op, true);
                recover_right = true;

                let changed = match_equivalent(&mut left, &mut right, op.clone(), false, can_move)?;

                if !changed {
                    match_equivalent(&mut left, &mut right, op.clone(), true, can_move)?
                } else { changed }
            }
        }
        _ => {
            let mut left_copy = left.as_expr_tree();
            let mut right_copy = take_or_clone(orig_right, can_move);

            let changed = if let Some(cf) = common_factor(&mut left_copy, &mut right_copy, tuple_op)? {
                left_copy = left_copy.propagate(ExprTree::clean, |x| Ok(x))?;
                right_copy = right_copy.propagate(ExprTree::clean, |x| Ok(x))?;

                shorten_expr(&mut left_copy, &mut right_copy, op.clone(), true, true)?;
                left_copy = ExprTree::make_opr(left_copy, cf.0, cf.1);

                true
            } else {
                *orig_right = right_copy;
                false
            };

            left = get_expr_opr_by_op(left_copy, MUL, false);
            changed
        }
    };

    left.is_default = left.is_default && !changed;
    let left = left.as_expr_tree();

    if recover_right && can_move {
        *orig_right = right.as_expr_tree();
    }

    *orig_left = if !changed && ensure_change {
        ExprTree::make_opr(left, take_or_clone(orig_right, can_move), op)
    } else {
        left
    }.propagate(ExprTree::clean, |x| Ok(x))?;

    Ok(changed)
}
