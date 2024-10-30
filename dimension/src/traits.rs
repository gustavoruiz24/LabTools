use expression::expr_structs::ExprTree;

use crate::error::{DimenError, OperRes};
use crate::error::ModDimenRes;
use crate::{CustomUnits, GeneDimen};

pub trait ParseErr<T> {
    fn parse_err(self) -> Result<T, DimenError>;
}

pub trait Pow<Rhs = Self> {
    fn pow(self, other: Rhs) -> GeneDimen;
}

pub trait PowD<Rhs = Self> {
    fn powd(self, other: Rhs) -> GeneDimen;
}

pub trait DimenBasics {
    fn is_nd(&self) -> bool;

    fn to_si(&self) -> Self
        where
            Self: Sized + Clone,
    {
        let mut d = self.clone();
        d.bcm_si();
        d
    }

    fn to_unit(&self, unit: &[&str]) -> ModDimenRes<Self>
        where
            Self: Sized + Clone,
    {
        let mut d = self.clone();
        d.bcm_unit(unit)?;
        Ok(d)
    }

    fn bcm_si(&mut self);

    fn bcm_unit(&mut self, unit: &[&str]) -> Result<(), DimenError>;

    fn to_generic(self) -> GeneDimen;

    fn verified_add(&mut self, other: GeneDimen) -> OperRes
        where
            Self: Sized;

    fn verified_sub(&mut self, other: GeneDimen) -> OperRes
        where
            Self: Sized;

    fn verified_mul(&mut self, other: GeneDimen) -> OperRes
        where
            Self: Sized;

    fn verified_div(&mut self, other: GeneDimen) -> OperRes
        where
            Self: Sized;

    fn verified_pow(&mut self, other: GeneDimen) -> OperRes
        where
            Self: Sized;
}

pub trait DimenSetAndGet {
    fn set_value(&mut self, other: ExprTree);

    fn get_value(&self) -> ExprTree;

    fn get_move_value(&mut self) -> ExprTree;

    fn get_unit(&self) -> ExprTree;

    fn get_move_unit(&mut self) -> ExprTree;

    fn get_num_or_unit(&self) -> ExprTree;

    fn get_move_num_or_unit(&mut self) -> ExprTree;

    fn get_custom_units(&self) -> CustomUnits;

    fn get_move_custom_units(&mut self) -> CustomUnits;

    fn get_base_to_display(&self) -> String;

    fn get_name() -> &'static str;
}
