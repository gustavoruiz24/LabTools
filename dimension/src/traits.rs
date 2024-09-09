use crate::error::DimenError;
use crate::error::{NewDimenRes, ModDimenRes};
use crate::GeneDimen;

pub trait PowD<Rhs = Self> {
    type Output;

    fn powd(self, other: Rhs) -> Self::Output;
}

pub trait Pow<Rhs = Self> {
    type Output;

    fn pow(self, other: Rhs) -> Self::Output;
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

    fn to_unit<T: AsRef<str>>(&self, unit: T) -> ModDimenRes<Self>
        where
            Self: Sized + Clone,
    {
        let mut d = self.clone();
        d.bcm_unit(unit)?;
        Ok(d)
    }

    fn bcm_si(&mut self);

    fn bcm_unit<T: AsRef<str>>(&mut self, unit: T) -> Result<(), DimenError>;

    fn to_generic(self) -> GeneDimen;

    fn verified_add(&self, other: &Self) -> ModDimenRes<Self>
        where
            Self: Sized;

    fn verified_sub(&self, other: &Self) -> ModDimenRes<Self>
        where
            Self: Sized;

    fn verified_mul(&self, other: &Self) -> ModDimenRes<Self>
        where
            Self: Sized;

    fn verified_div(&self, other: &Self) -> ModDimenRes<Self>
        where
            Self: Sized;

    fn verified_pow(&self, other: &Self) -> ModDimenRes<Self>
        where
            Self: Sized;
}

pub trait DimenSetAndGet {
    fn set_value(&mut self, other: f64);

    fn get_value(&self) -> f64;

    fn get_unit(&self) -> String;

    fn get_base_to_display(&self) -> String;

    fn get_name() -> &'static str;
}

pub trait DimenBaseDependents<Base> {
    fn init(value: f64, unit: &str, base: Base) -> NewDimenRes<Self>
        where
            Self: Sized;

    fn get_base(&self) -> Base;
}