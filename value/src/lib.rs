use crate::Value::{Number, Unknown};
use dimension::*;
use std::fmt::{self, Debug, Display};
use std::ops::*;

#[derive(Clone)]
pub enum Value {
    Number(GeneDimen),
    Unknown(String),
}

impl Value {
    pub fn is_number(&self) -> bool {
        match self {
            Number(_) => true,
            Unknown(_) => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Number(_) => false,
            Unknown(_) => true,
        }
    }

    pub fn unwrap(&self) -> GeneDimen {
        match self {
            Number(x) => x.clone(),
            _ => {
                panic!("Called `Value::unwrap()` on an `Unknown` value.")
            }
        }
    }

    pub fn unwrap_unk(&self) -> String {
        match self {
            Unknown(x) => x.clone(),
            _ => {
                panic!("Called `Value::unwrap_unk()` on a `Number` value.")
            }
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Number(GeneDimen::from(SimpDimen::init_nd(value)))
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Number(GeneDimen::from(SimpDimen::init_nd(value as f64)))
    }
}

impl From<SimpDimen> for Value {
    fn from(value: SimpDimen) -> Self {
        Number(GeneDimen::from(value))
    }
}

impl From<CompDimen> for Value {
    fn from(value: CompDimen) -> Self {
        Number(GeneDimen::from(value))
    }
}

impl From<GeneDimen> for Value {
    fn from(value: GeneDimen) -> Self {
        Number(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Unknown(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Unknown(value.to_string())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number(x) => {
                write!(f, "Number({})", x)
            }
            Unknown(x) => {
                write!(f, "Unknown({})", x.split(':').collect::<Vec<&str>>()[0])
            }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number(x) => {
                write!(f, "Number({})", x.unwrap_to_debug())
            }
            Unknown(x) => {
                write!(f, "Unknown({})", x)
            }
        }
    }
}

impl Add<Value> for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Number(x), Number(y)) => Number(x + y),
            (Number(x), Unknown(y)) => Unknown(y + &format!(":p{}", x.unwrap_to_debug())),
            (Unknown(x), Number(y)) => Unknown(x + &format!(":p{}", y.unwrap_to_debug())),
            _ => {
                panic!("Tried add `Unknown` to `Unknown`.")
            }
        }
    }
}

impl Add<GeneDimen> for Value {
    type Output = Value;

    fn add(self, other: GeneDimen) -> Value {
        match self {
            Number(x) => Number(x + other),
            Unknown(x) => Unknown(x + &format!(":p{}", other.unwrap_to_debug())),
        }
    }
}

impl Add<f64> for Value {
    type Output = Value;

    fn add(self, other: f64) -> Value {
        match self {
            Number(x) => Number(x + other),
            Unknown(x) => Unknown(x + &format!(":p{}ND", other)),
        }
    }
}

impl Add<i32> for Value {
    type Output = Value;

    fn add(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => Number(x + other),
            Unknown(x) => Unknown(x + &format!(":p{}ND", other)),
        }
    }
}

impl Sub<Value> for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self.clone(), other.clone()) {
            (Number(x), Number(y)) => Number(x - y),
            (Number(x), Unknown(_)) => -other + x,
            (Unknown(x), Number(y)) => Unknown(x + &format!(":s{}", y.unwrap_to_debug())),
            _ => {
                panic!("Tried subtract `Unknown` from `Unknown`.")
            }
        }
    }
}

impl Sub<GeneDimen> for Value {
    type Output = Value;

    fn sub(self, other: GeneDimen) -> Value {
        match self {
            Number(x) => Number(x - other),
            Unknown(x) => Unknown(x + &format!(":s{}", other.unwrap_to_debug())),
        }
    }
}

impl Sub<f64> for Value {
    type Output = Value;

    fn sub(self, other: f64) -> Value {
        match self {
            Number(x) => Number(x - other),
            Unknown(x) => Unknown(x + &format!(":s{}ND", other)),
        }
    }
}

impl Sub<i32> for Value {
    type Output = Value;

    fn sub(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => Number(x - other),
            Unknown(x) => Unknown(x + &format!(":s{}ND", other)),
        }
    }
}

impl Mul<Value> for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Number(x), Number(y)) => Number(x * y),
            (Number(x), Unknown(y)) => Unknown(y + &format!(":m{}", x.unwrap_to_debug())),
            (Unknown(x), Number(y)) => Unknown(x + &format!(":m{}", y.unwrap_to_debug())),
            _ => {
                panic!("Tried multiply `Unknown` by `Unknown`.")
            }
        }
    }
}

impl Mul<GeneDimen> for Value {
    type Output = Value;

    fn mul(self, other: GeneDimen) -> Value {
        match self {
            Number(x) => Number(x * other),
            Unknown(x) => Unknown(x + &format!(":m{}", other.unwrap_to_debug())),
        }
    }
}

impl Mul<f64> for Value {
    type Output = Value;

    fn mul(self, other: f64) -> Value {
        match self {
            Number(x) => Number(x * other),
            Unknown(x) => Unknown(x + &format!(":m{}ND", other)),
        }
    }
}

impl Mul<i32> for Value {
    type Output = Value;

    fn mul(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => Number(x * other),
            Unknown(x) => Unknown(x + &format!(":m{}ND", other)),
        }
    }
}

impl Div<Value> for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other.clone()) {
            (Number(x), Number(y)) => Number(x / y),
            (Number(x), Unknown(_y)) => (other / x).powf(-1.0),
            (Unknown(x), Number(y)) => Unknown(x + &format!(":d{}", y.unwrap_to_debug())),
            _ => {
                panic!("Tried divide `Unknown` by `Unknown`.")
            }
        }
    }
}

impl Div<GeneDimen> for Value {
    type Output = Value;

    fn div(self, other: GeneDimen) -> Value {
        match self {
            Number(x) => Number(x / other),
            Unknown(x) => Unknown(x + &format!(":d{}", other.unwrap_to_debug())),
        }
    }
}

impl Div<f64> for Value {
    type Output = Value;

    fn div(self, other: f64) -> Value {
        match self {
            Number(x) => Number(x / other),
            Unknown(x) => Unknown(x + &format!(":d{}ND", other)),
        }
    }
}

impl Div<i32> for Value {
    type Output = Value;

    fn div(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => Number(x / other),
            Unknown(x) => Unknown(x + &format!(":d{}ND", other)),
        }
    }
}

impl Pow<Value> for Value {
    type Output = Value;

    fn powf(self, other: Value) -> Value {
        match (self, other) {
            (Number(x), Number(y)) => Number(x.powf(y.get_value())),
            (Unknown(x), Number(y)) => {
                Unknown(x.to_owned() + &format!(":pw{}", y.unwrap_to_debug()))
            }
            _ => {
                panic!("`exp` parameter received an `Unknown` value at `Value::powv`.")
            }
        }
    }
}

impl Pow<GeneDimen> for Value {
    type Output = Value;

    fn powf(self, other: GeneDimen) -> Value {
        match self {
            Number(x) => Number(x.powf(other.get_value())),
            Unknown(x) => Unknown(x.to_owned() + &format!(":pw{}", other.unwrap_to_debug())),
        }
    }
}

impl Pow<f64> for Value {
    type Output = Value;

    fn powf(self, other: f64) -> Value {
        match self {
            Number(x) => Number(x.powf(other)),
            Unknown(x) => Unknown(x.to_owned() + &format!(":pw{}ND", other)),
        }
    }
}

impl Pow<i32> for Value {
    type Output = Value;

    fn powf(self, other: i32) -> Value {
        let other = other as f64;
        self.powf(other)
    }
}

impl AddAssign<Value> for Value {
    fn add_assign(&mut self, other: Value) {
        *self = self.clone().add(other)
    }
}

impl AddAssign<GeneDimen> for Value {
    fn add_assign(&mut self, other: GeneDimen) {
        *self = self.clone().add(other)
    }
}

impl AddAssign<f64> for Value {
    fn add_assign(&mut self, other: f64) {
        *self = self.clone().add(other)
    }
}

impl AddAssign<i32> for Value {
    fn add_assign(&mut self, other: i32) {
        *self = self.clone().add(other)
    }
}

impl SubAssign<Value> for Value {
    fn sub_assign(&mut self, other: Value) {
        *self = self.clone().sub(other)
    }
}

impl SubAssign<GeneDimen> for Value {
    fn sub_assign(&mut self, other: GeneDimen) {
        *self = self.clone().sub(other)
    }
}

impl SubAssign<f64> for Value {
    fn sub_assign(&mut self, other: f64) {
        *self = self.clone().sub(other)
    }
}

impl SubAssign<i32> for Value {
    fn sub_assign(&mut self, other: i32) {
        *self = self.clone().sub(other)
    }
}

impl MulAssign<Value> for Value {
    fn mul_assign(&mut self, other: Value) {
        *self = self.clone().mul(other)
    }
}

impl MulAssign<GeneDimen> for Value {
    fn mul_assign(&mut self, other: GeneDimen) {
        *self = self.clone().mul(other)
    }
}

impl MulAssign<f64> for Value {
    fn mul_assign(&mut self, other: f64) {
        *self = self.clone().mul(other)
    }
}

impl MulAssign<i32> for Value {
    fn mul_assign(&mut self, other: i32) {
        *self = self.clone().mul(other)
    }
}

impl DivAssign<Value> for Value {
    fn div_assign(&mut self, other: Value) {
        *self = self.clone().div(other)
    }
}

impl DivAssign<GeneDimen> for Value {
    fn div_assign(&mut self, other: GeneDimen) {
        *self = self.clone().div(other)
    }
}

impl DivAssign<f64> for Value {
    fn div_assign(&mut self, other: f64) {
        *self = self.clone().div(other)
    }
}

impl DivAssign<i32> for Value {
    fn div_assign(&mut self, other: i32) {
        *self = self.clone().div(other)
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Number(x) => Number(-x),
            Unknown(x) => Unknown(x + &format!(":m{}ND", -1)),
        }
    }
}
