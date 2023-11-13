use crate::Value::{Number, Unknown};
use std::ops::*;
use dimension::Dimension;
use std::fmt::{self, Display, Debug};

#[derive(Clone)]
pub enum Value {
    Number(Dimension),
    Unknown(String)
}

impl Value {
    pub fn is_number(&self) -> bool {
        match self {
            Number(_) => { true },
            Unknown(_) => { false }
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Number(_) => { false },
            Unknown(_) => { true }
        }
    }

    pub fn unwrap(&self) -> Dimension {
        match self {
            Number(x) => { x.clone() },
            _ => { panic!("Called `Value::unwrap()` on an `Unknown` value.") }
        }
    }

    pub fn unwrap_unk(&self) -> String {
        match self {
            Unknown(x) => { x.clone() },
            _ => { panic!("Called `Value::unwrap_unk()` on a `Number` value.") }
        }
    }

    pub fn powv(&self, exp: Value) -> Value {
        match (self, exp) {
            (Number(x), Number(y)) => { Number(x.pow(y.get_value())) },
            (Unknown(x), Number(y)) => { Unknown(x.to_owned() + &format!(":pw{:?}", y)) },
            _ => { panic!("`exp` parameter received an `Unknown` value at `Value::powv`.") }
        }
    }

    pub fn powd(&self, exp: Dimension) -> Value {
        match self {
            Number(x) => { Number(x.pow(exp.get_value())) },
            Unknown(x) => { Unknown(x.to_owned() + &format!(":pw{:?}", exp)) }
        }
    }

    pub fn pow(&self, exp: f64) -> Value {
        match self {
            Number(x) => { Number(x.pow(exp)) },
            Unknown(x) => { Unknown(x.to_owned() + &format!(":pw{}ND", exp)) }
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Number(Dimension::init_nd(value))
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Number(Dimension::init_nd(value as f64))
    }
}

impl From<Dimension> for Value {
    fn from(value: Dimension) -> Self {
        Number(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Unknown(value)
    }
}

impl From<&'static str> for Value {
    fn from(value: &str) -> Self {
        Unknown(value.to_string())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number(x) => { write!(f, "Number({})", x )},
            Unknown(x) => { write!(f, "Unknown({})", x.split(":").collect::<Vec<&str>>()[0]) }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number(x) => { write!(f, "Number({})", x )},
            Unknown(x) => { write!(f, "Unknown({})", x) }
        }
    }
}

impl Add<Value> for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Number(x), Number(y)) => { Number(x + y) },
            (Number(x), Unknown(y)) => { Unknown(y + &format!(":p{:?}", x)) },
            (Unknown(x), Number(y)) => { { Unknown(x + &format!(":p{:?}", y)) } },
            _ => { panic!("Tried add `Unknown` to `Unknown`.") }
        }
    }
}

impl Add<Dimension> for Value {
    type Output = Value;

    fn add(self, other: Dimension) -> Value {
        match self {
            Number(x) => { Number(x + other) },
            Unknown(x) => { Unknown(x + &format!(":p{:?}", other)) }
        }
    }
}

impl Add<f64> for Value {
    type Output = Value;

    fn add(self, other: f64) -> Value {
        match self {
            Number(x) => { Number(x + other) },
            Unknown(x) => { Unknown(x + &format!(":p{}ND", other)) }
        }
    }
}

impl Add<i32> for Value {
    type Output = Value;

    fn add(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => { Number(x + other) },
            Unknown(x) => { Unknown(x + &format!(":p{}ND", other)) }
        }
    }
}

impl Sub<Value> for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self.clone(), other.clone()) {
            (Number(x), Number(y)) => { Number(x - y) },
            (Number(x), Unknown(_)) => { -other + x },
            (Unknown(x), Number(y)) => { Unknown(x + &format!(":s{:?}", y)) },
            _ => { panic!("Tried subtract `Unknown` from `Unknown`.") }
        }
    }
}

impl Sub<Dimension> for Value {
    type Output = Value;

    fn sub(self, other: Dimension) -> Value {
        match self {
            Number(x) => { Number(x - other) },
            Unknown(x) => { Unknown(x + &format!(":s{:?}", other)) }
        }
    }
}

impl Sub<f64> for Value {
    type Output = Value;

    fn sub(self, other: f64) -> Value {
        match self {
            Number(x) => { Number(x - other) },
            Unknown(x) => { Unknown(x + &format!(":s{}ND", other)) }
        }
    }
}

impl Sub<i32> for Value {
    type Output = Value;

    fn sub(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => { Number(x - other) },
            Unknown(x) => { Unknown(x + &format!(":s{}ND", other)) }
        }
    }
}

impl Mul<Value> for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Number(x), Number(y)) => { Number(x * y) },
            (Number(x), Unknown(y)) => { Unknown(y + &format!(":m{:?}", x)) },
            (Unknown(x), Number(y)) => { Unknown(x + &format!(":m{:?}", y)) },
            _ => { panic!("Tried multiply `Unknown` by `Unknown`.") }
        }
    }
}

impl Mul<Dimension> for Value {
    type Output = Value;

    fn mul(self, other: Dimension) -> Value {
        match self {
            Number(x) => { Number(x * other) },
            Unknown(x) => { Unknown(x + &format!(":m{:?}", other)) }
        }
    }
}

impl Mul<f64> for Value {
    type Output = Value;

    fn mul(self, other: f64) -> Value {
        match self {
            Number(x) => { Number(x * other) },
            Unknown(x) => { Unknown(x + &format!(":m{}ND", other)) }
        }
    }
}

impl Mul<i32> for Value {
    type Output = Value;

    fn mul(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => { Number(x * other) },
            Unknown(x) => { Unknown(x + &format!(":m{}ND", other)) }
        }
    }
}

impl Div<Value> for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other.clone()) {
            (Number(x), Number(y)) => { Number(x / y) },
            (Number(x), Unknown(_y)) => { (other / x).pow(-1.0) },
            (Unknown(x), Number(y)) => { Unknown(x + &format!(":d{:?}", y)) },
            _ => { panic!("Tried divide `Unknown` by `Unknown`.") }
        }
    }
}

impl Div<Dimension> for Value {
    type Output = Value;

    fn div(self, other: Dimension) -> Value {
        match self {
            Number(x) => { Number(x / other) },
            Unknown(x) => { Unknown(x + &format!(":d{:?}", other)) }
        }
    }
}

impl Div<f64> for Value {
    type Output = Value;

    fn div(self, other: f64) -> Value {
        match self {
            Number(x) => { Number(x / other) },
            Unknown(x) => { Unknown(x + &format!(":d{}ND", other)) }
        }
    }
}

impl Div<i32> for Value {
    type Output = Value;

    fn div(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => { Number(x / other) },
            Unknown(x) => { Unknown(x + &format!(":d{}ND", other)) }
        }
    }
}

impl Rem<Value> for Value {
    type Output = Value;

    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (Number(x), Number(y)) => { Number(x % y) },
            _ => { panic!("Tried calculate the remainder of `Value` divided by `Value::Unknown`.") }
        }
    }
}

impl Rem<Dimension> for Value {
    type Output = Value;

    fn rem(self, other: Dimension) -> Value {
        match self {
            Number(x) => { Number(x % other) },
            Unknown(_) => { panic!("Tried calculate the remainder of `Unknown` divided by `Dimension`.") }
        }
    }
}

impl Rem<f64> for Value {
    type Output = Value;

    fn rem(self, other: f64) -> Value {
        match self {
            Number(x) => { Number(x % other) },
            Unknown(_) => { panic!("Tried calculate the remainder of `Unknown` divided by `f64`.") }
        }
    }
}

impl Rem<i32> for Value {
    type Output = Value;

    fn rem(self, other: i32) -> Value {
        let other = other as f64;
        match self {
            Number(x) => { Number(x % other) },
            Unknown(_) => { panic!("Tried calculate the remainder of `Unknown` divided by `f64`.") }
        }
    }
}

impl AddAssign<Value> for Value {
    fn add_assign(&mut self, other: Value) {
        *self = self.clone().add(other)
    }
}

impl AddAssign<Dimension> for Value {
    fn add_assign(&mut self, other: Dimension) {
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

impl SubAssign<Dimension> for Value {
    fn sub_assign(&mut self, other: Dimension) {
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

impl MulAssign<Dimension> for Value {
    fn mul_assign(&mut self, other: Dimension) {
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

impl DivAssign<Dimension> for Value {
    fn div_assign(&mut self, other: Dimension) {
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

impl RemAssign<Value> for Value {
    fn rem_assign(&mut self, other: Value) {
        *self = self.clone().rem(other)
    }
}

impl RemAssign<Dimension> for Value {
    fn rem_assign(&mut self, other: Dimension) {
        *self = self.clone().rem(other)
    }
}

impl RemAssign<f64> for Value {
    fn rem_assign(&mut self, other: f64) {
        *self = self.clone().rem(other)
    }
}

impl RemAssign<i32> for Value {
    fn rem_assign(&mut self, other: i32) {
        *self = self.clone().rem(other)
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Number(x) => { Number(-x) },
            Unknown(x) => { Unknown(x + &format!(":m{}ND", -1)) }
        }
    }
}
