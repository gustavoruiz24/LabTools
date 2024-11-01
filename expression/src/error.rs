use std::fmt::{Debug, Display};
use self::ExprError::*;

#[derive(PartialEq)]
pub enum ExprError {
    SyntaxError(String),
    MathError(String),
}

impl Display for ExprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError(expr) => write!(f, "SyntaxError: The `{}` expression contains a syntax error.", expr),
            MathError(expr) => write!(f, "MathError: The `{}` expression contains a math error.", expr)
        }
    }
}

impl Debug for ExprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

