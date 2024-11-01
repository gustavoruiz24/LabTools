use std::fmt::{Display, Debug};

use expression::error::ExprError;
use dimension::error::DimenError;

use self::EquationError::*;

pub enum EquationError {
    SyntaxError(String),
    MathError(String),
    OperationError(String)
}

impl Display for EquationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError(expr) => write!(f, "SyntaxError: The `{}` equation contains a syntax error.", expr),
            MathError(equa) => write!(f, "MathError: The `{}` equation has no solution.", equa),
            OperationError(err_msg) => write!(f, "{}", err_msg)
        }
    }
}

impl Debug for EquationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn parse_expr_err(equa: &str, err: ExprError) -> EquationError {
    match err {
        ExprError::SyntaxError(_) => SyntaxError(equa.to_string()),
        ExprError::MathError(_) => MathError(equa.to_string())
    }
}

pub fn parse_dimen_err(equa: &str, err: DimenError) -> EquationError {
    match &err {
        DimenError::SyntaxError(_) => SyntaxError(equa.to_string()),
        DimenError::MathError(_) => MathError(equa.to_string()),
        DimenError::OperationError(_, _, _, _, _) => OperationError(err.to_string()),
        _ => unreachable!()
    }
}
