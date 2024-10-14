use super::*;
use std::fmt::{self, Debug, Display, Formatter};

use expression::error::ExprError;

pub type NewDimenRes<Dimen> = Result<Dimen, DimenError>;
pub type ModDimenRes<Dimen> = Result<Dimen, DimenError>;
pub type OperRes = Result<GeneDimen, DimenError>;

pub type OwnerName = &'static str;
pub type OwnerBase = String;
pub type OwnerBaseOption = Option<String>;
pub type Unit = String;
pub type Operator = Option<String>;
pub type Operation = &'static str;
pub type Attribute = &'static str;
pub type Expression = String;

#[derive(Clone, PartialEq)]
pub enum DimenError {
    UnitError(OwnerName, OwnerBaseOption, Unit),
    DiffLenError(OwnerName, Attribute, Attribute),
    InvalidValError(OwnerName),
    OperatorError(Operator),
    OperationError(OwnerName, OwnerBase, Operation, OwnerName, OwnerBase),
    SyntaxError(Expression),
    MathError(Expression)
}

impl Display for DimenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnitError(name, base, unit) => {
                if let Some(owner_base) = base {
                    write!(
                        f,
                        "UnitError: `{}` with base `{}` does not contain the `{}` unit.",
                        name, owner_base, unit
                    )
                } else {
                    write!(f, "UnitError: Any `{}` contains the `{}` unit.", name, unit)
                }
            }
            DiffLenError(name, attribute1, attribute2) => {
                write!(
                    f,
                    "DiffLenError: The `{}` attributes `{}` and `{}` must have the same length.",
                    name, attribute1, attribute2
                )
            }
            InvalidValError(name) => {
                write!(
                    f,
                    "InvalidValError: The provided value to `{}::from` must include \
                both a numerical value and a unit in order.",
                    name
                )
            }
            OperatorError(op) => {
                if let Some(op) = op {
                    write!(
                        f,
                        "OperatorError: No implementations to `{}` operator for `CompDimen`. \
                    Try one of: (*, /, ^)",
                        op
                    )
                } else {
                    write!(f, "OperatorError: `CompDimen` can not be built without an operator. Try `SimpDimen`.")
                }
            }
            OperationError(name1, base1, operation, name2, base2) => {
                write!(
                    f,
                    "OperationError: `{}` with base `{}` can not apply the `{}` operation for `{}` with base `{}`.",
                    name1, base1, operation, name2, base2
                )
            }
            SyntaxError(expression) => {
                write!(
                    f,
                    "SyntaxError: The syntax of `{}` expression is invalid.",
                    expression
                )
            }
            MathError(expression) => {
                write!(
                    f,
                    "MathError: The `{}` expression has a math error.",
                    expression
                )
            }
        }
    }
}

impl Debug for DimenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<ExprError> for DimenError {
    fn from(err: ExprError) -> DimenError {
        match err {
            ExprError::SyntaxError(expression) => SyntaxError(expression),
            ExprError::MathError(expression) => MathError(expression)
        }
    }
}
