use super::*;
use std::fmt::{self, Debug, Display, Formatter};

pub type NewDimenRes<Dimen> = Result<Dimen, DimenError>;
pub type ModDimenRes<Dimen> = Result<Dimen, DimenError>;

pub type OwnerName = &'static str;
pub type OwnerBase = String;
pub type OwnerBaseOption = Option<String>;
pub type Unit = String;
pub type Operator = Option<String>;
pub type Operation = &'static str;
pub type Attribute = &'static str;

#[derive(Clone, PartialEq)]
pub enum DimenError {
    UnitError(OwnerName, OwnerBaseOption, Unit),
    DiffLenError(OwnerName, Attribute, Attribute),
    InvalidValError(OwnerName),
    OperatorError(Operator),
    OperationError(OwnerName, OwnerBase, Operation, OwnerName, OwnerBase),
    VerifiedOpError(OwnerName, Unit, Operation, OwnerName, Unit),
    BracketError(Unit),
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
            VerifiedOpError(owner_name1, unit1, operation, owner_name2, unit2) => {
                write!(
                    f,
                    "VerifiedOpError: `{}` with unit `{}` expected a `{}` with unit `{}` in the `verified_{}` operation \
                    but received `{}` with unit `{}`.",
                    owner_name1, unit1, owner_name1, unit1, operation, owner_name2, unit2
                )
            }
            BracketError(unit) => {
                write!(
                    f,
                    "BracketError: The brackets in `{}` must be balanced.",
                    unit
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
