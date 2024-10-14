macro_rules! push_expr_leaf {
    ($eu:expr, $e:expr, $leaf:expr, $expr_str:expr) => {
        push_expr_leaf(&mut $eu, &mut $e, Some(Leaf($leaf)), $expr_str)?
    }
}

macro_rules! impl_ops {
    ($($t:ident, $tfn:ident, $op:expr);+) => {
        $(impl $t for ExprTree {
            type Output = Self;

            fn $tfn(self, other: Self) -> Self {
                apply_simplifications(self, other, $op).unwrap_or_else(|err| panic!("{}", err))
            }
        })*
    }
}

macro_rules! impl_ops_num {
    ($($t:ident, $tfn:ident, $op:expr);+) => {
        $(impl<T: Into<f64>> $t<T> for ExprTree {
            type Output = Self;

            fn $tfn(self, other: T) -> Self {
                let num = Leaf(Num(other.into()));
                apply_simplifications(self, num, $op).unwrap_or_else(|err| panic!("{}", err))
            }
        })*
    }
}

macro_rules! impl_ops_asn {
    ($($t:ident, $tfn:ident, $op:expr);+) => {
        $(impl $t for ExprTree {
            fn $tfn(&mut self, other: Self) {
                *self = apply_simplifications(take(self), other, $op).unwrap_or_else(|err| panic!("{}", err))
            }
        })*
    }
}

macro_rules! impl_ops_asn_num {
    ($($t:ident, $tfn:ident, $op:tt);+) => {
        $(impl<T: Into<f64>> $t<T> for ExprTree {
            fn $tfn(&mut self, other: T) {
                let num = Leaf(Num(other.into()));
                *self = apply_simplifications(take(self), num, $op).unwrap_or_else(|err| panic!("{}", err))
            }
        })*
    }
}

pub(crate) use push_expr_leaf;
pub(crate) use impl_ops;
pub(crate) use impl_ops_num;
pub(crate) use impl_ops_asn;
pub(crate) use impl_ops_asn_num;
