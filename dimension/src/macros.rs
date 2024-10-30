macro_rules! impl_ops_d_for_d {
    ($($d1:ident, $d2:ident);+) => {
        $(
            impl Add<$d2> for $d1 {
                type Output = GeneDimen;

                fn add(mut self, other: $d2) -> Self::Output {
                    self.verified_add(other.to_generic()).map_err(|err| panic!("{}", err)).unwrap()
                }
            }

            impl Sub<$d2> for $d1 {
                type Output = GeneDimen;

                fn sub(mut self, other: $d2) -> Self::Output {
                    self.verified_sub(other.to_generic()).map_err(|err| panic!("{}", err)).unwrap()
                }
            }

            impl Mul<$d2> for $d1 {
                type Output = GeneDimen;

                fn mul(mut self, other: $d2) -> Self::Output {
                    self.verified_mul(other.to_generic()).map_err(|err| panic!("{}", err)).unwrap()
                }
            }

            impl Div<$d2> for $d1 {
                type Output = GeneDimen;

                fn div(mut self, other: $d2) -> Self::Output {
                    self.verified_div(other.to_generic()).map_err(|err| panic!("{}", err)).unwrap()
                }
            }
        )*
    }
}

macro_rules! impl_ops_num_for_d {
    ($($t:ident, $tfn:ident, $d: ident);+) => {
        $(impl<T: Into<f64>> $t<T> for $d {
            type Output = $d;

            fn $tfn(mut self, other: T) -> $d {
                self.set_value(self.get_value().$tfn(other.into()));
                self
            }
        })*
    }
}

macro_rules! impl_neg_for_d {
    ($($d: ident),+) => {
        $(impl Neg for $d {
            type Output = $d;

            fn neg(mut self) -> $d {
                let new_value = -take(&mut self.get_move_value());
                self.set_value(new_value);
                self
            }
        })*
    }
}

macro_rules! impl_ops_asn_num_for_d {
    ($($t:ident, $tfn:ident, $op:tt, $d:ident);+) => {
        $(impl<T: Into<f64>> $t<T> for $d {
            fn $tfn(&mut self, other: T) {
                self.set_value(self.get_value() $op other.into());
            }
        })*
    }
}

macro_rules! pass_ds_to_impl_ops {
    ($($d:ident),+) => {
        $(
            impl_ops_d_for_d!($d, SimpDimen; $d, CompDimen; $d, GeneDimen);
            impl_ops_num_for_d!(Add, add, $d; Sub, sub, $d; Mul, mul, $d; Div, div, $d);
            impl_neg_for_d!($d);
            impl_ops_asn_num_for_d!(AddAssign, add_assign, +, $d; SubAssign, sub_assign, -, $d; MulAssign, mul_assign, *, $d; DivAssign, div_assign, /, $d);
        )*
    };
}

macro_rules! impl_ops_asn_gd_for_gd {
    ($($t:ident, $tfn:ident, $op:tt);+) => {
        $(impl $t<GeneDimen> for GeneDimen {
            fn $tfn(&mut self, other: GeneDimen) {
                *self = (self.clone() $op other);
            }
        })*
    }
}

macro_rules! impl_exp_for_ds {
    ($($d:ident),+) => {
        $(
        impl UpperExp for $d {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "{:E}{}", self.get_value(), self.get_unit())
            }
        }

        impl LowerExp for $d {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "{:e}{}", self.get_value(), self.get_unit())
            }
        }
        )+
    };
}

macro_rules! impl_from_str_string_for_ds {
    ($($d: ident),+) => {
        $(impl From<&str> for $d {
            fn from(value: &str) -> Self {
                Self::soft_from(value).unwrap_or_else(|err| panic!("{}", err))
            }
        }

        impl From<String> for $d {
            fn from(value: String) -> Self {
                Self::soft_from(value).unwrap_or_else(|err| panic!("{}", err))
            }
        })*
    };
}

macro_rules! apply_verified_ops {
    ($($s: expr, $o: expr, $op:expr)?) => {
        $(
            {
                let tree = apply_simplifications($s.get_move_value(), $o.get_value(), $op);
                let value = tree.parse_err()?;
                let unit = get_unit_based_on_op(
                    &$op,
                    $s.get_move_unit(),
                    &mut ZERO,
                    $o.get_move_num_or_unit()
                )?;
                
                let mut custom_units = $s.get_move_custom_units();
                custom_units.append(&mut $o.get_move_custom_units());

                GeneDimen::init(value, unit, custom_units)
            }
        )*
    };
}

pub(crate) use impl_ops_d_for_d;
pub(crate) use impl_ops_num_for_d;
pub(crate) use impl_neg_for_d;
pub(crate) use impl_ops_asn_num_for_d;
pub(crate) use pass_ds_to_impl_ops;
pub(crate) use impl_ops_asn_gd_for_gd;
pub(crate) use impl_exp_for_ds;
pub(crate) use impl_from_str_string_for_ds;
pub(crate) use apply_verified_ops;
