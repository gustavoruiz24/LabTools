macro_rules! impl_ops_d_for_d {
    ($($d1:ident, $d2:ident);+) => {
        $(
            impl Add<$d2> for $d1 {
                type Output = GeneDimen;

                fn add(mut self, other: $d2) -> Self::Output {
                    if other.is_nd() {
                        self.set_value(self.get_value() + other.get_value());
                        self.to_generic()
                    }
                    else if self.is_nd() {
                        let mut other = other;
                        other.set_value(self.get_value() + other.get_value());
                        other.to_generic()
                    }
                    else {
                        let other_result = other.to_unit(self.get_unit());
                        if let Ok(other) = other_result {
                            self.set_value(self.get_value() + other.get_value());
                            self.to_generic()
                        }
                        else {
                            panic!("{}",
                                OperationError(
                                Self::get_name(),
                                self.get_base_to_display(),
                                "add",
                                $d2::get_name(),
                                other.get_base_to_display())
                            )
                        }
                    }
                }
            }

            impl Sub<$d2> for $d1 {
                type Output = GeneDimen;

                fn sub(mut self, other: $d2) -> Self::Output {
                    if other.is_nd() {
                        self.set_value(self.get_value() - other.get_value());
                        self.to_generic()
                    }
                    else if self.is_nd() {
                        let mut other = other;
                        other.set_value(self.get_value() - other.get_value());
                        other.to_generic()
                    }
                    else {
                        let other_result = other.to_unit(self.get_unit());
                        if let Ok(other) = other_result {
                            self.set_value(self.get_value() - other.get_value());
                            self.to_generic()
                        }
                        else {
                            panic!("{}",
                                OperationError(
                                Self::get_name(),
                                self.get_base_to_display(),
                                "sub",
                                $d2::get_name(),
                                other.get_base_to_display())
                            )
                        }
                    }
                }
            }

            impl Mul<$d2> for $d1 {
                type Output = GeneDimen;

                fn mul(mut self, other: $d2) -> Self::Output {
                    let mut other = other;
                    if other.is_nd() {
                        self.set_value(self.get_value() * other.get_value());
                        self.to_generic()
                    }
                    else if self.is_nd() {
                        other.set_value(self.get_value() * other.get_value());
                        other.to_generic()
                    }
                    else {
                        let _ = other.bcm_unit(self.get_unit());
                        GeneDimen::init_from_operation(self.to_generic(),
                        '*',
                        other.to_generic())
                    }
                }
            }

            impl Div<$d2> for $d1 {
                type Output = GeneDimen;

                fn div(mut self, other: $d2) -> Self::Output {
                    if other.is_nd() {
                        self.set_value(self.get_value() / other.get_value());
                        self.to_generic()
                    }
                    else if self.is_nd() {
                        GeneDimen::init_from_operation(self.to_generic(),
                        '/',
                        other.to_generic())
                    }
                    else {
                        let other_result = other.to_unit(self.get_unit());
                        if let Ok(other) = other_result {
                            GeneDimen::from(
                                SimpDimen::init_nd( self.get_value() / other.get_value() )
                            )
                        }
                        else {
                            GeneDimen::init_from_operation(self.to_generic(),
                            '/',
                            other.to_generic())
                        }
                    }
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
                self.set_value(-self.get_value());
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

macro_rules! impl_partial_ord_for_ds {
    ($($d:ident)?) => {
        $(
        impl PartialOrd for $d {
            fn ge(&self, other: &Self) -> bool {
                let same_unit_other = other.to_unit(self.get_unit());
                if let Ok(new_other) = same_unit_other {
                    self.get_value() >= new_other.get_value()
                }
                else {
                    panic!("{}", OperationError(Self::get_name(), self.get_base_to_display(), "ge", Self::get_name(), other.get_base_to_display()))
                }
            }

            fn gt(&self, other: &Self) -> bool {
                let same_unit_other = other.to_unit(self.get_unit());
                if let Ok(new_other) = same_unit_other {
                    self.get_value() > new_other.get_value()
                }
                else {
                    panic!("{}", OperationError(Self::get_name(), self.get_base_to_display(), "gt", Self::get_name(), other.get_base_to_display()))
                }
            }

            fn le(&self, other: &Self) -> bool {
                let same_unit_other = other.to_unit(self.get_unit());
                if let Ok(new_other) = same_unit_other {
                    self.get_value() <= new_other.get_value()
                }
                else {
                    panic!("{}", OperationError(Self::get_name(), self.get_base_to_display(), "le", Self::get_name(), other.get_base_to_display()))
                }
            }

            fn lt(&self, other: &Self) -> bool {
                let same_unit_other = other.to_unit(self.get_unit());
                if let Ok(new_other) = same_unit_other {
                    self.get_value() < new_other.get_value()
                }
                else {
                    panic!("{}", OperationError(Self::get_name(), self.get_base_to_display(), "lt", Self::get_name(), other.get_base_to_display()))
                }
            }

            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                let same_unit_other = other.to_unit(self.get_unit());
                if let Ok(new_other) = same_unit_other {
                    self.get_value().partial_cmp(&new_other.get_value())
                }
                else {
                    panic!("{}", OperationError(Self::get_name(), self.get_base_to_display(), "partial_cmp", Self::get_name(), other.get_base_to_display()))
                }
            }
        }
        )*
    };
}

macro_rules! pass_ds_to_impl_ops {
    ($($d:ident),+) => {
        $(
            impl_ops_d_for_d!($d, SimpDimen; $d, CompDimen; $d, GeneDimen);
            impl_ops_num_for_d!(Add, add, $d; Sub, sub, $d; Mul, mul, $d; Div, div, $d);
            impl_neg_for_d!($d);
            impl_ops_asn_num_for_d!(AddAssign, add_assign, +, $d; SubAssign, sub_assign, -, $d; MulAssign, mul_assign, *, $d; DivAssign, div_assign, /, $d);
            impl_partial_ord_for_ds!($d);
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
    ($($s: expr, $o: expr, $t1: ty, $t2: tt, $op:ident, $op_name:expr)?) => {
        $(
            {
                let unit = $s.get_unit();
                let other_unit = $o.get_unit();
                if unit == other_unit {
                    Ok($t2 {
                        value: $s.value.$op($o.get_value()),
                        ..$s.clone()
                    })
                } else {
                    Err(VerifiedOpError(
                        <$t1>::get_name(),
                        unit,
                        $op_name,
                        <$t1>::get_name(),
                        other_unit,
                    ))
                }
            }
        )*
    };
}

macro_rules! apply_verified_ops_gd {
    ($($s: expr, $o: expr, $op:ident, $op_name:expr)?) => {
        $(
            match ($s, &$o) {
                (GenSimpDimen(d1), GenSimpDimen(d2)) => Ok(GeneDimen::from(d1.$op(d2)?)),
                (GenCompDimen(dr1), GenCompDimen(dr2)) => Ok(GeneDimen::from(dr1.$op(dr2)?)),
                (GenSimpDimen(_), GenCompDimen(_)) => Err(VerifiedOpError(
                    "GeneDimen::GenSimpDimen",
                    $s.get_unit(),
                    $op_name,
                    "GeneDimen::GenCompDimen",
                    $o.get_unit(),
                )),
                (GenCompDimen(_), GenSimpDimen(_)) => Err(VerifiedOpError(
                    "GeneDimen::GenCompDimen",
                    $s.get_unit(),
                    $op_name,
                    "GeneDimen::GenSimpDimen",
                    $o.get_unit(),
                )),
            }
        )*
    };
}

pub(crate) use impl_ops_d_for_d;
pub(crate) use impl_ops_num_for_d;
pub(crate) use impl_neg_for_d;
pub(crate) use impl_ops_asn_num_for_d;
pub(crate) use impl_partial_ord_for_ds;
pub(crate) use pass_ds_to_impl_ops;
pub(crate) use impl_ops_asn_gd_for_gd;
pub(crate) use impl_exp_for_ds;
pub(crate) use impl_from_str_string_for_ds;
pub(crate) use apply_verified_ops;
pub(crate) use apply_verified_ops_gd;