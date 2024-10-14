pub trait Pow<Rhs = Self> {
    type Output;

    fn pow(self, other: Rhs) -> Self::Output;
}
