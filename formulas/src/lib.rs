pub const WIEN: f32 = 0.0028977685;

pub fn wien_law(temperature: f32) -> f32 {
    WIEN / temperature
}
