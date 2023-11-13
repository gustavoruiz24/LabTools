use std::f64::consts::PI;
use dimension::{Dimension, DimensionBase::*};
use value::Value::{self, Number, Unknown};
use unk_tools::{solve_unknown, rule_of_three_d, rule_of_three};


fn main() {
    // let dimensions = [Length, Time, Mass, ElCurrent, Temperature, AmOfSubstance, LuminousIn, Angle, ND];
    //
    // for d in dimensions {
    //     println!("{:=^50}", "=");
    //     println!("{}:", d);
    //     Dimension::units_vs_si(d);
    // }
    // println!("{:=^50}", "=");
    // println!("Temos 1 quilometro");
    // let mut quilometro = Dimension::init(1.0, "km", Length).unwrap();
    // println!("{}\n", quilometro);
    //
    // println!("Temos 1 metro");
    // let mut metro = Dimension::init(1.0, "m", Length).unwrap();
    // println!("{}\n", metro);
    //
    // println!("Agora temos 2 quilometros e 2 metros");
    // quilometro += 1;
    // metro += 1;
    // println!("{}", quilometro);
    // println!("{}\n", metro);
    //
    // println!("Podemos ver ambos como centimetros");
    // println!("{}", quilometro.as_unit("cm").unwrap());
    // println!("{}\n", metro.as_unit("cm").unwrap());
    //
    // println!("Ou transforma-los em milimetro e metro, respectivamente");
    // metro.to_unit("mm");
    // quilometro.to_unit("m");
    // let milimetro = metro;
    // let metro = quilometro;
    // println!("{}", metro);
    // println!("{}\n", milimetro);
    //
    // println!("Agora vamos descobrir quantas veze 1 metro é maior que 1 milimetro");
    // let milimetros_vezes_x = Value::from("x") * milimetro.clone();
    // let valor_esperado = Value::from(metro.clone());
    // let resultado = solve_unknown(milimetros_vezes_x, valor_esperado);
    // println!("{}\n", resultado.unwrap());
    //
    // println!("Desta vez usando regra de 3");
    // let resultado = rule_of_three_d((Dimension::init(1.0, "mm", Length).unwrap(), Value::from("x")), (milimetro.clone(), metro.clone()));
    // println!("{}\n", resultado.unwrap());
    //
    // println!("Tambem podemos somar 1 milimetro com 1 metro");
    // println!("{}\n", milimetro.clone() + metro.clone());
    //
    // println!("Colocando na unidade do sistema internacional");
    // println!("{}", (milimetro + metro).as_si());
}
