use dimension::Dimension;
use value::Value;


pub fn solve_unknown(unk: Value, exp_res: Value) -> Result<Value, String> {
    if unk.is_number() { return Err("The `unk` parameter from `solve_unknown` just accepts `Value::Unknown`".to_string()) }
    let unk = unk.unwrap_unk();
    let ops: Vec<&str> = unk.split(":").collect::<Vec<&str>>()[1..].to_vec();
    let mut result = exp_res;

    for op in ops.iter().rev() {
        let info: (String, String, String);
        {
            let mut letters = op.split(|x: char| x.is_numeric() || x == '.' || x == '-' ).collect::<Vec<&str>>();
            letters.retain(|x| !x.is_empty());
            let number = op.replace(|x: char| !x.is_numeric() && x != '.' && x != '-', "");
            if !number.is_empty() && letters.len() == 2 {
                info = (letters[0].to_string(), number, letters[1].to_string())
            }
            else { return Err("Invalid values were passed to `unk` in `solve_unknown`.".to_string()) }
        }

        match (info.0.as_str(), info.1, info.2.as_str()) {
            ("p", v, "ND") => { result -= v.parse::<f64>().unwrap() },
            ("p", v, m) => { result -= Dimension::init(v.parse::<f64>().unwrap(), m, Dimension::get_unit_owner(m)?)? },
            ("s", v, "ND") => { result += v.parse::<f64>().unwrap() },
            ("s", v, m) => { result += Dimension::init(v.parse::<f64>().unwrap(), m, Dimension::get_unit_owner(m)?)? },
            ("m", v, "ND") => { result /= v.parse::<f64>().unwrap() },
            ("m", v, m) => { result /= Dimension::init(v.parse::<f64>().unwrap(), m, Dimension::get_unit_owner(m)?)? },
            ("d", v, "ND") => { result *= v.parse::<f64>().unwrap() },
            ("d", v, m) => { result *= Dimension::init(v.parse::<f64>().unwrap(), m, Dimension::get_unit_owner(m)?)? },
            ("pw", v, "ND") => { result = result.pow(1.0/v.parse::<f64>().unwrap()) },
            ("pw", v, m) => { result = result.powd(Dimension::init(v.parse::<f64>().unwrap(), m, Dimension::get_unit_owner(m)?)?.pow(-1.0)) },
            _ => { panic!("Some bug occurred!") }
        }
    }

    Ok(result)
}

pub fn rule_of_three_v(fst_rel: (Value, Value), snd_rel: (Value, Value)) -> Value {
    let right = fst_rel.0 * snd_rel.1;
    let left = fst_rel.1 * snd_rel.0;
    if left.is_unknown() { solve_unknown(left, right).unwrap() } else { solve_unknown(right, left).unwrap() }
}

pub fn rule_of_three_d(fst_rel: (Dimension, Value), snd_rel: (Dimension, Dimension)) -> Value {
    let right = fst_rel.0 * snd_rel.1 / snd_rel.0;
    let left = fst_rel.1;
    solve_unknown(left, Value::from(right)).unwrap()
}

pub fn rule_of_three(fst_rel: (f64, Value), snd_rel: (f64, f64)) -> Value {
    let right = fst_rel.0 * snd_rel.1 / snd_rel.0;
    let left = fst_rel.1;
    solve_unknown(left, Value::from(right)).unwrap()
}
