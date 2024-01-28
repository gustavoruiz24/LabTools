use dimension::*;
use value::Value;

pub fn solve_unknown(unk: Value, exp_res: Value) -> Result<Value, String> {
    if unk.is_number() {
        return Err(
            "The `unk` parameter from `solve_unknown` just accepts `Value::Unknown`".to_string(),
        );
    }
    let unk = unk.unwrap_unk();
    let ops: Vec<&str> = unk.split(':').collect::<Vec<&str>>()[1..].to_vec();
    let mut result = exp_res;

    for op in ops.iter().rev() {
        let info: (String, String, String);
        {
            let letters: Vec<&str> = op
                .split(|x: char| x.is_numeric() || x == '.' || x == '-')
                .filter(|x| !x.is_empty())
                .collect();
            let number = op.replace(|x: char| !x.is_numeric() && x != '.' && x != '-', "");

            if !number.is_empty() && letters.len() == 2 {
                info = (letters[0].to_string(), number, letters[1].to_string());
            } else {
                return Err("Invalid values were passed to `unk` in `solve_unknown`.".to_string());
            }
        }

        let (op, string_val, unit) = info;

        let val = if &unit != "ND" {
            GeneDimen::soft_from(string_val + &unit).or_else(|err| Err(err.to_string()))?
        } else {
            GeneDimen::from(SimpDimen::init_nd(string_val.parse::<f64>().unwrap()))
        };

        match op.as_str() {
            "p" => result -= val,
            "s" => result += val,
            "m" => result /= val,
            "d" => result *= val,
            "pw" => result = result.pow(val.pow(-1.0)),
            _ => return Err("Invalid values were passed to `unk` in `solve_unknown`.".to_string()),
        }
    }

    Ok(result)
}

pub fn rule_of_three_v(fst_rel: (Value, Value), snd_rel: (Value, Value)) -> Value {
    let right = fst_rel.0 * snd_rel.1;
    let left = fst_rel.1 * snd_rel.0;
    if left.is_unknown() {
        solve_unknown(left, right).unwrap()
    } else {
        solve_unknown(right, left).unwrap()
    }
}

pub fn rule_of_three_d(fst_rel: (GeneDimen, Value), snd_rel: (GeneDimen, GeneDimen)) -> Value {
    let right = fst_rel.0 * (snd_rel.1 / snd_rel.0);
    let left = fst_rel.1;
    solve_unknown(left, Value::from(right)).unwrap()
}

pub fn rule_of_three(fst_rel: (f64, Value), snd_rel: (f64, f64)) -> Value {
    let right = fst_rel.0 * snd_rel.1 / snd_rel.0;
    let left = fst_rel.1;
    solve_unknown(left, Value::from(right)).unwrap()
}
