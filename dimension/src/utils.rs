use crate::*;

pub fn bcm_no_decimals(sd: &mut SimpDimen) {
    sd.remove_prefix();
    for u in sd.get_unit_pos()..sd.get_units().len() {
        sd.bcm_unit_unchecked(sd.get_prefix(), u);
        let integer_part = sd.get_value().trunc();
        if sd.get_value() == integer_part {
            return;
        } else if sd.get_value() > integer_part {
            break;
        }
    }
    for (i, (_, p)) in sd.get_prefixes().iter().enumerate() {
        if sd.get_value() * p.powi(-1) >= sd.get_value().trunc() {
            sd.bcm_unit_unchecked(i, sd.get_unit_pos());
            return;
        }
    }
    sd.set_value(sd.get_value().round());
}

pub fn bcm_lower_with_no_dec(sd: &mut SimpDimen) {
    sd.bcm_unit_unchecked(sd.get_prefix(), 0);
    bcm_no_decimals(sd);
}

pub fn format<S: AsRef<str>>(sd: &SimpDimen, separator: S, show_rest: bool) -> String {
    let mut formatted = Vec::new();
    let mut working_copy = sd.clone();
    working_copy.remove_prefix();

    for unit_index in 0..working_copy.get_units().len() {
        working_copy.bcm_unit_unchecked(sd.get_prefix(), unit_index);
        let integer_part = working_copy.get_value().trunc();

        if integer_part != 0.0 {
            let is_last = formatted.len() + 1 == working_copy.get_units().len();
            let val = if show_rest && is_last {
                working_copy.get_value()
            } else {
                integer_part
            };

            let val_to_display = SimpDimen::val_to_display(val, working_copy.get_unit(), true);
            formatted.push(val_to_display);
            working_copy -= integer_part;
        }
    }
    formatted.join(separator.as_ref())
}

pub fn format_with_units<'a, U, S>(
    sd: &SimpDimen,
    units: U,
    separator: S,
    show_unit: bool,
    show_rest: bool,
) -> Result<String, DimenError>
where
    U: IntoIterator<Item = &'a str> + Clone,
    S: AsRef<str>,
{
    let mut formatted = Vec::new();
    let mut working_copy = sd.clone();
    working_copy.remove_prefix();

    let to_convert = units
        .into_iter()
        .map(|x| sd.to_unit(x))
        .collect::<Result<Vec<SimpDimen>, DimenError>>();

    let to_convert = to_convert?;

    let mut to_convert: Vec<_> = to_convert.iter().enumerate().collect();
    to_convert.sort_unstable_by(|(_, a), (_, b)| {
        a.get_value()
            .partial_cmp(&b.get_value())
            .expect("Some SimpDimen received a non-numeric value.")
    });

    for (i, d) in &to_convert {
        working_copy.bcm_unit_unchecked(d.get_prefix(), d.get_unit_pos());
        let is_last = formatted.len() + 1 == to_convert.len();

        let integer_part = working_copy.get_value().trunc();
        let val = if show_rest && is_last {
            working_copy.get_value()
        } else {
            integer_part
        };

        let val_to_display = SimpDimen::val_to_display(val, working_copy.get_unit(), show_unit);
        formatted.push((val_to_display, i));

        if is_last {
            break;
        }

        working_copy -= integer_part;
    }

    formatted.sort_unstable_by(|a, b| a.1.cmp(b.1));

    Ok(formatted
        .iter()
        .map(|x| x.0.as_str())
        .collect::<Vec<&str>>()
        .join(separator.as_ref()))
}

pub fn units_vs_si(base: SimpDimenBase) {
    let d: SimpDimen = SimpDimen::new(base) + 1;
    for unit in d.get_units().iter() {
        println!(
            "{:4} {}",
            unit.to_string() + ":",
            SimpDimen::init(1.0, unit, base).unwrap().to_si()
        )
    }
}

pub fn units_vs_unit(base: SimpDimenBase, unit: &str) -> Result<(), DimenError> {
    let d: SimpDimen = SimpDimen::new(base) + 1;
    for c_unit in d.get_units().iter() {
        println!(
            "{:4} {}",
            c_unit.to_string() + ":",
            SimpDimen::init(1.0, c_unit, base).unwrap().to_unit(unit)?
        )
    }
    Ok(())
}

pub fn show_prefixes() {
    for prefix in PREFIXES {
        if !prefix.0.is_empty() {
            println!("{}: {}", prefix.0, prefix.1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_units_test() {
        let d = SimpDimen::from("1.11h");
        assert_eq!(
            format_with_units(&d, ["h", "min", "s"], ":", true, false).unwrap(),
            "1h:6min:36s".to_string()
        );
        let d = SimpDimen::from("1965.210959y");
        assert_eq!(
            format_with_units(&d, ["day", "mo", "y"], "/", false, false).unwrap(),
            "17/2/1965".to_string()
        );
        let d = SimpDimen::from("717793.3028day");
        assert_eq!(
            format_with_units(&d, ["mo", "day", "y"], "/", false, false).unwrap(),
            "2/17/1965".to_string()
        );
        let d = SimpDimen::from("1.1111m");
        assert_eq!(
            format_with_units(&d, ["m", "cm", "mm"], " ", true, true).unwrap(),
            "1m 11cm 1.0999999999999766mm".to_string()
        );
    }
}
