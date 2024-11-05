mod error;

use error::EquationError::{self, *};
use error::{EquaResult, ParseErr};

use dimension::{separate_value_unit, split_value_unit, CustomUnits, UnitsDict};
use expression::api::expr_tree_to_infix;
use expression::expr_structs::ExprTree::{self, Leaf, Operation};
use expression::expr_structs::ExprUnit::{Num, Op};
use expression::Tier::*;
use expression::*;

use std::mem::take;

fn seek_in_tree(tree: &ExprTree, target: &ExprTree) -> bool {
    match tree {
        Operation(node, _, _) if seek_in_tree(node, target) => true,
        Operation(_, _, Op(Tier3, _)) => false,
        Operation(_, node, _) if seek_in_tree(node, target) => true,
        tree => tree == target
    }
}

fn can_take(node: &ExprTree, target: &ExprTree, takes_target: bool, tier: Tier) -> bool {
    if (tier == Tier1 && node == &ZERO) || (tier == Tier2 && node == &ONE) {
        return false;
    }

    (takes_target && seek_in_tree(node, target)) || (!takes_target && !seek_in_tree(node, target))
}

fn take_other_values(tree: &mut ExprTree, target: &ExprTree, takes_target: bool) -> ExprResult<CommonFactor> {
    match take(tree) {
        Operation(node1, node2, POW) if !takes_target => {
            *tree = *node1;
            Ok(Some((ExprTree::make_opr(ONE, *node2, DIV), POW)))
        }
        Operation(node1, node2, Op(t, i)) if t != Tier3 => {
            let op = Op(t, i);
            let (mut node1, mut node2) = (*node1, *node2);
            let (mut taken_left, mut taken_right) = if t == Tier1 {
                (ZERO, ZERO)
            } else { (ONE, ONE) };

            let can_take_left = can_take(&node1, target, takes_target, t);
            let can_take_right = can_take(&node2, target, takes_target, t);

            let target_is_mul = (t == Tier2 && i) && can_take_right;
            let target_is_dividend = t == Tier2 && can_take_left;

            if takes_target && (target_is_mul || target_is_dividend)  {
                *tree = ZERO;
                return Ok(Some((ExprTree::make_opr(node1, node2, op), SUB)))
            }

            if can_take(&node1, target, takes_target, t) {
                (node1, taken_left) = (taken_left, node1);
            }

            if can_take(&node2, target, takes_target, t) {
                (node2, taken_right) = (taken_right, node2);
            }

            *tree = ExprTree::make_opr(node1, node2, op.clone());

            if !can_take_left && !can_take_right { return Ok(None); }

            if !i { (taken_left, taken_right) = (taken_right, taken_left) }
            let final_value = ExprTree::make_opr(taken_left, taken_right, op);

            let result = (final_value, Op(t, !i));
            Ok(Some(result))
        }
        leaf if can_take(&leaf, target, takes_target, Tier1) => {
            *tree = ZERO;
            Ok(Some((leaf, SUB)))
        }
        leaf => {
            *tree = leaf;
            Ok(None)
        }
    }
}

fn fix_opposite_and_inverse(mut left: ExprTree, mut right: ExprTree, target: &ExprTree) -> ExprResult<(ExprTree, ExprTree)> {
    if let Operation(node1, node2, DIV) = left {
        if seek_in_tree(&node2, target) {
            let minus_one = Leaf(Num(-1.0));
            left = Operation(node2, node1, DIV);
            right = ExprTree::organize(right, minus_one, POW)?;
        } else { left = Operation(node1, node2, DIV); }
    };

    if let Operation(node1, node2, SUB) = left {
        if *node1 == ZERO {
            left = *node2;
            right = ExprTree::organize(ZERO, right, SUB)?;
        } else { left = Operation(node1, node2, SUB); }
    }

    Ok((left, right))
}

pub fn simplify_equation_tree(mut left: ExprTree, mut right: ExprTree, target: ExprTree) -> ExprResult<(ExprTree, ExprTree)> {
    left = simplify(left)?;
    right = simplify(right)?;
    let mut changed = false;

    (left, right) = fix_opposite_and_inverse(left, right, &target)?;

    if let Some((val, op)) = take_other_values(&mut left, &target, false)? {
        right = operate_and_simplify(right, val, op)?;
        changed = true;
    }

    if let Some((val, op)) = take_other_values(&mut right, &target, true)? {
        left = operate_and_simplify(left, val, op)?;
        changed = true;
    }

    if changed {
        simplify_equation_tree(left, right, target)
    } else {
        let left = left.propagate(
            |c1, c2, op| Ok(ExprTree::make_opr(c1, c2, op)),
            number_to_sci_notation,
        )?;
        let right = right.propagate(
            |c1, c2, op| Ok(ExprTree::make_opr(c1, c2, op)),
            number_to_sci_notation,
        )?;

        Ok((left, right))
    }
}

fn take_infix(left: ExprTree, right: ExprTree, sep: &str) -> (String, String) {
    (expr_tree_to_infix(&left, sep), expr_tree_to_infix(&right, sep))
}

fn basic_simplify(equation: &str, target: &str, sep: &str, marker: &str) -> EquaResult<String> {
    let (left, right) = equation.split_once(marker).ok_or(SyntaxError(equation.to_string()))?;

    let left = ExprTree::new(left).parse_err(equation)?;
    let right = ExprTree::new(right).parse_err(equation)?;
    let target = ExprTree::new(target).parse_err(equation)?;

    let simplified = simplify_equation_tree(left, right, target);
    let (left, right) = simplified.parse_err(equation)?;

    let (left, right) = take_infix(left, right, sep);

    let result = [left, marker.to_string(), right];

    Ok(result.join(sep))
}

pub fn simplify_differentiation(differentiation: &str, target: &str, sep: &str) -> EquaResult<String> {
    basic_simplify(differentiation, target, sep, "!=")
}

pub fn simplify_equation(equation: &str, target: &str, sep: &str) -> EquaResult<String> {
    basic_simplify(equation, target, sep, "=")
}

fn take_final_unit(tree: ExprTree, dict: &mut UnitsDict, equation: &str) -> EquaResult<(ExprTree, String)> {
    let (tree, unit) = separate_value_unit(
        tree, dict, &|unit: &str| {
            let new_unit = unit.trim_start_matches("_u_");
            if unit != new_unit { ("1", new_unit.to_string()) } else { (unit, String::new()) }
        },
        &|_, _, _| { Ok(()) },
    ).parse_err(equation)?;

    let unit = expr_tree_to_infix(&unit, "");

    Ok((tree.propagate(ExprTree::clean, |x| Ok(x)).parse_err(equation)?, unit))
}

pub fn basic_simplify_dimen(
    equation: &str,
    target: &str,
    sep: &str,
    marker: &str,
    custom_units: &CustomUnits) -> EquaResult<String> {
    let (left, right) = equation.split_once(marker).ok_or(SyntaxError(equation.to_string()))?;

    let left = ExprTree::raw_new(left).parse_err(equation)?;
    let right = ExprTree::raw_new(right).parse_err(equation)?;

    let mut dict = UnitsDict::new();
    let mut separate = |tree| {
        separate_value_unit(
            tree, &mut dict, &|text| split_value_unit(text, custom_units), &|dict, value, unit| {
                dict.take_unit_with_custom(value, unit, custom_units)?;
                *unit = "_u_".to_string() + &unit;
                Ok(())
            },
        )
    };

    let (left_value, left_unit) = separate(left).parse_err(equation)?;
    let (right_value, right_unit) = separate(right).parse_err(equation)?;

    let mut left = ExprTree::make_opr(left_value, left_unit, MUL);
    let mut right = ExprTree::make_opr(right_value, right_unit, MUL);
    let target = ExprTree::new(target).parse_err(equation)?;

    let simplified = simplify_equation_tree(left, right, target);
    (left, right) = simplified.parse_err(equation)?;

    let (left, unit_left) = take_final_unit(left, &mut dict, equation)?;
    let (right, unit_right) = take_final_unit(right, &mut dict, equation)?;

    let (left, right) = take_infix(left, right, sep);

    let result = [left, unit_left, marker.to_string(), right, unit_right];

    Ok(result.join(sep))
}

pub fn simplify_dimen_differentiation(
    differentiation: &str,
    target: &str,
    sep: &str,
    custom_units: &CustomUnits) -> Result<String, EquationError> {
    basic_simplify_dimen(differentiation, target, sep, "!=", custom_units)
}

pub fn simplify_dimen_equation(
    equation: &str,
    target: &str,
    sep: &str,
    custom_units: &CustomUnits) -> Result<String, EquationError> {
    basic_simplify_dimen(equation, target, sep, "=", custom_units)
}
