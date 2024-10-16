pub mod api;
pub mod expr_structs;
pub mod macros;
pub mod error;
pub mod traits;

use std::mem::take;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, LowerExp, UpperExp};

use expr_structs::*;
use error::ExprError;
use self::ExprTree::{Leaf, Operation};
use self::ExprUnit::*;
use self::Tier::*;

type Expr = Vec<ExprTree>;
type ExprResult<T> = Result<T, ExprError>;
type ExprTreeResult = ExprResult<ExprTree>;
type CommonFactor = Option<(ExprTree, ExprUnit)>;

pub const ADD: ExprUnit = Op(Tier1, true);
pub const SUB: ExprUnit = Op(Tier1, false);
pub const MUL: ExprUnit = Op(Tier2, true);
pub const DIV: ExprUnit = Op(Tier2, false);
pub const POW: ExprUnit = Op(Tier3, true);
pub const ROOT: ExprUnit = Op(Tier3, false);

pub const ZERO: ExprTree = Leaf(Num(0.0));
pub const ONE: ExprTree = Leaf(Num(1.0));

#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub enum Tier {
    #[default]
    Tier1 = 1,
    Tier2,
    Tier3,
}

impl Tier {
    fn get_num(&self) -> i8 {
        *self as i8
    }

    fn plus_one(self) -> Self {
        match self {
            Tier1 => Tier2,
            Tier2 => Tier3,
            Tier3 => panic!("There is no tier above Tier::Tier3")
        }
    }
}

impl PartialOrd for Tier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get_num().partial_cmp(&other.get_num())
    }
    fn lt(&self, other: &Self) -> bool {
        self.get_num().lt(&other.get_num())
    }
    fn le(&self, other: &Self) -> bool {
        self.get_num().le(&other.get_num())
    }
    fn gt(&self, other: &Self) -> bool {
        self.get_num().gt(&other.get_num())
    }
    fn ge(&self, other: &Self) -> bool {
        self.get_num().ge(&other.get_num())
    }
}

pub fn tuple_to_op(tuple: (Tier, bool)) -> ExprUnit {
        Op(tuple.0, tuple.1)
}

pub fn operate_and_simplify(left: ExprTree, right: ExprTree, op: ExprUnit) -> ExprTreeResult {
    ExprTree::make_opr(left, right, op)
        .propagate(ExprTree::clean, |x| Ok(x))?
        .propagate(apply_simplifications, |x| Ok(x))
}

pub fn simplify(tree: ExprTree) -> ExprTreeResult {
    tree.propagate(ExprTree::clean, |x| Ok(x))?
        .propagate(apply_simplifications, |x| Ok(x))
}

pub fn operate_nums(num1: f64, op: (Tier, bool), num2: f64) -> ExprTree {
    let mut result = match op {
        (Tier1, true) => num1 + num2,
        (Tier1, false) => num1 - num2,
        (Tier2, true) => num1 * num2,
        (Tier2, false) => num1 / num2,
        (Tier3, true) => num1.powf(num2),
        (Tier3, false) => num1.powf(1.0 / num2),
    };

    result = (result * 10e10).round() / 10e10;
    Leaf(Num(result))
}

pub fn apply_simplifications(mut child1: ExprTree, mut child2: ExprTree, mut op: ExprUnit) -> ExprTreeResult {
    let mut changed = true;

    while changed {
        changed = false;
        shorten_expr(&mut child1, &mut child2, op.clone(), true, true)?;
        if let Operation(c1, c2, new_op) = child1 {
            (child1, child2, op) = (*c1, *c2, new_op);
        } else {
            return Ok(child1);
        }
    };

    Ok(ExprTree::make_opr(child1, child2, op))
}

fn take_or_clone(expr: &mut ExprTree, can_move: bool) -> ExprTree {
    if can_move {
        take(expr)
    } else { expr.clone() }
}

fn make_opr_from_ref(left: &mut ExprTree, right: &mut ExprTree, op: ExprUnit, can_move: bool) -> ExprTree {
    let left = take(left);
    let right = take_or_clone(right, can_move);

    ExprTree::make_opr(left, right, op)
}

fn distributive(left: &mut ExprOper, right: &mut ExprTree, op: ExprUnit, can_move: bool) -> ExprResult<bool> {
    let changed1 = shorten_expr(&mut left.left, right, op.clone(), false, false)?;
    let can_move2 = changed1 && can_move;
    let changed2 = shorten_expr(&mut left.right, right, op.clone(), can_move2, changed1)?;

    if !(changed1 || changed2) {
        return Ok(false);
    } else if !changed1 {
        left.left = make_opr_from_ref(&mut left.left, right, op, can_move);
    }
    Ok(true)
}

fn match_equivalent(left: &mut ExprOper, right: &mut ExprOper, op: ExprUnit, invert: bool, can_move: bool) -> ExprResult<bool> {
    let (a, mut b) = (&mut left.left, &mut left.right);
    let (mut x, mut y) = (&mut right.left, &mut right.right);
    let mut is_default1 = false;

    let (mut r_left_incr, mut r_right_incr) = (true, right.op.1);
    let mut can_move2 = can_move;

    if invert {
        (x, y) = (y, x);
        is_default1 = right.is_default;
        (r_left_incr, r_right_incr) = (r_right_incr, r_left_incr);
    };

    let main_op_info = op.clone().op_as_tuple();

    let op1_incr = main_op_info.1 == r_left_incr;
    let op1 = Op(main_op_info.0, op1_incr);
    
    let op2_incr = (left.op.1 == main_op_info.1) == r_right_incr;
    let op2 = Op(main_op_info.0, op2_incr);
    let mut final_op_incr = true;
    
    if !left.op.1 {
        if op2_incr {
            final_op_incr = false;
        }
        else {
            can_move2 = true;
            (b, y) = (y, b);
        }
    };

    let (changed1, changed2) = if is_default1 {
        let changed2 = shorten_expr(b, y, op2, can_move, false)?;
        shorten_expr(a, x, op1.clone(), can_move2, changed2)?;

        (false, changed2)
    } else {
        let changed1 = shorten_expr(a, x, op1.clone(), can_move, false)?;
        let changed2 = shorten_expr(b, y, op2, can_move2, changed1)?;

        (changed1, changed2)
    };
    
    if !(changed1 || changed2) {
        return Ok(false);
    } else if !changed1 {
        *a = make_opr_from_ref(a, x, op1, can_move);
    }

    let (mut a, mut b) = (take(a), take(b));
    let final_op = Op(main_op_info.0, final_op_incr);

    if (!left.op.1 && !main_op_info.1) && right.op.1 {
        if shorten_expr(&mut b, &mut a, final_op.clone(), true, false)? {
            *left = get_expr_opr_by_op(b, final_op, false);
        } else {
            (left.left, left.right) = (b, a);
            left.op = final_op.op_as_tuple();
        }
    }
    else {
        (left.left, left.right) = (a, b);
        left.op = final_op.op_as_tuple();
    }
    Ok(true)
}

fn insert_reduced_pow_value(source: &mut ExprTree, val: ExprTree, new_val: &ExprTree,
                            orig_pow: ExprTree, pow: ExprTree) {
    let multiplier1 = ExprTree::make_opr(new_val.clone(), pow, POW);
    let multiplier2 = ExprTree::make_opr(val, orig_pow, POW);

    *source = ExprTree::make_opr(multiplier1, multiplier2, MUL);
}

fn recover(source: &mut ExprTree, mut oper: ExprOper, l: ExprTree, can_recover: bool) {
    if can_recover {
        oper.left = l;
        *source = oper.as_expr_tree();
    }
}

fn common_operation(a: &mut ExprTree, b: &mut ExprTree, op: ExprUnit,
                    x: &mut ExprTree, y: &mut ExprTree) -> ExprResult<CommonFactor> {
    let factor1 = common_factor(a, x, (Tier1, true))?.unwrap_or((ONE, MUL)).0; 
    let factor2 = common_factor(b, y, (Tier1, true))?.unwrap_or((ONE, MUL)).0; 

    Ok(
        if factor1 != ONE {
            Some((ExprTree::make_opr(factor1, factor2, op), MUL))
        } else if factor2 != ONE {
            Some((factor2, op))
        } else { None }
      )
}

fn common_factor(orig_left: &mut ExprTree, orig_right: &mut ExprTree, op: (Tier, bool)) -> ExprResult<CommonFactor> {
    let mut left = get_expr_opr_by_op(take(orig_left), POW, true);
    let mut right = get_expr_opr_by_op(take(orig_right), POW, true);
    let both_are_default = left.is_default && right.is_default;

    let result = match (take(&mut left.left), take(&mut right.left)) {
        (x, y) if x == ONE || y == ONE => {
            left.left = x;
            right.left = y;
            None
        }
        (mut x, mut y) if !both_are_default || op.0 == Tier2 => {
            let mut result = None;
            let (mut recover_left, mut recover_right) = (true, true);
            let (p1, p2) = (&mut left.right, &mut right.right);

            if (p1.is_num() && p2.is_num()) || op == (Tier2, false) {
                if let Some(mut cf) = common_factor(&mut x, &mut y, (Tier1, true))? {
                    let cases = if let (Leaf(Num(n1)), Leaf(Num(n2))) = (&p1, &p2) {
                        (n1 < n2, n1 > n2)
                    } else {
                        (false, true)
                    };

                    if op == (Tier2, true) {
                        let mut cf_oper = get_expr_opr_by_op(cf.0, POW, true);
                        cf_oper.right = ExprTree::make_opr(p1.clone(), p2.clone(), ADD);
                        cf_oper.is_default = false;
                        cf.0 = cf_oper.as_expr_tree();
                    } else if cases.0 {
                        let pow = ExprTree::make_opr(p2.clone(), p1.clone(), SUB);
                        insert_reduced_pow_value(orig_right, take(&mut y), &cf.0, take(p2), pow);
                        recover_right = false;
                    } else if cases.1 {
                        let pow = ExprTree::make_opr(p1.clone(), p2.clone(), SUB);
                        insert_reduced_pow_value(orig_left, take(&mut x), &cf.0, take(p1), pow);
                        recover_left = false;
                    } 

                    if op == (Tier2, false) {
                        cf.0 = ONE;
                    }

                    result = Some(cf);
                }
            } else if op.0 == Tier2 {
                if let Some(mut cf) = common_factor(&mut left.right, &mut right.right, (Tier1, true))? {
                    cf.1 = POW;
                    result = Some(cf);
                }
            }

            recover(orig_left, left, x, recover_left);
            recover(orig_right, right, y, recover_right);

            return Ok(result)
        }
        (x, y) if x == y => {
            left.left = ONE;
            right.left = ONE;

            Some((x, MUL))
        }
        (x, mut y) if seek_multiplier(&y, &x) => {
            let mut factor = x;

            shorten_expr(&mut y, &mut factor, DIV, false, false)?;
            left.left = ONE;
            right.left = y;

            Some((factor, MUL))
        }
        (Operation(mut w, mut x, DIV), Operation(mut y, mut z, DIV)) => {
            let result = common_operation(&mut w, &mut x, DIV, &mut y, &mut z)?;

            left.left = Operation(w, x, DIV);
            right.left = Operation(y, z, DIV);

            result
        }
        (Operation(mut x, mut y, o), mut z) => {
            let factor1 = common_factor(&mut x, &mut z, op)?.unwrap_or((ONE, MUL)).0; 
            let factor2 = if o == MUL {
                common_factor(&mut y, &mut z, op)?.unwrap_or((ONE, MUL)).0
            } else { ONE }; 

            left.left = Operation(x, y, o);
            right.left = z;

            if factor1 != ONE || factor2 != ONE {
                Some((ExprTree::make_opr(factor1, factor2, MUL), MUL))
            } else { None }
        }
        (x, y) => {
            left.left = x;
            right.left = y;
            None
        }
    };

    *orig_left = left.as_expr_tree();
    *orig_right = right.as_expr_tree();

    Ok(result)
}

fn get_next_op(op: ExprUnit, always_true: bool) -> ExprUnit {
    match op {
        Op(Tier3, i) => Op(Tier3, i || always_true),
        Op(t, i) => Op(t.plus_one(), i || always_true),
        _ => unreachable!()
    }
}

fn get_expr_opr_by_op(expr_tree: ExprTree, target_op: ExprUnit, strict: bool) -> ExprOper {
    match expr_tree {
        Operation(c1, c2, op) if !strict || op == target_op => ExprOper::new(*c1, *c2, op.op_as_tuple(), false),
        _ => {
            if let Op(Tier1, _) = target_op {
                ExprOper::new(expr_tree, ZERO, target_op.op_as_tuple(), true)
            } else { ExprOper::new(expr_tree, ONE, target_op.op_as_tuple(), true) }
        }
    }
}

fn seek_multiplier(expr: &ExprTree, target: &ExprTree) -> bool {
    match expr {
        Operation(c1, c2, MUL) => {
            seek_multiplier(c1, target) || seek_multiplier(c2, target)
        },
        Operation(c1, _, POW) => seek_multiplier(c1, target),
        Leaf(Num(_)) if target.is_num() => true,
        expr if expr == target => true,
        _ => false
    }
}

fn seek_divisor(expr: &ExprTree, target: &ExprTree) -> bool {
    if let Operation(_, c2, DIV) = expr {
        seek_multiplier(c2, target)
    } else { false }
}

fn shorten_expr_early(left: &mut ExprTree, right: &mut ExprTree, op: ExprUnit,
                 can_move: bool, ensure_change: bool) -> Option<bool> {
    match (&left, &right) {
        (Leaf(Num(n1)), Leaf(Num(n2))) => {
            *left = operate_nums(*n1, op.op_as_tuple(), *n2);
            return Some(true);
        },
        (Leaf(Unk(_)), Leaf(Num(_))) | (Leaf(Num(_)), Leaf(Unk(_))) => {
            if ensure_change {
                *left = make_opr_from_ref(left, right, op, can_move);
            }
            return Some(false);
        },
        (l, r) if l == r && op.op_as_tuple().0 == Tier2 => {
            let new_op = POW;

            let mut left_opr = get_expr_opr_by_op(take(left), new_op.clone(), true);
            left_opr.is_default = false;
            *left = left_opr.as_expr_tree();

            let mut right_opr = get_expr_opr_by_op(take(right), new_op, true);
            right_opr.is_default = false;
            *right = right_opr.as_expr_tree();

            None
        },
        _ => None
    }
}

pub fn shorten_expr(orig_left: &mut ExprTree, orig_right: &mut ExprTree,
                    op: ExprUnit, can_move: bool, ensure_change: bool) -> ExprResult<bool> {
    if let Some(changed) = shorten_expr_early(orig_left, orig_right, op.clone(), can_move, ensure_change){
        return Ok(changed);
    }

    let mut left = if let Some(op) = orig_right.get_op() {
        get_expr_opr_by_op(take(orig_left), op, false)
    } else {
        let next_op = get_next_op(op.clone(), true);
        get_expr_opr_by_op(take(orig_left), next_op, false)
    };

    let tuple_op = op.clone().op_as_tuple();
    let mut right = ExprOper::new(ZERO, ZERO, (Tier1, true), true);
    let mut recover_right = false;
    let can_simp = !left.is_default;

    let changed = match tuple_op.0.get_num() - left.op.0.get_num() {
        1 => {
            let will_remove = if op == DIV {
                seek_multiplier(&left.left, orig_right) || seek_multiplier(&left.right, orig_right)
            } else if op == MUL {
                seek_divisor(&left.left, orig_right) || seek_divisor(&left.right, orig_right)
            } else { true };

            if will_remove && can_simp {
                distributive(&mut left, orig_right, op.clone(), can_move)?
            } else { false }
        }
        0 => {
            if let Tier3 = left.op.0 {
                false
            } else {
                let left_op = tuple_to_op(left.op);
                let right_copy = take_or_clone(orig_right, can_move);
                right = get_expr_opr_by_op(right_copy, left_op, true);
                recover_right = true;

                let changed = match_equivalent(&mut left, &mut right, op.clone(), false, can_move)?;

                if !changed {
                    match_equivalent(&mut left, &mut right, op.clone(), true, can_move)?
                } else { changed }
            }
        }
        _ => {
            let mut right_copy = take_or_clone(orig_right, can_move);
            let mut left_copy = left.as_expr_tree();

            let changed = if let Some(cf) = common_factor(&mut left_copy, &mut right_copy, tuple_op)? {
                shorten_expr(&mut left_copy, &mut right_copy, op.clone(), true, true)?;
                left_copy = ExprTree::make_opr(left_copy, cf.0, cf.1);

                true
            } else {
                *orig_right = right_copy;
                false
            };

            left = get_expr_opr_by_op(left_copy, MUL, false);
            changed
        }
    };
    
    left.is_default = left.is_default && !changed;
    let left = left.as_expr_tree();

    if recover_right && can_move {
        *orig_right = right.as_expr_tree();
    }

    *orig_left = if !changed && ensure_change {
        ExprTree::make_opr(left, take_or_clone(orig_right, can_move), op)
    } else {
        left
    }.propagate(ExprTree::clean, |x| Ok(x))?;

    Ok(changed)
}
