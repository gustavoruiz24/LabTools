use std::mem::take;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

use self::ExprError::*;
use self::ExprTree::{Leaf, Operation};
use self::ExprUnit::*;
use self::Tier::*;

type Expr = Vec<ExprTree>;
type ExprOption = Option<ExprTree>;
type ExprResult = Result<ExprOption, ExprError>;

const ADD: ExprUnit = Op(Tier1, true);
const SUB: ExprUnit = Op(Tier1, false);
const MUL: ExprUnit = Op(Tier2, true);
const DIV: ExprUnit = Op(Tier2, false);
const POW: ExprUnit = Op(Tier3, true);

const ZERO: ExprTree = Leaf(Num(0.0));
const ONE: ExprTree = Leaf(Num(1.0));

#[derive(Copy, Clone, PartialEq, Debug)]
enum Tier {
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

#[derive(PartialEq, Debug)]
pub enum ExprError {
    SyntaxError,
    MathError,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExprUnit {
    Num(f64),
    Unk(String),
    // Tier 1 = add/sub; Tier 2 = mul/div; Tier 3 = pow;
    // bool = increases or not
    Op(Tier, bool),
    OpenBracket,
    CloseBracket,
}

impl ExprUnit {
    fn op_as_tuple(self) -> (Tier, bool) {
        if let Op(tier, incr) = self {
            (tier, incr)
        } else { panic!("Called op_as_tuple method on an non-Op ExprTree.") }
    }
}

impl Display for ExprUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Num(x) => write!(f, "{}", x),
            Unk(x) => write!(f, "{}", x),
            OpenBracket => write!(f, "("),
            CloseBracket => write!(f, ")"),
            &ADD => write!(f, "+"),
            &SUB => write!(f, "-"),
            &MUL => write!(f, "*"),
            &DIV => write!(f, "/"),
            &POW => write!(f, "^"),
            _ => unreachable!()
        }
    }
}

fn push_expr_leaf(building_leaf: &mut String, expr: &mut Expr, new: Option<ExprTree>) -> Result<(), ExprError> {
    if !building_leaf.is_empty() {
        let value = if let Ok(n) = building_leaf.parse::<f64>() {
            building_leaf.clear();
            Num(n)
        } else { Unk(take(building_leaf)) };

        expr.push(Leaf(value));
    }

    if let (Some(Leaf(Unk(_) | Num(_) | OpenBracket)), true) = (expr.last(), expr.len() > 1) {
        if let Leaf(Unk(_) | Num(_) | CloseBracket) = &expr[expr.len() - 2] {
            expr.insert(expr.len() - 1, Leaf(MUL))
        }
    }

    match (new, expr.last_mut()) {
        (Some(Leaf(Op(Tier2 | Tier3, _))), Some(Leaf(Op(_, _))) | None) => return Err(SyntaxError),
        (Some(Leaf(Op(Tier1, x))), Some(Leaf(Op(Tier1, y)))) => {
            *y = !(x ^ *y)
        }
        (Some(Leaf(ADD)), Some(Leaf(Op(_, _))) | None) => {}
        (Some(Leaf(SUB)), Some(Leaf(OpenBracket)) | None) => {
            expr.push(ZERO);
            expr.push(Leaf(SUB));
        }
        (Some(Leaf(Op(x, y))), _) => expr.push(Leaf(Op(x, y))),
        (Some(nt), Some(Leaf(Unk(_) | Num(_) | CloseBracket))) if nt != Leaf(CloseBracket) => {
            expr.push(Leaf(MUL));
            expr.push(nt);
        }
        (Some(nt), _) => expr.push(nt),
        _ => {}
    }

    Ok(())
}

fn condense_op(expr: &mut Expr, tier: Tier) {
    let mut pos: Vec<(usize, bool)> = vec![];
    let mut last_is_target = false;

    for (i, building_leaf) in expr.iter().enumerate() {
        match building_leaf {
            Leaf(SUB) if last_is_target => {
                pos.last_mut().unwrap().1 = true;
                last_is_target = false
            },
            Leaf(Op(t, _)) if *t == tier => {
                pos.push((i - 1, false));
                last_is_target = true
            },
            _ => last_is_target = false
        }
    }

    let mut diff = 0;
    for (mut p, is_neg) in pos {
        p -= diff;

        let val_1 = expr.remove(p);
        let op = expr.remove(p).unwrap_leaf();
        let val_2 = expr.remove(p);
        let val_2 = if is_neg {
            diff += 1;
            ExprTree::make_opr(ZERO, expr.remove(p), SUB)
        } else { val_2 };

        let operation = ExprTree::make_opr(val_1, val_2, op);
        diff += 2;

        expr.insert(p, operation);
    }
}

fn operate_nums(num1: f64, op: (Tier, bool), num2: f64) -> ExprTree {
    match op {
        (Tier1, true) => Leaf(Num(num1 + num2)),
        (Tier1, false) => Leaf(Num(num1 - num2)),
        (Tier2, true) => Leaf(Num(num1 * num2)),
        (Tier2, false) => Leaf(Num(num1 / num2)),
        (Tier3, true) => Leaf(Num(num1.powf(num2))),
        _ => unreachable!()
    }
}

fn format_expr(expr: &[ExprTree]) -> String {
    expr.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ")
}

#[derive(Debug, Clone)]
pub enum ExprTree {
    Operation(Box<ExprTree>, Box<ExprTree>, ExprUnit),
    Leaf(ExprUnit),
}

macro_rules! push_expr_leaf {
    ($eu: expr, $e:expr, $leaf: expr) => {
        push_expr_leaf(&mut $eu, &mut $e, Some(Leaf($leaf)))?
    }
}

impl ExprTree {
    pub fn new(expr_str: &str) -> Result<ExprTree, ExprError> {
        let expr = Self::parse_expr(expr_str)?;
        Self::build(expr)
    } 

    fn build(mut expr: Expr) -> Result<ExprTree, ExprError> {
        let mut pos = vec![];
        let mut open_brackets = vec![];

        for (i, building_leaf) in expr.iter().enumerate() {
            match building_leaf {
                Leaf(OpenBracket) => open_brackets.push(i),
                Leaf(CloseBracket) => pos.push((open_brackets.pop().ok_or(SyntaxError)?, i)),
                _ => {}
            }
        }

        if !open_brackets.is_empty() {
            return Err(SyntaxError);
        }

        let pos_clone = pos.clone();
        for (i, (p1, p2)) in pos_clone.iter().enumerate() {
            let mut discount = true;
            pos = pos.iter().enumerate().map(
                |(i2, (mut o, mut c))| {
                    if i2 > i {
                        if p1 < &o && discount {
                            o -= p2 - p1;
                            c -= p2 - p1
                        }
                        else if p1 > &o && discount {
                            c -= p2 - p1;
                            discount = false
                        }
                        (o, c)
                    } else { (o, c) }
                }
            ).collect();
        }

        for (o, c) in pos {
            expr.remove(c);
            expr.remove(o);
            let sub_expr: Vec<ExprTree> = expr.drain(o..(c - 1)).collect();
            expr.insert(o, ExprTree::build(sub_expr)?);
        }

        condense_op(&mut expr, Tier3);
        condense_op(&mut expr, Tier2);
        condense_op(&mut expr, Tier1);

        expr.remove(0).propagate(Self::clean)
    }

    fn parse_expr(expr_str: &str) -> Result<Expr, ExprError> {
        let mut expr = vec![];
        let mut expr_unit = String::new();

        for ch in expr_str.chars() {
            match ch {
                '+' => push_expr_leaf!(expr_unit, expr, ADD),
                '-' => push_expr_leaf!(expr_unit, expr, SUB),
                '*' => push_expr_leaf!(expr_unit, expr, MUL),
                '/' => push_expr_leaf!(expr_unit, expr, DIV),
                '^' => push_expr_leaf!(expr_unit, expr, POW),
                '(' => push_expr_leaf!(expr_unit, expr, OpenBracket),
                ')' => push_expr_leaf!(expr_unit, expr, CloseBracket),
                _ if ch.is_whitespace() => push_expr_leaf(&mut expr_unit, &mut expr, None)?,
                _ => expr_unit.push(ch),
            }
        }
        push_expr_leaf(&mut expr_unit, &mut expr, None)?;

        Ok(expr)
    }

    pub fn make_opr(left: ExprTree, right: ExprTree, op: ExprUnit) -> ExprTree {
        Operation(Box::new(left), Box::new(right), op)
    }

    pub fn unwrap_opr(self) -> (ExprTree, ExprTree, ExprUnit) {
        if let Operation(child1, child2, op) = self {
            (*child1, *child2, op)
        } else { panic!("Called ’ExprTree.unwrap_opr’ in a ’ExprTree::Leaf’ value.") }
    }

    pub fn unwrap_leaf(self) -> ExprUnit {
        if let Leaf(child) = self {
            child
        } else { panic!("Called ’ExprTree.unwrap_leaf’ in a ’ExprTree::Operation’ value.") }
    }

    pub fn is_num(&self) -> bool {
        if let Leaf(Num(_)) = self {
            true
        } else {
            false
        }
    }

    pub fn get_op(&self) -> Option<ExprUnit> {
        if let Operation(_, _, op) = self {
            Some(op.clone())
        } else {
            None
        }
    }

    pub fn propagate(self, key: fn(ExprTree, ExprTree, ExprUnit) -> Result<ExprTree, ExprError>) -> Result<ExprTree, ExprError> {
        match self {
            Operation(child1, child2, op) => {
                let child1 = child1.propagate(key)?;
                let child2 = child2.propagate(key)?;
                key(child1, child2, op)
            },
            Leaf(_) => Ok(self)
        }
    }

    fn organize(child1: ExprTree, child2: ExprTree, op: ExprUnit) -> Result<ExprTree, ExprError> {
        let et = match (child1, child2, op) {
            (c1, Leaf(Num(n)), MUL) => Self::make_opr(Leaf(Num(n)), c1, MUL),
            (c1, Operation(node1, node2, MUL), MUL) if node1.is_num() => {
                let multiplier = Operation(node2, Box::new(c1), MUL);
                Operation(node1, Box::new(multiplier), MUL)
            }
            (Operation(node1, node2, POW), c2, POW) => {
                let pow = Self::make_opr(*node2, c2, MUL);
                Self::make_opr(*node1, pow, POW)
            },
            (c1, Operation(node1, node2, DIV), MUL) => {
                let dividend = Self::make_opr(c1, *node1, MUL);
                Self::make_opr(dividend, *node2, DIV)
            },
            (Operation(node1, node2, DIV), c2, MUL) => {
                let dividend = Self::make_opr(*node1, c2, MUL);
                Self::make_opr(dividend, *node2, DIV)
            }
            (child1, child2, op) => Self::make_opr(child1, child2, op)
        };

        Ok(et)
    }

    fn clean(child1: ExprTree, child2: ExprTree, op: ExprUnit) -> Result<ExprTree, ExprError> {
        let mut et = Self::organize(child1, child2, op)?;

        et = match et.unwrap_opr() {
            // Math errors
            (_, num, DIV) if num == ZERO => Err(MathError)?,
            (num1, num2, POW) if num1 == ZERO && num2 == ZERO => Err(MathError)?,

            // Two numbers operation
            (Leaf(Num(x)), Leaf(Num(y)), Op(t, i)) => operate_nums(x, (t, i), y),

            // Operations with zero
            (node, num, Op(Tier1, _)) if num == ZERO => node,
            (num, node, ADD) if num == ZERO => node,
            (num, _, Op(Tier2 | Tier3, _)) if num == ZERO => ZERO,
            (_, num, Op(Tier3, _)) if num == ZERO => ONE,

            // Operations with one
            (node, num, Op(Tier2, _)) if num == ONE => node,
            (num, node, MUL) if num == ONE => node,
            (node, num, POW) if num == ONE => node,
            (num, _, POW) if num == ONE => num,

            // Negative
            (num1, Operation(num2, c2, SUB), SUB) if num1 == ZERO && *num2 == ZERO => *c2,
            (num1, Operation(c1, c2, MUL), SUB) if num1 == ZERO && c1.is_num() => {
                if let Leaf(Num(n)) = *c1 {
                    let c1 = Box::new(Leaf(Num(-n)));
                    Operation(c1, c2, MUL)
                } else {
                    unreachable!()
                }
            }
            (Operation(num1, c1, SUB), Operation(num2, c2, SUB), Op(Tier2, o))
                if *num1 == ZERO && *num2 == ZERO => Operation(c1, c2, Op(Tier2, o)),
            (Operation(num1, c1, SUB), node, Op(Tier2, o)) if *num1 == ZERO => {
                let right = Self::make_opr(*c1, node, Op(Tier2, o));
                Self::make_opr(ZERO, right, SUB)
            },
            (node, Operation(num2, c2, SUB), Op(Tier2, o)) if *num2 == ZERO => {
                let right = Self::make_opr(node, *c2, Op(Tier2, o));
                Self::make_opr(ZERO, right, SUB)
            },

            // Common common
            (Operation(c11, c12, MUL), Operation(c21, c22, MUL), Op(Tier1, i))
                if c11 == c21 => {
                let multiplier = Operation(c12, c22, Op(Tier1, i));
                Self::make_opr(*c11, multiplier, MUL)
            },
            (Operation(c11, c12, Op(Tier2, i2)), Operation(c21, c22, Op(Tier2, i3)), Op(Tier1, i))
                if c12 == c22 && i2 == i3 => {
                    let value = Operation(c11, c21, Op(Tier1, i));
                    Self::make_opr(value, *c12, Op(Tier2, i2))
            },

            // Any clean possible
            (child1, child2, op) => Self::make_opr(child1, child2, op)
        };

        if let Operation(c1, c2, op) = et {
            Self::organize(*c1, *c2, op)
        } else {
            Ok(et)
        }
    }
}

impl Display for ExprTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Leaf(x) => write!(f, "{}", x),
            Operation(x, y, z) => write!(f, "{} {} {}", x, y, z)
        }
    }
}

impl Default for ExprTree {
    fn default() -> Self {
        Leaf(Num(0.0))
    }
}

impl PartialEq for ExprTree {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Leaf(eu1), Leaf(eu2)) if eu1 == eu2 => true,
            (Operation(ll, lr, ADD), Operation(rl, rr, ADD)) => {
                (ll == rl && lr == rr) || (ll == rr && lr == rl)
            },
            (Operation(ll, lr, MUL), Operation(rl, rr, MUL)) => {
                (ll == rl && lr == rr) || (ll == rr && lr == rl)
            },
            (Operation(ll, lr, op1), Operation(rl, rr, op2)) => {
                (ll == rl && lr == rr) && op1 == op2
            },
            _ => false
        }
    }
    
    fn ne(&self, other: &Self) -> bool {
        !(self == other)
    }
}

pub fn apply_simplifications(mut child1: ExprTree, mut child2: ExprTree, mut op: ExprUnit) -> Result<ExprTree, ExprError> {
    let mut changed = true;

    while changed {
        changed = shorten_expr(&mut child1, &mut child2, op.clone(), true, true);
        let cleaned = child1.propagate(ExprTree::clean)?;
        if let Operation(c1, c2, new_op) = cleaned {
            (child1, child2, op) = (*c1, *c2, new_op);
        } else {
            return Ok(cleaned);
        }
    };

    Ok(ExprTree::make_opr(child1, child2, op))
}

#[derive(Clone)]
struct ExprOper {
    left: ExprTree,
    right: ExprTree,
    op: (Tier, bool),
    is_default: bool,
}

impl ExprOper {
    fn new(left: ExprTree, right: ExprTree, op: (Tier, bool), is_default: bool) -> ExprOper {
        ExprOper {left, right, op, is_default}
    }

    fn as_expr_tree(self) -> ExprTree {
        if self.is_default {
            self.left
        } else { Operation(Box::new(self.left), Box::new(self.right), Op(self.op.0, self.op.1)) }
    }
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

fn distributive(left: &mut ExprOper, right: &mut ExprTree, op: ExprUnit, can_move: bool) -> bool {
    let changed1 = shorten_expr(&mut left.left, right, op.clone(), false, false);
    let changed2 = shorten_expr(&mut left.right, right, op.clone(), can_move, changed1);

    if !(changed1 || changed2) {
        return false;
    } else if !changed1 {
        left.left = make_opr_from_ref(&mut left.left, right, op, can_move);
    }
    true
}

fn match_equivalent(left: &mut ExprOper, right: &mut ExprOper, op: ExprUnit, invert: bool, can_move: bool) -> bool {
    let (a, mut b) = (&mut left.left, &mut left.right);
    let (mut x, mut y) = (&mut right.left, &mut right.right);

    let (mut r_left_incr, mut r_right_incr) = (true, right.op.1);
    let mut can_move2 = can_move;

    if invert {
        (x, y) = (y, x);
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

    let changed1 = shorten_expr(a, x, op1.clone(), can_move, false);
    let changed2 = shorten_expr(b, y, op2.clone(), can_move2, changed1);
    
    if !(changed1 || changed2) {
        return false;
    } else if !changed1 {
        *a = make_opr_from_ref(a, x, op1, can_move);
    }

    let (mut a, mut b) = (take(a), take(b));
    let final_op = Op(main_op_info.0, final_op_incr);

    if (!left.op.1 && !main_op_info.1) && right.op.1 {
        if shorten_expr(&mut b, &mut a, final_op.clone(), true, false) {
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
    true
}

fn common_factor(left: &mut ExprOper, right: &mut ExprOper, op: ExprUnit,
                 invert: bool, can_move: bool, can_simp: bool) -> bool {    
    let (a, b) = (&mut left.left, &mut left.right);
    let (mut x, mut y) = (&mut right.left, &mut right.right);

    if invert {
        (x, y) = (y, x);
    }

    if a == x && left.op.1 {
        shorten_expr(b, y, op, can_move, true);
        true
    } else if a == x {
        if let (Leaf(Num(n1)), Leaf(Num(n2))) = (b, y) {
            let sum = operate_nums(*n1, op.op_as_tuple(), *n2);
            let dividend = ExprTree::make_opr(sum, take(a), MUL);
            let divisor = operate_nums(*n1, (Tier2, true), *n2);

            left.left = dividend;
            left.right = divisor;

            true
        } else {
            false
        }
    } else if (b == y && left.op.0 == Tier2) && can_simp {
        shorten_expr(a, x, op, can_move, true);
        true
    } else {
        false
    }
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
            *left = ExprTree::make_opr(Leaf(Num(*n1)), Leaf(Num(*n2)), op);
            return Some(true);
        },
        (Leaf(Unk(_)), Leaf(Num(_))) | (Leaf(Num(_)), Leaf(Unk(_))) => {
            if ensure_change {
                *left = make_opr_from_ref(left, right, op, can_move);
            }
            return Some(false);
        },
        (l, r) if l == r => {
            let new_op = get_next_op(op.clone(), true);

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
                    op: ExprUnit, can_move: bool, ensure_change: bool) -> bool {
    if let Some(changed) = shorten_expr_early(orig_left, orig_right, op.clone(), can_move, ensure_change){
        return changed;
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
    let mut can_simp = !left.is_default;

    let changed = match tuple_op.0.get_num() - left.op.0.get_num() {
        1 => {
            let will_remove = if op == Op(Tier2, false) {
                seek_multiplier(&left.left, orig_right) || seek_multiplier(&left.right, orig_right)
            } else if op == Op(Tier2, true) {
                seek_divisor(&left.left, orig_right) || seek_divisor(&left.right, orig_right)
            } else { true };

            if will_remove && can_simp {
                distributive(&mut left, orig_right, op.clone(), can_move)
            } else { false }
        }
        0 => {
            if let Tier3 = left.op.0 {
                false
            } else {
                let left_op = Op(left.op.0, left.op.1);
                let right_copy = take_or_clone(orig_right, can_move);
                right = get_expr_opr_by_op(right_copy, left_op, true);
                recover_right = true;

                let changed = match_equivalent(&mut left, &mut right, op.clone(), false, can_move);

                if !changed {
                    match_equivalent(&mut left, &mut right, op.clone(), true, can_move)
                } else { changed }
            }
        }
        -1 => {
            let left_op = Op(left.op.0, left.op.1);
            let right_copy = take_or_clone(orig_right, can_move);
            right = get_expr_opr_by_op(right_copy, left_op, true);
            recover_right = true;
            let op = Op(Tier1, tuple_op.1);

            can_simp = can_simp || !right.is_default;
            let changed = common_factor(&mut left, &mut right, op.clone(), false, can_move, can_simp);

            let left_is_symmetric = left.left == left.right;
            let right_is_symmetric = right.left == right.right;
            let some_is_symmetric = left_is_symmetric || right_is_symmetric;

            if (!changed && left.op == MUL.op_as_tuple()) && !some_is_symmetric {
                common_factor(&mut left, &mut right, op, true, can_move, true)
            } else { changed }
        }
        _ => {
            false
        }
    };
    
    left.is_default = left.is_default && !changed;
    let left = left.as_expr_tree();
    if recover_right && can_move {
        *orig_right = right.as_expr_tree();
    }
    if !changed && ensure_change {
        *orig_left = ExprTree::make_opr(left, take_or_clone(orig_right, can_move), op);
    } else {
        *orig_left = left;
    }
    changed
}
