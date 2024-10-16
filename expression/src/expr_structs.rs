use std::fmt::Debug;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use api::expr_tree_to_infix;
use traits::Pow;

use super::*;
use super::error::ExprError::*;
use super::macros::*;

#[derive(PartialEq, Debug, Clone)]
pub enum ExprUnit {
    Num(f64),
    Unk(String),
    // Tier 1 = add/sub; Tier 2 = mul/div; Tier 3 = pow/root;
    // bool = increases or not
    Op(Tier, bool),
    // For scientific notation
    E,
    OpenBracket,
    CloseBracket,
}

impl ExprUnit {
    pub fn unwrap_op(self) -> (Tier, bool) {
        match self {
            Op(tier, incr) => (tier, incr),
            _ => panic!("Called unwrap_op method on an non-Op ExprUnit.")
        }
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
            &ROOT => unreachable!(),
            E => write!(f, "E")
        }
    }
}

fn push_expr_leaf(building_leaf: &mut String, expr: &mut Expr,
                  new: Option<ExprTree>, expr_str: &str) -> ExprResult<()> {
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
        (Some(Leaf(Op(Tier2 | Tier3, _))), Some(Leaf(Op(_, _))) | None) => return Err(SyntaxError(expr_str.to_string())),
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
            }
            Leaf(Op(t, _)) if *t == tier => {
                pos.push((i - 1, false));
                last_is_target = true
            }
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

fn expr_to_string(expr: &Expr) -> String {
    expr.into_iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ")
}

#[derive(Debug, Clone)]
pub enum ExprTree {
    Operation(Box<ExprTree>, Box<ExprTree>, ExprUnit),
    Leaf(ExprUnit),
}

type SciNotationKey<E> = fn(ExprTree, ExprTree) -> Result<ExprTree, E>;
type OperKey<E> = fn(ExprTree, ExprTree, ExprUnit) -> Result<ExprTree, E>;
type LeafKey<E> = fn(ExprTree) -> Result<ExprTree, E>;

impl ExprTree {
    pub fn new(expr_str: &str) -> ExprTreeResult {
        let tree = Self::raw_new(expr_str)?;
        tree.propagate(Self::clean, number_to_sci_notation)
    }

    pub fn raw_new(expr_str: &str) -> ExprTreeResult {
        let expr = Self::parse_expr(expr_str)?;
        Self::build(expr)
    }

    fn build(mut expr: Expr) -> ExprTreeResult {
        let mut pos = vec![];
        let mut open_brackets = vec![];

        for (i, building_leaf) in expr.iter().enumerate() {
            match building_leaf {
                Leaf(OpenBracket) => open_brackets.push(i),
                Leaf(CloseBracket) => pos.push((open_brackets.pop().ok_or(SyntaxError(expr_to_string(&expr)))?, i)),
                _ => {}
            }
        }

        if !open_brackets.is_empty() {
            return Err(SyntaxError(expr_to_string(&expr)));
        }

        let mut pos_clone = pos.clone();
        for i in 0..pos.len() {
            let (p1, p2) = pos_clone[i];
            let mut discount = true;
            let mut fixed_part = pos.drain((i + 1)..pos.len()).into_iter().enumerate().map(
                |(i2, (mut o, mut c))| {
                    if discount {
                        if p1 < o {
                            o -= p2 - p1;
                            c -= p2 - p1;
                            pos_clone[i + 1 + i2] = (o, c);
                        } else if p1 > o {
                            c -= p2 - p1;
                            discount = false
                        }
                    }
                    (o, c)
                }
            ).collect::<Vec<(usize, usize)>>();
            pos.append(&mut fixed_part);
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

        if expr.is_empty() {
            Ok(Leaf(Unk("".to_string())))
        } else {
            Ok(expr.remove(0))
        }
    }

    fn parse_expr(expr_str: &str) -> ExprResult<Expr> {
        let mut expr = vec![];
        let mut expr_unit = String::new();

        for ch in expr_str.chars() {
            match ch {
                '+' => push_expr_leaf!(expr_unit, expr, ADD, expr_str),
                '-' => push_expr_leaf!(expr_unit, expr, SUB, expr_str),
                '*' => push_expr_leaf!(expr_unit, expr, MUL, expr_str),
                '/' => push_expr_leaf!(expr_unit, expr, DIV, expr_str),
                '^' => push_expr_leaf!(expr_unit, expr, POW, expr_str),
                '(' => push_expr_leaf!(expr_unit, expr, OpenBracket, expr_str),
                ')' => push_expr_leaf!(expr_unit, expr, CloseBracket, expr_str),
                _ if ch.is_whitespace() => push_expr_leaf(&mut expr_unit, &mut expr, None, expr_str)?,
                _ => expr_unit.push(ch),
            }
        }
        push_expr_leaf(&mut expr_unit, &mut expr, None, expr_str)?;

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

    pub fn unwrap_num(self) -> f64 {
        if let Leaf(Num(num)) = self {
            num
        } else { panic!("Called ’ExprTree.unwrap_num’ in a value different from ’ExprTree::Leaf(ExprUnit::Num)’.") }
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

    pub fn propagate_sci_notation<E>(
        self,
        sci_notation_key: SciNotationKey<E>,
        oper_key: OperKey<E>,
        leaf_key: LeafKey<E>,
    ) -> Result<ExprTree, E> {
        match self {
            Operation(child1, child2, E) => {
                let child1 = child1.propagate_sci_notation(sci_notation_key, oper_key, leaf_key)?;
                let child2 = child2.propagate_sci_notation(sci_notation_key, oper_key, leaf_key)?;
                sci_notation_key(child1, child2)
            }
            Operation(child1, child2, op) => {
                let child1 = child1.propagate_sci_notation(sci_notation_key, oper_key, leaf_key)?;
                let child2 = child2.propagate_sci_notation(sci_notation_key, oper_key, leaf_key)?;
                oper_key(child1, child2, op)
            }
            leaf => leaf_key(leaf)
        }
    }

    pub fn propagate<E>(self, oper_key: OperKey<E>, leaf_key: LeafKey<E>) -> Result<ExprTree, E> {
        self.propagate_sci_notation(
            |c1, c2| Ok(Self::make_opr(c1, c2, E)),
            oper_key,
            leaf_key,
        )
    }

    pub fn organize(child1: ExprTree, child2: ExprTree, op: ExprUnit) -> ExprTreeResult {
        let et = match (child1, child2, op) {
            (c1, Leaf(Num(n)), MUL) => Self::make_opr(Leaf(Num(n)), c1, MUL),
            (c1, Operation(node1, node2, MUL), MUL) if node1.is_num() => {
                let multiplier = Self::clean(*node2, c1, MUL)?;
                Self::make_opr(*node1, multiplier, MUL)
            }
            (Operation(node1, node2, op), Leaf(c2), MUL) => {
                Self::make_opr(Leaf(c2), Operation(node1, node2, op), MUL)
            }
            (Operation(node1, node2, POW), c2, POW) => {
                let pow = Self::clean(*node2, c2, MUL)?;
                Self::make_opr(*node1, pow, POW)
            }
            (c1, Operation(node1, node2, DIV), MUL) => {
                let factor = Self::clean(c1, *node1, MUL)?;
                Self::make_opr(factor, *node2, DIV)
            }
            (Operation(node1, node2, DIV), c2, MUL) => {
                let factor = Self::clean(*node1, c2, MUL)?;
                Self::make_opr(factor, *node2, DIV)
            }
            (Operation(node1, node2, DIV), c2, DIV) => {
                let factor = Self::clean(*node2, c2, MUL)?;
                Self::make_opr(*node1, factor, DIV)
            }
            (c1, c2, Op(Tier3, i)) => {
                Self::make_opr(sci_notation_to_number(c1), sci_notation_to_number(c2), Op(Tier3, i))
            }
            (child1, child2, op) => Self::make_opr(child1, child2, op)
        };

        Ok(et)
    }

    pub fn clean(child1: ExprTree, child2: ExprTree, op: ExprUnit) -> ExprTreeResult {
        let mut et = Self::organize(child1, child2, op)?;

        et = match et.unwrap_opr() {
            // Math errors
            (node, num, DIV) if num == ZERO => {
                let tree = Self::make_opr(node, num, DIV);

                Err(MathError(expr_tree_to_infix(&tree, " ")))?
            }
            (num1, num2, POW) if num1 == ZERO && num2 == ZERO => {
                let tree = Self::make_opr(num1, num2, POW);

                Err(MathError(expr_tree_to_infix(&tree, " ")))?
            }

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
            }
            (node, Operation(num2, c2, SUB), Op(Tier2, o)) if *num2 == ZERO => {
                let right = Self::make_opr(node, *c2, Op(Tier2, o));
                Self::make_opr(ZERO, right, SUB)
            }

            // Common factor
            (Operation(c11, c12, MUL), Operation(c21, c22, MUL), Op(Tier1, i))
            if c11 == c21 => {
                let multiplier = Operation(c12, c22, Op(Tier1, i));
                Self::make_opr(*c11, multiplier, MUL)
            }
            (Operation(c11, c12, Op(Tier2, i2)), Operation(c21, c22, Op(Tier2, i3)), Op(Tier1, i))
            if c12 == c22 && i2 == i3 => {
                let value = Operation(c11, c21, Op(Tier1, i));
                Self::make_opr(value, *c12, Op(Tier2, i2))
            }

            // Two numbers operation
            (Operation(man1, exp1, E), Leaf(Num(num)), Op(t, i)) => {
                let (man2, exp2) = take_sci_notation_number(num);
                operate_sci_notation_nums(
                    man1.unwrap_num(), exp1.unwrap_num(),
                    man2, exp2,
                    (t, i)
                )
            }
            (Leaf(Num(num)), Operation(man2, exp2, E), Op(t, i)) => {
                let (man1, exp1) = take_sci_notation_number(num);
                operate_sci_notation_nums(
                    man1, exp1,
                    man2.unwrap_num(), exp2.unwrap_num(),
                    (t, i)
                )
            }
            (Operation(man1, exp1, E), Operation(man2, exp2, E), Op(t, i)) => {
                operate_sci_notation_nums(
                    man1.unwrap_num(), exp1.unwrap_num(),
                    man2.unwrap_num(), exp2.unwrap_num(),
                    (t, i)
                )
            }
            (Leaf(Num(x)), Leaf(Num(y)), Op(t, i)) => operate_nums(x, y, (t, i)),

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
            Operation(x, y, E) => write!(f, "{} 10 {} ^ *", x, y),
            Operation(x, y, ROOT) => write!(f, "{} 1 {} / ^", x, y),
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
            }
            (Operation(ll, lr, MUL), Operation(rl, rr, MUL)) => {
                (ll == rl && lr == rr) || (ll == rr && lr == rl)
            }
            (Operation(ll, lr, op1), Operation(rl, rr, op2)) => {
                (ll == rl && lr == rr) && op1 == op2
            }
            _ => false
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !(self == other)
    }
}

impl Neg for ExprTree {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::clean(ZERO, self, SUB).unwrap()
    }
}

impl Pow for ExprTree {
    type Output = Self;

    fn pow(self, other: Self) -> Self::Output {
        let tree = Self::make_opr(self, other, POW);
        let tree = tree.propagate(apply_simplifications, |x| Ok(x));
        tree.unwrap_or_else(|err| panic!("{}", err))
    }
}

impl<T: Into<f64>> Pow<T> for ExprTree {
    type Output = Self;

    fn pow(self, other: T) -> Self::Output {
        let tree = Self::make_opr(self, Leaf(Num(other.into())), POW);
        let tree = tree.propagate(apply_simplifications, |x| Ok(x));
        tree.unwrap_or_else(|err| panic!("{}", err))
    }
}

impl UpperExp for ExprTree {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Leaf(Num(x)) => write!(f, "{:E}", x),
            Leaf(x) => write!(f, "{}", x),
            Operation(x, y, ROOT) => write!(f, "{:E} 1 {:E} / ^", **x, **y),
            Operation(x, y, z) => write!(f, "{:E} {:E} {}", **x, **y, z)
        }
    }
}

impl LowerExp for ExprTree {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Leaf(Num(x)) => write!(f, "{:e}", x),
            Leaf(x) => write!(f, "{}", x),
            Operation(x, y, ROOT) => write!(f, "{:e} 1 {:e} / ^", **x, **y),
            Operation(x, y, z) => write!(f, "{:e} {:e} {}", **x, **y, z)
        }
    }
}

#[derive(Clone, Default, PartialEq)]
pub struct ExprOper {
    pub left: ExprTree,
    pub right: ExprTree,
    pub op: (Tier, bool),
    pub is_default: bool,
}

impl ExprOper {
    pub fn new(left: ExprTree, right: ExprTree, op: (Tier, bool), is_default: bool) -> ExprOper {
        ExprOper { left, right, op, is_default }
    }

    pub fn from(tree: ExprTree) -> ExprOper {
        get_expr_opr_by_op(tree, MUL, false)
    }

    pub fn as_expr_tree(self) -> ExprTree {
        if self.is_default {
            self.left
        } else { Operation(Box::new(self.left), Box::new(self.right), tuple_to_op(self.op)) }
    }
}

impl Display for ExprOper {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}",
               expr_tree_to_infix(&self.left, " "),
               tuple_to_op(self.op),
               expr_tree_to_infix(&self.right, " ")
        )
    }
}

impl Debug for ExprOper {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", &self.left, &self.right, tuple_to_op(self.op))
    }
}

impl_ops!(Add, add, ADD; Sub, sub, SUB; Mul, mul, MUL; Div, div, DIV);
impl_ops_num!(Add, add, ADD; Sub, sub, SUB; Mul, mul, MUL; Div, div, DIV);

impl_ops_asn!(AddAssign, add_assign, ADD; SubAssign, sub_assign, SUB;
              MulAssign, mul_assign, MUL; DivAssign, div_assign, DIV);
impl_ops_asn_num!(AddAssign, add_assign, ADD; SubAssign, sub_assign, SUB;
                  MulAssign, mul_assign, MUL; DivAssign, div_assign, DIV);
