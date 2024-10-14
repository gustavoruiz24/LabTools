use super::*;

fn child_to_infix(child: &ExprTree, root_op: (Tier, bool), is_left: bool, sep: &str) -> String {
    if let Some(Op(tier, i)) = child.get_op() {
        if root_op.0 > tier || ((root_op.0 == tier && (!root_op.1 && i)) && !is_left) {
            return format!("({})", expr_tree_to_infix(child, sep));
        }
    }
    format!("{}", expr_tree_to_infix(child, sep))
}

pub fn expr_tree_to_infix(expr_tree: &ExprTree, sep: &str) -> String {
    if let Operation(child1, child2, Op(tier, i)) = expr_tree {
        let mut result = child_to_infix(child1, (*tier, *i), true, sep) + sep;
        result += &(format!("{}", Op(*tier, *i)) + sep);
        result += &child_to_infix(child2, (*tier, *i), false, sep);
        format!("{}", result)
    } else { format!("{}", expr_tree) }
}

pub fn simplify_expression(expression: &str, sep: &str) -> ExprResult<String> {
    let expr_tree = ExprTree::new(expression)?.propagate(apply_simplifications, |x| Ok(x))?;
    Ok(expr_tree_to_infix(&expr_tree, sep))
}
