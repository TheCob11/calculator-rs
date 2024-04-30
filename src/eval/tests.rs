use super::*;

#[allow(clippy::float_cmp)]
fn test_eval(expr: Expr, val: Number) {
    let mut ctx = Context::new();
    assert_eq!(ctx.eval(&expr).unwrap_or_else(|err| panic!("{err}")), val);
}

#[allow(clippy::float_cmp)]
fn test_eval_both_sides(lhs: Expr, rhs: Expr) {
    let mut ctx = Context::new();
    let val_lhs = ctx.eval(&lhs).unwrap_or_else(|err| panic!("{err}"));
    let val_rhs = ctx.eval(&rhs).unwrap_or_else(|err| panic!("{err}"));
    assert_eq!(val_lhs, val_rhs);
}

#[allow(clippy::float_cmp)]
fn test_eval_ctx(ctx: &mut Context, expr: Expr, val: Number) {
    assert_eq!(ctx.eval(&expr).unwrap_or_else(|err| panic!("{err}")), val);
}

#[test]
fn one_plus_one_eq_two() {
    test_eval(
        Expr::BinaryOp(
            Box::new(Expr::Number(1.)),
            BinaryOpKind::Add,
            Box::new(Expr::Number(1.)),
        ),
        2.,
    );
}

#[test]
fn two_times_four_eq_eight() {
    test_eval(
        Expr::BinaryOp(
            Box::new(Expr::Number(2.)),
            BinaryOpKind::Multiply,
            Box::new(Expr::Number(4.)),
        ),
        8.,
    );
}

fn bin_op_numbers(lhs: Number, kind: BinaryOpKind, rhs: Number) -> Expr {
    Expr::BinaryOp(
        Box::new(Expr::Number(lhs)),
        kind,
        Box::new(Expr::Number(rhs)),
    )
}

#[test]
fn two_times_neg_three_eq_five_times_two_minus_eight_times_two() {
    test_eval_both_sides(
        bin_op_numbers(2., BinaryOpKind::Multiply, -3.),
        Expr::BinaryOp(
            Box::new(bin_op_numbers(5., BinaryOpKind::Multiply, 2.)),
            BinaryOpKind::Subtract,
            Box::new(bin_op_numbers(8., BinaryOpKind::Multiply, 2.)),
        ),
    );
}

#[test]
fn divide_by_zero() {
    test_eval(
        bin_op_numbers(5., BinaryOpKind::Divide, 0.),
        Number::INFINITY,
    );
}

#[test]
fn custom_func() {
    let mut ctx = Context::new();
    let add_one_id: Identifier = "add_one".into();
    let x_id: Identifier = "x".into();
    ctx.fns.insert(
        add_one_id.clone(),
        UserFunction {
            arg_names: vec![x_id.clone()],
            body: Expr::BinaryOp(
                Box::new(Expr::Id(x_id)),
                BinaryOpKind::Add,
                Box::new(Expr::Number(1.)),
            ),
        },
    );
    let call_expr = Expr::Call(add_one_id, vec![Expr::Number(4.)]);
    test_eval_ctx(&mut ctx, call_expr, 5.);
}
