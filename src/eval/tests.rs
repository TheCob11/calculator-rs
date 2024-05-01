use super::*;

type Res = Result<(), Error>;

#[allow(clippy::float_cmp)]
fn test_eval(expr: &Expr, val: Number) -> Res {
    let mut ctx = Context::new();
    assert_eq!(ctx.eval(expr)?, val);
    Ok(())
}

#[allow(clippy::float_cmp)]
fn test_eval_both_sides(lhs: &Expr, rhs: &Expr) -> Res {
    let mut ctx = Context::new();
    let val_lhs = ctx.eval(lhs)?;
    let val_rhs = ctx.eval(rhs)?;
    assert_eq!(val_lhs, val_rhs);
    Ok(())
}

#[allow(clippy::float_cmp)]
fn test_eval_ctx(ctx: &mut Context, expr: &Expr, val: Number) -> Res {
    assert_eq!(ctx.eval(expr)?, val);
    Ok(())
}

#[test]
fn one_plus_one_eq_two() -> Res {
    test_eval(
        &Expr::BinaryOp(
            Box::new(Expr::Number(1.)),
            BinaryOpKind::Add,
            Box::new(Expr::Number(1.)),
        ),
        2.,
    )
}

#[test]
fn two_times_four_eq_eight() -> Res {
    test_eval(
        &Expr::BinaryOp(
            Box::new(Expr::Number(2.)),
            BinaryOpKind::Multiply,
            Box::new(Expr::Number(4.)),
        ),
        8.,
    )
}

fn bin_op_numbers(lhs: Number, kind: BinaryOpKind, rhs: Number) -> Expr {
    Expr::BinaryOp(
        Box::new(Expr::Number(lhs)),
        kind,
        Box::new(Expr::Number(rhs)),
    )
}

#[test]
fn two_times_neg_three_eq_five_times_two_minus_eight_times_two() -> Res {
    test_eval_both_sides(
        &bin_op_numbers(2., BinaryOpKind::Multiply, -3.),
        &Expr::BinaryOp(
            Box::new(bin_op_numbers(5., BinaryOpKind::Multiply, 2.)),
            BinaryOpKind::Subtract,
            Box::new(bin_op_numbers(8., BinaryOpKind::Multiply, 2.)),
        ),
    )
}

#[test]
fn divide_by_zero() -> Res {
    test_eval(
        &bin_op_numbers(5., BinaryOpKind::Divide, 0.),
        Number::INFINITY,
    )
}

#[test]
fn x_eq_three_minus_e() -> Res {
    let mut ctx = Context::new();
    test_eval_ctx(
        &mut ctx,
        &Expr::VarAssign(
            "x".into(),
            Box::new(Expr::BinaryOp(
                Box::new(Expr::Number(3.)),
                BinaryOpKind::Multiply,
                Box::new(Expr::Number(2.)),
            )),
        ),
        3. * 2.,
    )?;
    test_eval_ctx(&mut ctx, &Expr::Id("x".into()), 3. * 2.)
}

#[test]
fn custom_func_add_one() -> Res {
    let mut ctx = Context::new();
    let add_one_id: Identifier = "add_one".into();
    let x_id: Identifier = "x".into();
    test_eval_ctx(
        &mut ctx,
        &Expr::FnDef(
            add_one_id.clone(),
            Rc::new([x_id.clone()]),
            Rc::new(Expr::BinaryOp(
                Box::new(Expr::Id(x_id)),
                BinaryOpKind::Add,
                Box::new(Expr::Number(1.)),
            )),
        ),
        1.,
    )?;
    let call_expr = Expr::Call(add_one_id, vec![Expr::Number(4.)]);
    test_eval_ctx(&mut ctx, &call_expr, 5.)
}
