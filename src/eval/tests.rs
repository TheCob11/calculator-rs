use Expr::*;

use super::*;

type Res = Result<(), Error>;

#[allow(clippy::float_cmp)]
fn test_eval(expr: &Expr, val: impl Into<Value>) -> Res {
    let mut ctx = Context::new();
    assert_eq!(ctx.eval(expr)?, val.into());
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
fn test_eval_ctx(ctx: &mut Context, expr: &Expr, val: impl Into<Value>) -> Res {
    assert_eq!(ctx.eval(expr)?, val.into());
    Ok(())
}

#[test]
fn one_plus_one_eq_two() -> Res {
    test_eval(
        &BinaryOp(Num(1.).into(), BinaryOpKind::Add, Num(1.).into()),
        2.,
    )
}

#[test]
fn two_times_four_eq_eight() -> Res {
    test_eval(
        &BinaryOp(Num(2.).into(), BinaryOpKind::Multiply, Num(4.).into()),
        8.,
    )
}

fn bin_op_numbers(lhs: super::Number, kind: BinaryOpKind, rhs: super::Number) -> Expr {
    BinaryOp(Num(lhs).into(), kind, Num(rhs).into())
}

#[test]
fn two_times_neg_three_eq_five_times_two_minus_eight_times_two() -> Res {
    test_eval_both_sides(
        &bin_op_numbers(2., BinaryOpKind::Multiply, -3.),
        &BinaryOp(
            bin_op_numbers(5., BinaryOpKind::Multiply, 2.).into(),
            BinaryOpKind::Subtract,
            bin_op_numbers(8., BinaryOpKind::Multiply, 2.).into(),
        ),
    )
}

#[test]
fn divide_by_zero() -> Res {
    test_eval(
        &bin_op_numbers(5., BinaryOpKind::Divide, 0.),
        super::Number::INFINITY,
    )
}

#[test]
fn x_eq_three_minus_e() -> Res {
    let mut ctx = Context::new();
    test_eval_ctx(
        &mut ctx,
        &VarAssign(
            "x".into(),
            BinaryOp(Num(3.).into(), BinaryOpKind::Multiply, Num(2.).into()).into(),
        ),
        3. * 2.,
    )?;
    test_eval_ctx(&mut ctx, &Id("x".into()), 3. * 2.)
}

#[test]
fn custom_func_add_one() -> Res {
    let mut ctx = Context::new();
    let add_one_id: Identifier = "add_one".into();
    let x_id: Identifier = "x".into();
    let add_one_args = vec![x_id.clone()];
    let add_one_body: PExpr = BinaryOp(Id(x_id).into(), BinaryOpKind::Add, Num(1.).into()).into();
    test_eval_ctx(
        &mut ctx,
        &VarAssign(
            add_one_id.clone(),
            FnDef(add_one_args.clone(), add_one_body.clone()).into(),
        ),
        Value::Func(UserFunction {
            arg_names: add_one_args,
            body: add_one_body,
        }),
    )?;
    let call_expr = Call(Id(add_one_id).into(), vec![Num(4.).into()]);
    test_eval_ctx(&mut ctx, &call_expr, 5.)
}

#[test]
fn one_plus_custom_func_add_one() -> Res {
    let mut ctx = Context::new();
    let add_one_id: Identifier = "add_one".into();
    let x_id: Identifier = "x".into();
    let arg_names = vec![x_id.clone()];
    let add_one_body: PExpr = BinaryOp(Id(x_id).into(), BinaryOpKind::Add, Num(1.).into()).into();
    ctx.eval(&VarAssign(
        add_one_id.clone(),
        FnDef(arg_names.clone(), add_one_body.clone()).into(),
    ))?;
    test_eval_ctx(
        &mut ctx,
        &BinaryOp(Num(1.).into(), BinaryOpKind::Add, Id(add_one_id).into()),
        Value::Func(UserFunction {
            arg_names,
            body: BinaryOp(Num(1.).into(), BinaryOpKind::Add, add_one_body).into(),
        }),
    )
}
