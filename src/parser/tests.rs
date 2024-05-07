use std::rc::Rc;

use super::ast::{
    BinaryOpKind::*,
    Expr::{self, *},
    UnaryOpKind::*,
};

#[allow(clippy::unnecessary_box_returns)]
fn p(x: Expr) -> super::ast::PExpr {
    x.into()
}
type Res = Result<(), super::Error>;

//noinspection RsLiveness (rustrover bug doesnt realize that panic fmt uses error)
#[allow(clippy::needless_pass_by_value)]
fn test_parse(s: &str, expected: Expr) -> Result<(), super::Error> {
    assert_eq!(expected, s.parse()?);
    Ok(())
}

#[test]
fn one_plus_one() -> Res {
    test_parse("1+1", BinaryOp(p(Number(1.)), Add, p(Number(1.))))
}

#[test]
fn two_plus_four_pow_three_div_five() -> Res {
    test_parse(
        "     (2+ 4) * * (3  ÷5)",
        BinaryOp(
            p(BinaryOp(p(Number(2.)), Add, p(Number(4.)))),
            Power,
            p(BinaryOp(p(Number(3.)), Divide, p(Number(5.)))),
        ),
    )
}

#[test]
fn neg_whole_two_div_whole_one_plus_one() -> Res {
    test_parse(
        "-(2/(1+1))",
        UnaryOp(
            Neg,
            p(BinaryOp(
                p(Number(2.)),
                Divide,
                p(BinaryOp(p(Number(1.)), Add, p(Number(1.)))),
            )),
        ),
    )
}

#[test]
fn neg_one_whole_squared() -> Res {
    test_parse(
        "(-1)**2",
        BinaryOp(p(UnaryOp(Neg, p(Number(1.)))), Power, p(Number(2.))),
    )
}

#[test]
fn five_plus_exp_sin_2_times_3_over_four() -> Res {
    test_parse(
        "5+exp(sin(2*3)/4)",
        BinaryOp(
            p(Number(5.)),
            Add,
            p(Call(
                Id("exp".into()).into(),
                vec![BinaryOp(
                    p(Call(
                        Id("sin".into()).into(),
                        vec![BinaryOp(p(Number(2.)), Multiply, p(Number(3.))).into()],
                    )),
                    Divide,
                    p(Number(4.)),
                )
                .into()],
            )),
        ),
    )
}

#[test]
fn max_one_two() -> Res {
    test_parse(
        "max(1,2)",
        Call(
            Id("max".into()).into(),
            vec![Number(1.).into(), Number(2.).into()],
        ),
    )
}

#[test]
fn x_eq_three() -> Res {
    test_parse("x=3", VarAssign("x".into(), p(Number(3.))))
}

#[test]
fn f_of_x_eq_two_times_x() -> Res {
    test_parse(
        "f(x)=2*x",
        VarAssign(
            "f".into(),
            FnDef(
                ["x".into()].into(),
                Rc::new(BinaryOp(p(Number(2.)), Multiply, p(Id("x".into())))),
            )
            .into(),
        ),
    )
}
