use std::rc::Rc;

use super::ast::{
    BinaryOpKind::*,
    Expr::{self, *},
    UnaryOpKind::*,
};

#[allow(clippy::unnecessary_box_returns)]
fn b<T>(x: T) -> Box<T> {
    Box::new(x)
}

//noinspection RsLiveness (rustrover bug doesnt realize that panic fmt uses error)
#[allow(clippy::needless_pass_by_value)]
fn test_parse(s: &str, expected: Expr) {
    match s.parse::<Expr>() {
        Ok(expr) => assert_eq!(expr, expected),
        Err(e) => panic!("{e}"),
    }
}

#[test]
fn one_plus_one() {
    test_parse("1+1", BinaryOp(b(Number(1.)), Add, b(Number(1.))));
}

#[test]
fn two_plus_four_pow_three_div_five() {
    test_parse(
        "     (2+ 4) * * (3  ÷5)",
        BinaryOp(
            b(BinaryOp(b(Number(2.)), Add, b(Number(4.)))),
            Power,
            b(BinaryOp(b(Number(3.)), Divide, b(Number(5.)))),
        ),
    );
}

#[test]
fn neg_whole_two_div_whole_one_plus_one() {
    test_parse(
        "-(2/(1+1))",
        UnaryOp(
            Neg,
            b(BinaryOp(
                b(Number(2.)),
                Divide,
                b(BinaryOp(b(Number(1.)), Add, b(Number(1.)))),
            )),
        ),
    );
}

#[test]
fn neg_one_whole_squared() {
    test_parse(
        "(-1)**2",
        BinaryOp(b(UnaryOp(Neg, b(Number(1.)))), Power, b(Number(2.))),
    );
}

#[test]
fn five_plus_exp_sin_2_times_3_over_four() {
    test_parse(
        "5+exp(sin(2*3)/4)",
        BinaryOp(
            b(Number(5.)),
            Add,
            b(Call(
                "exp".into(),
                vec![BinaryOp(
                    b(Call(
                        "sin".into(),
                        vec![BinaryOp(b(Number(2.)), Multiply, b(Number(3.)))],
                    )),
                    Divide,
                    b(Number(4.)),
                )],
            )),
        ),
    );
}

#[test]
fn max_one_two() {
    test_parse("max(1,2)", Call("max".into(), vec![Number(1.), Number(2.)]));
}

#[test]
fn x_eq_three() {
    test_parse("x=3", VarAssign("x".into(), b(Number(3.))));
}

#[test]
fn f_of_x_eq_two_times_x() {
    test_parse(
        "f(x)=2*x",
        FnDef(
            "f".into(),
            Rc::new(["x".into()]),
            Rc::new(BinaryOp(b(Number(2.)), Multiply, b(Id("x".into())))),
        ),
    );
}
