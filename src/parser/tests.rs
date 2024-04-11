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
