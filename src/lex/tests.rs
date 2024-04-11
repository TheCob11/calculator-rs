use crate::lex::token::Token::*;
use crate::lex::token::{GroupEnd, GroupKind};
use crate::parser::ast::Identifier;

use super::*;

//noinspection RsLiveness (rustrover bug doesnt realize that panic fmt uses error)
fn test_lexing(s: &str, expected_toks: &[Token]) {
    let toks = Lexer::new(s).collect::<Result<Vec<_>, _>>();
    // println!("{s} --> {toks:?}");
    match toks {
        Ok(x) => assert_eq!(x, expected_toks),
        Err(x) => panic!("{x}"),
    }
}

#[test]
fn one_plus_one() {
    const TOKS_1P1: &[Token] = &[Number(1.), Plus, Number(1.)];
    test_lexing("1+1", TOKS_1P1);
    test_lexing("1 + 1", TOKS_1P1);
    test_lexing(" 1 + 1", TOKS_1P1);
    test_lexing("1    +1", TOKS_1P1);
    test_lexing(" 1 + 1         ", TOKS_1P1);
}

#[test]
fn two_plus_four_times_eight() {
    test_lexing("2+4*8", &[Number(2.), Plus, Number(4.), Mul, Number(8.)]);
}

#[test]
fn five_point_four_pow_three_times_2_over_9() {
    test_lexing(
        "(5.4**3)*2/9",
        &[
            Group(GroupKind::Paren, GroupEnd::Open),
            Number(5.4),
            Pow,
            Number(3.),
            Group(GroupKind::Paren, GroupEnd::Close),
            Mul,
            Number(2.),
            Div,
            Number(9.),
        ],
    );
}

#[test]
fn five_cross_three() {
    test_lexing("5×3", &[Number(5.), Mul, Number(3.)]);
}

#[test]
fn five_div_three_pow_two_cross_two_point_two() {
    test_lexing(
        "5÷3**2×2.2",
        &[
            Number(5.),
            Div,
            Number(3.),
            Pow,
            Number(2.),
            Mul,
            Number(2.2),
        ],
    );
}

#[test]
fn five_dot_three_pow_four_cross_two_times_five() {
    test_lexing(
        "5∙3**4×2*5",
        &[
            Number(5.),
            Mul,
            Number(3.),
            Pow,
            Number(4.),
            Mul,
            Number(2.),
            Mul,
            Number(5.),
        ],
    );
}

#[test]
fn x_plus_one() {
    test_lexing(
        "x+1",
        &[Id(Identifier(String::from("x"))), Plus, Number(1.)],
    );
}

#[test]
fn sin_five() {
    test_lexing(
        "sin(5)",
        &[
            Id(Identifier(String::from("sin"))),
            Group(GroupKind::Paren, GroupEnd::Open),
            Number(5.),
            Group(GroupKind::Paren, GroupEnd::Close),
        ],
    );
}
