use crate::lex::token::Token::*;
use crate::lex::token::{GroupEnd, GroupKind};

use super::*;

type Res = Result<(), super::Error>;

//noinspection RsLiveness (rustrover bug doesnt realize that panic fmt uses error)
fn test_lexing(s: &str, expected_toks: &[Token]) -> Res {
    let toks = Lexer::new(s).collect::<Result<Vec<_>, _>>()?;
    // println!("{s} --> {toks:?}");
    assert_eq!(expected_toks, toks);
    Ok(())
}

#[test]
fn one_plus_one() -> Res {
    const TOKS_1P1: &[Token] = &[Number(1.), Plus, Number(1.)];
    test_lexing("1+1", TOKS_1P1)?;
    test_lexing("1 + 1", TOKS_1P1)?;
    test_lexing(" 1 + 1", TOKS_1P1)?;
    test_lexing("1    +1", TOKS_1P1)?;
    test_lexing(" 1 + 1         ", TOKS_1P1)
}

#[test]
fn two_plus_four_times_eight() -> Res {
    test_lexing("2+4*8", &[Number(2.), Plus, Number(4.), Mul, Number(8.)])
}

#[test]
fn five_point_four_pow_three_times_2_over_9() -> Res {
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
    )
}

#[test]
fn five_cross_three() -> Res {
    test_lexing("5×3", &[Number(5.), Mul, Number(3.)])
}

#[test]
fn five_div_three_pow_two_cross_two_point_two() -> Res {
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
    )
}

#[test]
fn five_dot_three_pow_four_cross_two_times_five() -> Res {
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
    )
}

#[test]
fn x_plus_one() -> Res {
    test_lexing("x+1", &[Id("x".into()), Plus, Number(1.)])
}

#[test]
fn sin_five() -> Res {
    test_lexing(
        "sin(5)",
        &[
            Id("sin".into()),
            Group(GroupKind::Paren, GroupEnd::Open),
            Number(5.),
            Group(GroupKind::Paren, GroupEnd::Close),
        ],
    )
}

#[test]
fn x_equals_three() -> Res {
    test_lexing("x=3", &[Id("x".into()), Equals, Number(3.)])
}

#[test]
fn f_of_x_equals_two_times_x() -> Res {
    test_lexing(
        "f(x)=2*x",
        &[
            Id("f".into()),
            Group(GroupKind::Paren, GroupEnd::Open),
            Id("x".into()),
            Group(GroupKind::Paren, GroupEnd::Close),
            Equals,
            Number(2.),
            Mul,
            Id("x".into()),
        ],
    )
}
