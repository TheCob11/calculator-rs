use std::iter::Peekable;
use std::rc::Rc;
use std::str::FromStr;

use thiserror::Error;

use ast::{BinaryOpKind, Expr, UnaryOpKind};

use crate::lex::token::{GroupEnd, GroupKind, Token};
use crate::lex::{Error as LexError, Lexer};
use crate::parser::ast::{Identifier, PExpr};

pub mod ast;
#[cfg(test)]
mod tests;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    LexingError(#[from] LexError),
    #[error("Unexpected token '{0:?}'")]
    UnexpectedToken(Token),
    #[error(transparent)]
    BadUnOp(#[from] BadUnOp),
    #[error(transparent)]
    BadBinOp(#[from] BadBinOp),
    #[error("Unclosed grouping '{0:?}'")]
    UnclosedGroup(GroupKind),
    #[error("Expected a prefix operator, found '{0:?}'")]
    NotPrefix(Token),
    #[error("Unexpected end of input")]
    GoneTooSoon,
    #[error("Unexpected trailing input after end of expression: '{0:?}'")]
    TrailingToken(Vec<Result<Token, LexError>>),
    // #[error("Implicit multiplication with parenthesis not supported")]
    //todo? it messes w/ order of ops
    // ImplicitMul,
    #[error(
        "Function parameters must be defined as variables, but function {0:?} has parameter {1:?}"
    )]
    FnDefParamExpr(Identifier, PExpr),
    #[error("Function definitions can only be assigned to identifiers, not {0:?}")]
    FnDefIdExpr(PExpr),
    #[error("Can only assign to variables or functions, not {0:?}")]
    BadAssign(PExpr),
}

fn handle_expect(
    tok: &Token,
    lex: &mut Peekable<impl Iterator<Item = Result<Token, LexError>>>,
) -> Result<(), Error> {
    if let Token::Group(kind, GroupEnd::Open) = tok {
        match lex.next().transpose()? {
            Some(Token::Group(other_kind, GroupEnd::Close)) if other_kind.eq(kind) => {}
            _ => return Err(Error::UnclosedGroup(*kind)),
        }
    };
    Ok(())
}

fn parse_step(
    lex: &mut Peekable<impl Iterator<Item = Result<Token, LexError>>>,
    curr_power: BindingPower,
) -> Result<Expr, Error> {
    use crate::lex::token::Token as T;
    use Error as PErr;
    use Expr as E;
    let mut lhs = match lex.next().ok_or(PErr::GoneTooSoon)?? {
        T::Number(x) => E::Num(x),
        T::Id(x) => E::Id(x),
        tok => {
            let Some(pre_power) = tok.prefix_power() else {
                return Err(PErr::NotPrefix(tok));
            };
            let rhs = parse_step(lex, pre_power)?;
            handle_expect(&tok, lex)?;
            if let T::Group(_, GroupEnd::Open) = tok {
                rhs
            } else {
                E::UnaryOp(tok.try_into()?, rhs.into())
            }
        }
    };
    while let Some(Ok(tok)) = lex.next_if(|res| {
        res.as_ref().is_ok_and(|tok| {
            tok.infix_power()
                .is_some_and(|(left_power, _)| curr_power < left_power)
        })
    }) {
        let (_, r_power) = tok
            .infix_power()
            .expect("should have been checked on the next_if");
        lhs = if let T::Group(GroupKind::Paren, GroupEnd::Open) = tok {
            // let Expr::Id(func) = lhs else {
            //     return Err(PErr::ImplicitMul);
            // };
            E::Call(lhs.into(), parse_args(lex, r_power)?)
        } else if let T::Equals = tok {
            match lhs {
                Expr::Id(id) => Expr::VarAssign(id, parse_step(lex, r_power)?.into()),
                Expr::Call(f, args) => {
                    let f = Rc::into_inner(f)
                        .expect("Exprs shouldn't have multiple references during parsing");
                    let Expr::Id(id) = f else {
                        return Err(Error::FnDefIdExpr(f.into()));
                    };
                    let arg_names = match args
                        .into_iter()
                        .map(|x| {
                            if let Expr::Id(id) = x.as_ref() {
                                Ok(id.clone())
                            } else {
                                Err(x)
                            }
                        })
                        .collect::<Result<_, _>>()
                    {
                        Ok(x) => x,
                        Err(x) => return Err(Error::FnDefParamExpr(id, x)),
                    };
                    let def_expr = E::FnDef(arg_names, parse_step(lex, r_power)?.into());
                    E::VarAssign(id, def_expr.into())
                }
                x => return Err(Error::BadAssign(x.into())),
            }
        } else {
            let rhs = parse_step(lex, r_power)?;
            handle_expect(&tok, lex)?;
            E::BinaryOp(lhs.into(), tok.try_into()?, rhs.into())
        }
    }
    Ok(lhs)
}

fn parse_args(
    lex: &mut Peekable<impl Iterator<Item = Result<Token, LexError>>>,
    r_power: BindingPower,
) -> Result<Vec<PExpr>, Error> {
    let mut args = Vec::new();
    while lex
        .peek()
        .is_some_and(|res| !matches!(res, Ok(Token::Group(GroupKind::Paren, GroupEnd::Close))))
    {
        args.push(parse_step(lex, r_power)?.into());
        lex.next_if(|res| matches!(res, Ok(Token::Comma)));
    }
    let Some(Token::Group(GroupKind::Paren, GroupEnd::Close)) = lex.next().transpose()? else {
        return Err(Error::UnclosedGroup(GroupKind::Paren));
    };

    Ok(args)
}

impl Expr {
    fn parse(lex: Lexer) -> Result<Self, Error> {
        let mut lex = lex.peekable();
        let expr = parse_step(&mut lex, 0)?;
        // shouldn't have any more tokens after expression
        if lex.peek().is_some() {
            Err(Error::TrailingToken(lex.collect()))
        } else {
            Ok(expr)
        }
    }
}

impl TryFrom<Lexer> for Expr {
    type Error = Error;

    fn try_from(lex: Lexer) -> Result<Self, Self::Error> {
        Self::parse(lex)
    }
}

impl FromStr for Expr {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(Lexer::new(s))
    }
}

#[derive(Error, Debug)]
#[error("{0:?} is not a unary operator")]
pub struct BadUnOp(Token);

impl TryFrom<Token> for UnaryOpKind {
    type Error = BadUnOp;
    fn try_from(tok: Token) -> Result<Self, Self::Error> {
        use Token as T;
        Ok(match tok {
            T::Plus => UnaryOpKind::Plus,
            T::Minus => UnaryOpKind::Neg,
            T::Group(_, _)
            | T::Comma
            | T::Equals
            | T::Pow
            | T::Mul
            | T::Div
            | T::Number(_)
            | T::Id(_) => return Err(BadUnOp(tok)),
        })
    }
}

#[derive(Error, Debug)]
#[error("{0:?} is not a binary operator")]
pub struct BadBinOp(Token);

impl TryFrom<Token> for BinaryOpKind {
    type Error = BadBinOp;
    fn try_from(tok: Token) -> Result<Self, Self::Error> {
        use crate::lex::token::Token as T;
        use BinaryOpKind as K;
        Ok(match tok {
            T::Pow => K::Power,
            T::Mul => K::Multiply,
            T::Div => K::Divide,
            T::Plus => K::Add,
            T::Minus => K::Subtract,
            T::Group(_, _) | T::Comma | T::Equals | T::Number(_) | T::Id(_) => {
                return Err(BadBinOp(tok))
            }
        })
    }
}

pub(crate) type BindingPower = u8;

impl Token {
    #[must_use]
    const fn infix_power(&self) -> Option<(BindingPower, BindingPower)> {
        use Token as T;
        Some(match self {
            T::Pow => (11, 10),
            T::Mul | T::Div => (8, 9),
            T::Plus | T::Minus => (6, 7),
            T::Group(_, GroupEnd::Open) => (BindingPower::MAX, BindingPower::MIN),
            T::Equals => (1, 1),
            T::Number(_) | T::Id(_) | T::Group(_, GroupEnd::Close) | T::Comma => return None,
        })
    }
    #[must_use]
    const fn prefix_power(&self) -> Option<BindingPower> {
        use Token as T;
        Some(match &self {
            T::Plus | T::Minus => 10,
            T::Number(_) | T::Id(_) | T::Group(_, GroupEnd::Open) => 0,
            Token::Pow
            | Token::Mul
            | Token::Div
            | T::Group(_, GroupEnd::Close)
            | T::Comma
            | T::Equals => return None,
        })
    }
}
