use std::iter::Peekable;
use std::str::FromStr;

use thiserror::Error;

use ast::{BinaryOpKind, Expr, UnaryOpKind};

use crate::lex::token::{GroupEnd, GroupKind, Token};
use crate::lex::{Error as LexError, Lexer};

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
    #[error("Parenthesis multiplication not supported")] //todo? it messes w/ order of ops
    ImplicitMul,
}

fn handle_expect(tok: &Token, lex: &mut Peekable<Lexer>) -> Result<(), Error> {
    if let Token::Group(kind, GroupEnd::Open) = tok {
        match lex.next().transpose()? {
            Some(Token::Group(other_kind, GroupEnd::Close)) if other_kind.eq(kind) => {}
            _ => return Err(Error::UnclosedGroup(*kind)),
        }
    };
    Ok(())
}

fn parse_step(lex: &mut Peekable<Lexer>, curr_power: BindingPower) -> Result<Expr, Error> {
    use crate::lex::token::Token as T;
    use Error as PErr;
    use Expr as E;
    let mut lhs = match lex.next().ok_or(PErr::GoneTooSoon)?? {
        T::Number(x) => E::Number(x),
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
                E::UnaryOp(tok.try_into()?, Box::new(rhs))
            }
        }
    };
    loop {
        if !match lex.peek() {
            // this is not great
            Some(Ok(tok)) => tok.infix_power().is_some_and(|(l, _)| curr_power < l),
            Some(Err(_)) => return Err(PErr::LexingError(lex.next().unwrap().unwrap_err())),
            None => false,
        } {
            break;
        }
        // these can be unwrapped bc they were just checked on peek
        let tok = lex.next().unwrap().unwrap();
        let (_, r_power) = tok.infix_power().unwrap();
        let rhs = parse_step(lex, r_power)?;
        handle_expect(&tok, lex)?;
        lhs = if let T::Group(GroupKind::Paren, GroupEnd::Open) = tok {
            if let Expr::Id(id) = lhs {
                // todo multi arg fns
                E::Call(id, vec![rhs])
            } else {
                return Err(PErr::ImplicitMul);
            }
        } else {
            E::BinaryOp(Box::new(lhs), tok.try_into()?, Box::new(rhs))
        }
    }
    Ok(lhs)
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
            tok => return Err(BadUnOp(tok)),
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
            tok => return Err(BadBinOp(tok)),
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
            _ => return None,
        })
    }
    #[must_use]
    const fn prefix_power(&self) -> Option<BindingPower> {
        use Token as T;
        Some(match &self {
            T::Plus | T::Minus => 10,
            T::Number(_) | T::Id(_) | T::Group(_, GroupEnd::Open) => 0,
            _ => return None,
        })
    }
}
