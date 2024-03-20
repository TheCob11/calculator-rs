use std::str::FromStr;

use thiserror::Error;

use crate::ast::*;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unclosed grouping")]
    UnclosedGrouping,
}

impl FromStr for Expr {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_subexpr(s: &str, precedence: u32) -> Result<Expr, ParseError> {
            use Expr as Ex;
            use ParseError as PErr;
            if let Some(x) = s.strip_prefix('(') {
                let inner = &x[..x.find(')').ok_or(PErr::UnclosedGrouping)?];
                Ok(Ex::Grouping(Box::new(parse_subexpr(inner, 0)?)))
            } else if let (op @ ("+" | "-"), x) = s.split_at(1){
                let kind = match op {
                    "+" => UnaryOpKind::Pos,
                    "-" => UnaryOpKind::Neg,
                    _ => unreachable!() // this should've just been checked
                };
                todo!()
                // Ok(Ex::UnaryOp(Box::new(parse_subexpr(x, Ex::precedence(&Expr::UnaryOp(kind, Ex::Number(5.)))))))
            } else {
                todo!()
            }
        }
        parse_subexpr(s, 0)
    }
}

pub trait Precedence {
    fn precedence(&self) -> u32;
}

impl Expr {
    pub const fn precedence(&self) -> u32 {
        match self {
            Expr::Number(_) => 000,
            Expr::Grouping(_) => 100,
            Expr::BinaryOp(_, kind, _) => match kind {
                BinaryOpKind::Power => 200,
                BinaryOpKind::Multiply => 210,
                BinaryOpKind::Divide => 210,
                BinaryOpKind::Add => 220,
                BinaryOpKind::Subtract => 220,
            },
            Expr::UnaryOp(_, _) => 300,
        }
    }
}
