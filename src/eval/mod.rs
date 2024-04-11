use num::traits::Pow;
use thiserror::Error;

use crate::parser::ast::Identifier;
use crate::parser::ast::{BinaryOpKind, Expr, UnaryOpKind};

mod functions;
#[cfg(test)]
mod tests;

pub type Number = f64;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Undefined variable {0:?}")]
    UndefinedVar(Identifier), //todo?
    #[error("Unrecognized function {0:?}")]
    UnrecognizedFunction(Identifier),
    #[error(transparent)]
    WrongNArgs(#[from] functions::WrongNArgs),
}

impl Expr {
    pub fn eval(self) -> Result<Number, Error> {
        Ok(match self {
            Expr::Number(x) => x,
            Expr::UnaryOp(kind, expr) => Self::eval_unary_op(kind, *expr)?,
            Expr::BinaryOp(expr_l, kind, expr_r) => Self::eval_binary_op(*expr_l, kind, *expr_r)?,
            Expr::Call(id, expr) => Self::eval_call(id, expr)?,
            Expr::Id(x) => return Err(Error::UndefinedVar(x)),
        })
    }

    fn eval_unary_op(kind: UnaryOpKind, expr: Expr) -> Result<Number, Error> {
        let x = expr.eval()?;
        Ok(match kind {
            UnaryOpKind::Plus => x,
            UnaryOpKind::Neg => -x,
        })
    }

    fn eval_binary_op(expr_l: Expr, kind: BinaryOpKind, expr_r: Expr) -> Result<Number, Error> {
        let (lhs, rhs) = (expr_l.eval()?, expr_r.eval()?);
        Ok(match kind {
            BinaryOpKind::Add => lhs + rhs,
            BinaryOpKind::Subtract => lhs - rhs,
            BinaryOpKind::Multiply => lhs * rhs,
            BinaryOpKind::Divide => lhs / rhs,
            BinaryOpKind::Power => lhs.pow(rhs),
        })
    }

    fn eval_call(id: Identifier, args: Vec<Expr>) -> Result<Number, Error> {
        let args: Vec<_> = args.into_iter().map(Expr::eval).collect::<Result<_, _>>()?;
        Ok(functions::FN_TABLE
            .get(&id)
            .ok_or(Error::UnrecognizedFunction(id))?(args)?)
    }
}
