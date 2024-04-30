use std::collections::HashMap;

use num::traits::Pow;
use thiserror::Error;

use crate::parser::ast::Identifier;
use crate::parser::ast::{BinaryOpKind, Expr, UnaryOpKind};

mod builtins;
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
    WrongNArgs(#[from] WrongNArgs),
}

#[derive(Error, Debug)]
#[error("Expected {0} args, got {1}")]
pub struct WrongNArgs(usize, usize);

pub struct UserFunction {
    arg_names: Vec<Identifier>,
    body: Expr,
}

impl UserFunction {
    fn call(&self, ctx: &mut Context, args: Vec<Number>) -> Result<Number, Error> {
        if args.len() != self.arg_names.len() {
            Err(WrongNArgs(self.arg_names.len(), args.len()))?;
        }
        let bindings_to_return = self
            .arg_names
            .iter()
            .cloned()
            .zip(args)
            .filter_map(|(id, val)| ctx.vars.insert(id.clone(), val).map(|x| (id, x)))
            .collect::<Vec<_>>();
        let res = ctx.eval(&self.body)?;
        ctx.vars.extend(bindings_to_return);
        Ok(res)
    }
}

pub struct Context {
    pub vars: HashMap<Identifier, Number>,
    pub fns: HashMap<Identifier, UserFunction>,
}

impl Context {
    #[must_use]
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Number, Error> {
        Ok(match expr {
            Expr::Number(x) => *x,
            Expr::UnaryOp(kind, expr) => self.eval_unary_op(*kind, expr)?,
            Expr::BinaryOp(expr_l, kind, expr_r) => self.eval_binary_op(expr_l, *kind, expr_r)?,
            Expr::Call(id, expr) => self.eval_call(id.clone(), expr)?,
            Expr::Id(id) => *builtins::BUILTIN_CONSTS
                .get(id)
                .or_else(|| self.vars.get(id))
                .ok_or_else(|| Error::UndefinedVar(id.clone()))?,
        })
    }

    fn eval_unary_op(&mut self, kind: UnaryOpKind, expr: &Expr) -> Result<Number, Error> {
        let x = self.eval(expr)?;
        Ok(match kind {
            UnaryOpKind::Plus => x,
            UnaryOpKind::Neg => -x,
        })
    }

    fn eval_binary_op(
        &mut self,
        expr_l: &Expr,
        kind: BinaryOpKind,
        expr_r: &Expr,
    ) -> Result<Number, Error> {
        let (lhs, rhs) = (self.eval(expr_l)?, self.eval(expr_r)?);
        Ok(match kind {
            BinaryOpKind::Add => lhs + rhs,
            BinaryOpKind::Subtract => lhs - rhs,
            BinaryOpKind::Multiply => lhs * rhs,
            BinaryOpKind::Divide => lhs / rhs,
            BinaryOpKind::Power => lhs.pow(rhs),
        })
    }

    fn eval_call(&mut self, id: Identifier, args: &[Expr]) -> Result<Number, Error> {
        let args: Vec<_> = args
            .iter()
            .map(|x| self.eval(x))
            .collect::<Result<_, _>>()?;
        Ok(if let Some(f) = builtins::BUILTIN_FNS.get(&id) {
            f.call(args)?
        } else {
            let Some(f) = self.fns.remove(&id) else {
                return Err(Error::UnrecognizedFunction(id));
            };
            let res = f.call(self, args)?;
            self.fns.insert(id, f);
            res
        })
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
