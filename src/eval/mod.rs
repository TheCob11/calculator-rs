use std::collections::HashMap;
use std::rc::Rc;

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
    UndefinedVar(Identifier),
    #[error("Unrecognized function {0:?}")]
    UnrecognizedFunction(Identifier),
    #[error(transparent)]
    WrongNArgs(#[from] WrongNArgs),
    #[error("Can't override builtin symbol {0:?}")]
    BuiltinOverride(Identifier),
    #[error("Cant use {0:?} as a parameter because it is a builtin symbol")]
    BuiltinParam(Identifier),
    #[error("{0:?} is a variable and can not be called")]
    VarNotFunc(Identifier),
    #[error("{0:?} is a function and must be called")]
    FuncNotVar(Identifier),
}

#[derive(Error, Debug)]
#[error("Expected {0} args, got {1}")]
pub struct WrongNArgs(usize, usize);

pub struct UserFunction {
    arg_names: Rc<[Identifier]>,
    body: Rc<Expr>,
}

impl UserFunction {
    fn call(&self, ctx: &mut Context, args: Vec<Number>) -> Result<Number, Error> {
        if args.len() != self.arg_names.len() {
            Err(WrongNArgs(self.arg_names.len(), args.len()))?;
        }
        let bindings_to_return = self
            .arg_names
            .iter()
            .zip(args)
            .map(|(id, val)| {
                let prev;
                if let Some(x) = ctx.vars.get_mut(id) {
                    (prev, *x) = (Some(*x), val);
                } else {
                    prev = None;
                    ctx.vars.insert(id.clone(), val);
                }
                (id, prev)
            })
            .collect::<Vec<_>>();
        let res = ctx.eval(&self.body);
        for (id, prev) in bindings_to_return {
            match prev {
                Some(x) => {
                    *ctx.vars
                        .get_mut(id)
                        .expect("Saved bindings before call should have been bound for call") = x;
                }
                None => {
                    ctx.vars.remove(id);
                }
            }
        }
        res
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
        match expr {
            Expr::Number(x) => Ok(*x),
            Expr::UnaryOp(kind, expr) => self.eval_unary_op(*kind, expr),
            Expr::BinaryOp(expr_l, kind, expr_r) => self.eval_binary_op(expr_l, *kind, expr_r),
            Expr::Call(id, expr) => self.eval_call(id.clone(), expr),
            Expr::Id(id) => builtins::BUILTIN_CONSTS
                .get(id)
                .or_else(|| self.vars.get(id))
                .ok_or_else(|| Error::UndefinedVar(id.clone()))
                .copied(),
            Expr::VarAssign(id, body) => self.eval_var_assign(id.clone(), body),
            Expr::FnDef(id, arg_names, body) => {
                self.eval_fn_def(id.clone(), arg_names.to_owned(), body.to_owned())
            }
        }
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
            let res = f.call(self, args);
            self.fns.insert(id, f);
            res?
        })
    }

    fn eval_var_assign(&mut self, id: Identifier, body: &Expr) -> Result<Number, Error> {
        if Self::is_builtin(&id) {
            return Err(Error::BuiltinOverride(id));
        }
        self.fns.remove(&id);
        let val = self.eval(body)?;
        self.vars.insert(id, val);
        Ok(val)
    }

    fn eval_fn_def(
        &mut self,
        id: Identifier,
        arg_names: Rc<[Identifier]>,
        body: Rc<Expr>,
    ) -> Result<Number, Error> {
        if Self::is_builtin(&id) {
            return Err(Error::BuiltinOverride(id));
        }
        if let Some(id) = arg_names.iter().find(|x| Self::is_builtin(x)) {
            return Err(Error::BuiltinParam(id.clone()));
        }
        self.vars.remove(&id);
        self.fns.insert(id, UserFunction { arg_names, body });
        // todo should this return a new value type?
        Ok(1.)
    }

    #[inline]
    fn is_builtin(id: &Identifier) -> bool {
        builtins::BUILTIN_FNS.contains_key(id) || builtins::BUILTIN_CONSTS.contains_key(id)
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
