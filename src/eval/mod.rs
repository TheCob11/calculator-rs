use std::collections::HashMap;

use num::traits::Pow;
use thiserror::Error;

use crate::parser::ast::{BinaryOpKind, Expr, Identifier, PExpr, UnaryOpKind};

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
    #[error("Can not call number {0:?} like a function (currently no support for implicit multiplication)")]
    ImplicitMul(Number),
    #[error("Calls nested too deeply(passed limit of {MAX_CALL_DEPTH})")]
    CallOverflow,
}

#[derive(Error, Debug)]
#[error("Expected {0} args, got {1}")]
pub struct WrongNArgs(usize, usize);

#[derive(Debug, Clone, PartialEq)]
pub struct UserFunction {
    arg_names: Vec<Identifier>,
    body: PExpr,
}

pub const MAX_CALL_DEPTH: usize = 100;

impl UserFunction {
    fn call(&self, ctx: &mut Context, args: Vec<Value>) -> Res {
        if ctx.call_depth > MAX_CALL_DEPTH {
            return Err(Error::CallOverflow);
        }
        if args.len() != self.arg_names.len() {
            Err(WrongNArgs(self.arg_names.len(), args.len()))?;
        }
        let bindings_to_return = self
            .arg_names
            .iter()
            .zip(args)
            .map(|(id, val)| {
                let prev;
                if let Some(x) = ctx.syms.get_mut(id) {
                    (prev, *x) = (Some(x.clone()), val);
                } else {
                    prev = None;
                    ctx.syms.insert(id.clone(), val);
                }
                (id, prev)
            })
            .collect::<Vec<_>>();
        ctx.call_depth += 1;
        let res = ctx.eval(&self.body);
        ctx.call_depth -= 1;
        for (id, prev) in bindings_to_return {
            if let Some(x) = prev {
                *ctx.syms
                    .get_mut(id)
                    .expect("Saved bindings before call should have been bound for call") = x;
            } else {
                ctx.syms.remove(id);
            }
        }
        res
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Num(Number),
    Func(UserFunction),
}

impl Value {
    fn num(&self) -> Option<&Number> {
        match self {
            Value::Num(x) => Some(x),
            Value::Func(_) => None,
        }
    }

    // fn func(self) -> Result<UserFunction, Value> {
    //     match self {
    //         Value::Func(x) => Ok(x),
    //         Value::Num(_) => Err(self),
    //     }
    // }

    fn expr_with_args(self) -> (PExpr, Option<Vec<Identifier>>) {
        match self {
            Value::Num(x) => (Expr::Num(x).into(), None),
            Value::Func(UserFunction { arg_names, body }) => (body, Some(arg_names)),
        }
    }
}

impl From<Number> for Value {
    fn from(value: Number) -> Self {
        Value::Num(value)
    }
}

type Res = Result<Value, Error>;

pub struct Context {
    pub syms: HashMap<Identifier, Value>,
    call_depth: usize,
}

impl Context {
    #[must_use]
    pub fn new() -> Self {
        Self {
            syms: HashMap::new(),
            call_depth: 0,
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Res {
        match expr {
            Expr::Num(x) => Ok(Value::Num(*x)),
            Expr::UnaryOp(kind, expr) => self.eval_unary_op(*kind, expr),
            Expr::BinaryOp(expr_l, kind, expr_r) => self.eval_binary_op(expr_l, *kind, expr_r),
            Expr::Call(lhs, expr) => self.eval_call(lhs, expr),
            Expr::Id(id) => builtins::BUILTIN_CONSTS
                .get(id)
                .map_or_else(|| self.syms.get(id).cloned(), |&x| Some(Value::Num(x)))
                .ok_or_else(|| Error::UndefinedVar(id.clone())),
            Expr::VarAssign(id, body) => self.eval_var_assign(id.clone(), body),
            Expr::FnDef(arg_names, body) => self.eval_fn_def(arg_names.clone(), body.clone()),
        }
    }

    fn eval_unary_op(&mut self, kind: UnaryOpKind, expr: &Expr) -> Res {
        match self.eval(expr)? {
            Value::Num(x) => Ok(Value::Num(match kind {
                UnaryOpKind::Plus => x,
                UnaryOpKind::Neg => -x,
            })),
            Value::Func(f) => Ok(Value::Func(UserFunction {
                body: Expr::UnaryOp(kind, f.body).into(),
                ..f
            })),
        }
    }

    fn eval_binary_op(&mut self, expr_l: &Expr, kind: BinaryOpKind, expr_r: &Expr) -> Res {
        Ok(match (self.eval(expr_l)?, self.eval(expr_r)?) {
            (Value::Num(lhs), Value::Num(rhs)) => Value::Num(match kind {
                BinaryOpKind::Add => lhs + rhs,
                BinaryOpKind::Subtract => lhs - rhs,
                BinaryOpKind::Multiply => lhs * rhs,
                BinaryOpKind::Divide => lhs / rhs,
                BinaryOpKind::Power => lhs.pow(rhs),
            }),
            (val_l, val_r) => {
                let ((lhs, args_l), (rhs, args_r)) =
                    (val_l.expr_with_args(), val_r.expr_with_args());
                let mut arg_names =
                    [args_l.unwrap_or_default(), args_r.unwrap_or_default()].concat();
                arg_names.sort_unstable();
                arg_names.dedup();
                Value::Func(UserFunction {
                    arg_names,
                    body: Expr::BinaryOp(lhs, kind, rhs).into(),
                })
            }
        })
    }

    fn eval_call(&mut self, lhs: &Expr, arg_exprs: &[PExpr]) -> Res {
        let args: Vec<Value> = arg_exprs
            .iter()
            .map(|x| self.eval(x))
            .collect::<Result<_, _>>()?;
        if let Expr::Id(id) = lhs {
            Ok(if let Some(f) = builtins::BUILTIN_FNS.get(id) {
                if let Some(nums) = args.iter().map(|val| val.num().copied()).collect() {
                    Value::Num(f.call(nums)?)
                } else {
                    let (bodies, args): (Vec<PExpr>, Vec<Option<Vec<Identifier>>>) =
                        args.into_iter().map(Value::expr_with_args).unzip();
                    Value::Func(UserFunction {
                        arg_names: args.into_iter().flatten().flatten().collect(),
                        body: Expr::Call(Expr::Id(id.clone()).into(), bodies).into(),
                    })
                }
            } else {
                let Some(Value::Func(f)) = self.syms.get(id).cloned() else {
                    return Err(Error::UnrecognizedFunction(id.clone()));
                };
                f.call(self, args)?
            })
        } else {
            match self.eval(lhs)? {
                Value::Func(f) => f.call(self, args),
                Value::Num(x) => Err(Error::ImplicitMul(x)),
            }
        }
    }

    fn eval_var_assign(&mut self, id: Identifier, body: &Expr) -> Res {
        if Self::is_builtin(&id) {
            return Err(Error::BuiltinOverride(id));
        }
        let val = self.eval(body)?;
        self.syms.insert(id, val.clone());
        Ok(val)
    }

    fn eval_fn_def(&mut self, arg_names: Vec<Identifier>, body: PExpr) -> Res {
        if let Some(id) = arg_names.iter().find(|x| Self::is_builtin(x)) {
            return Err(Error::BuiltinParam(id.clone()));
        }
        Ok(Value::Func(UserFunction { arg_names, body }))
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
