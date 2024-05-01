use std::rc::Rc;

use crate::eval::Number;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpKind {
    Plus,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpKind {
    Power,
    Multiply,
    Divide,
    Add,
    Subtract,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(Number),
    Id(Identifier),
    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(Box<Expr>, BinaryOpKind, Box<Expr>),
    Call(Identifier, Vec<Expr>),
    VarAssign(Identifier, Box<Expr>),
    FnDef(Identifier, Rc<[Identifier]>, Rc<Expr>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier(std::sync::Arc<str>);

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier(value.into())
    }
}
