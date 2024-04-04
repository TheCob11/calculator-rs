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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // Int(i32), sadge
    Number(Number),
    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(Box<Expr>, BinaryOpKind, Box<Expr>),
}
