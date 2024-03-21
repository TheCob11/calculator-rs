use crate::eval::Number;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpKind {
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

#[derive(Debug)]
pub enum Expr {
    // Int(i32), sadge
    Number(Number),
    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(Box<Expr>, BinaryOpKind, Box<Expr>),
}

impl Expr {
    pub fn bin_op_numbers(lhs: Number, kind: BinaryOpKind, rhs: Number) -> Expr {
        Self::BinaryOp(
            Box::new(Self::Number(lhs)),
            kind,
            Box::new(Self::Number(rhs)),
        )
    }
}
