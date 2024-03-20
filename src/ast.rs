use crate::eval::Number;

#[derive(Debug)]
pub enum UnaryOpKind {
    Pos, // noop
    Neg,
}
#[derive(Debug)]
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
    Grouping(Box<Expr>),
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
