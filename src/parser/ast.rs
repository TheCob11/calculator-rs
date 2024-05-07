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

// tbh im not super happy with using rc everywhere but i cant seem to find other options that suit how im doing functions as values
pub type PExpr = std::rc::Rc<Expr>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Num(Number),
    Id(Identifier),
    UnaryOp(UnaryOpKind, PExpr),
    BinaryOp(PExpr, BinaryOpKind, PExpr),
    Call(PExpr, Vec<PExpr>),
    VarAssign(Identifier, PExpr),
    FnDef(Vec<Identifier>, PExpr),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Ord, PartialOrd)]
pub struct Identifier(std::sync::Arc<str>);

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier(value.into())
    }
}
