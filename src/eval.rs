use num::traits::Pow;

use crate::ast::*;

pub type Number = f64;

pub trait Eval {
    fn eval(self) -> Number;
}

impl Eval for Expr {
    fn eval(self) -> Number {
        match self {
            Expr::Number(x) => x,
            Expr::Grouping(x) => x.eval(),
            Expr::UnaryOp(kind, expr) => Self::eval_unary_op(kind, *expr),
            Expr::BinaryOp(expr_l, kind, expr_r) => Self::eval_binary_op(*expr_l, kind, *expr_r),
        }
    }
}

impl Expr {
    fn eval_unary_op(kind: UnaryOpKind, expr: Expr) -> Number {
        let x = expr.eval();
        match kind {
            UnaryOpKind::Pos => x,
            UnaryOpKind::Neg => -x,
        }
    }
    
    fn eval_binary_op(expr_l: Expr, kind: BinaryOpKind, expr_r: Expr) -> Number {
        let (lhs, rhs) = (expr_l.eval(), expr_r.eval());
        match kind {
            BinaryOpKind::Add => lhs + rhs,
            BinaryOpKind::Subtract => lhs - rhs,
            BinaryOpKind::Multiply => lhs * rhs,
            BinaryOpKind::Divide => lhs / rhs,
            BinaryOpKind::Power => lhs.pow(rhs),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_plus_one_eq_two() {
        let expr = Expr::BinaryOp(Box::new(Expr::Number(1.)), BinaryOpKind::Add, Box::new(Expr::Number(1.)));
        println!("{expr:?}");
        assert_eq!(expr.eval(), 2.)
    }
    
    #[test]
    fn two_times_four_eq_eight() {
        let expr = Expr::BinaryOp(Box::new(Expr::Number(2.)), BinaryOpKind::Multiply, Box::new(Expr::Number(4.)));
        println!("{expr:?}");
        assert_eq!(expr.eval(), 8.);
    }
    
    #[test]
    fn two_times_neg_three_eq_five_times_two_minus_eight_times_two() {
        let expr_a = Expr::bin_op_numbers(2., BinaryOpKind::Multiply, -3.);
        let expr_b = Expr::BinaryOp(
            Box::new(Expr::bin_op_numbers(5., BinaryOpKind::Multiply, 2.)),
            BinaryOpKind::Subtract,
            Box::new(Expr::bin_op_numbers(8., BinaryOpKind::Multiply, 2.))
        );
        println!("{expr_a:#?} \n=?\n {expr_b:#?}");
        assert_eq!(expr_a.eval(), expr_b.eval())
    }
    
    #[test]
    fn divide_by_zero() {
        let expr = Expr::bin_op_numbers(5., BinaryOpKind::Divide, 0.);
        println!("{expr:?} <---- clueless");
        assert_eq!(expr.eval(), f64::INFINITY)
    }
}