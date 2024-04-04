use num::traits::Pow;

use crate::ast::{BinaryOpKind, Expr, UnaryOpKind};

pub type Number = f64;

pub trait Eval {
    fn eval(self) -> Number;
}

impl Eval for Expr {
    fn eval(self) -> Number {
        match self {
            Expr::Number(x) => x,
            Expr::UnaryOp(kind, expr) => Self::eval_unary_op(kind, *expr),
            Expr::BinaryOp(expr_l, kind, expr_r) => Self::eval_binary_op(*expr_l, kind, *expr_r),
        }
    }
}

impl Expr {
    fn eval_unary_op(kind: UnaryOpKind, expr: Expr) -> Number {
        let x = expr.eval();
        match kind {
            UnaryOpKind::Plus => x,
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

    #[allow(clippy::float_cmp)]
    fn test_eval(expr: Expr, val: Number) {
        assert_eq!(expr.eval(), val);
    }

    #[test]
    fn one_plus_one_eq_two() {
        test_eval(
            Expr::BinaryOp(
                Box::new(Expr::Number(1.)),
                BinaryOpKind::Add,
                Box::new(Expr::Number(1.)),
            ),
            2.,
        );
    }

    #[test]
    fn two_times_four_eq_eight() {
        test_eval(Expr::BinaryOp(
            Box::new(Expr::Number(2.)),
            BinaryOpKind::Multiply,
            Box::new(Expr::Number(4.)),
        ), 8.);
    }

    fn bin_op_numbers(lhs: Number, kind: BinaryOpKind, rhs: Number) -> Expr {
        Expr::BinaryOp(
            Box::new(Expr::Number(lhs)),
            kind,
            Box::new(Expr::Number(rhs)),
        )
    }
    #[test]
    fn two_times_neg_three_eq_five_times_two_minus_eight_times_two() {
        let expr_a = bin_op_numbers(2., BinaryOpKind::Multiply, -3.);
        let expr_b = Expr::BinaryOp(
            Box::new(bin_op_numbers(5., BinaryOpKind::Multiply, 2.)),
            BinaryOpKind::Subtract,
            Box::new(bin_op_numbers(8., BinaryOpKind::Multiply, 2.)),
        );
        // println!("{expr_a:#?} \n=?\n {expr_b:#?}");
        test_eval(expr_a, expr_b.eval());
    }

    #[test]
    fn divide_by_zero() {
        test_eval(bin_op_numbers(5., BinaryOpKind::Divide, 0.), Number::INFINITY);
    }
}
