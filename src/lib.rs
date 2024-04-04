#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

pub use eval::Eval;

pub mod ast;
pub mod eval;
pub mod lex;
pub mod parser;

pub fn calc(s: &str) -> Result<eval::Number, parser::ParsingError> {
    Ok(s.parse::<ast::Expr>()?.eval())
}

pub fn calc_parts(s: &str) -> Result<(lex::Lexer, ast::Expr, eval::Number), parser::ParsingError> {
    let lex: lex::Lexer = s.parse()?;
    let expr = ast::Expr::try_from(lex.clone())?;
    let out = expr.clone().eval();
    Ok((lex, expr, out))
}

#[cfg(test)]
mod tests {
    use crate::eval::Eval;
    use crate::*;
    
    #[allow(clippy::float_cmp)]
    fn test_calc(s: &str, ans: eval::Number) {
        match s.parse::<ast::Expr>() {
            Ok(expr) => assert_eq!(expr.eval(), ans),
            Err(e) => panic!("{e}"),
        }
    }

    #[test]
    fn one_plus_one() {
        test_calc("1+1", 2.);
    }

    #[test]
    fn two_plus_four_times_three() {
        test_calc("2+4*3", 14.);
    }

    #[test]
    fn point_one_plus_point_two() {
        // test_calc(".1+.2", 0.3) lol
        test_calc(".1+.2", 0.1 + 0.2);
    }

    //noinspection RsDoubleNeg
    #[test]
    #[allow(clippy::double_neg)]
    fn neg_neg_neg_neg_five_plus_three_pow_two_over_fifteen() {
        test_calc("----5+3**2/15", ----5. + 3f64.powf(2.) / 15.);
    }
}
