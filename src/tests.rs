//noinspection RsLiveness (rustrover bug doesnt realize that panic fmt uses error)
#[allow(clippy::float_cmp)]
fn test_calc(s: &str, ans: crate::eval::Number) {
    let mut ctx = crate::Context::new();
    match s.parse::<crate::parser::ast::Expr>() {
        Ok(expr) => match ctx.eval(&expr) {
            Ok(eval) => assert_eq!(eval, ans),
            Err(e) => panic!("Eval error: {e}"),
        },
        Err(e) => panic!("Parsing error: {e}"),
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

#[test]
fn sin_three_times_whole_four_plus_ln_six_over_two() {
    test_calc("sin(3*(4+ln(6/2)))", (3. * (4. + (6. / 2f64).ln())).sin());
}
