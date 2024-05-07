type Res = Result<(), super::Error>;

//noinspection RsLiveness (rustrover bug doesnt realize that panic fmt uses error)
#[allow(clippy::float_cmp)]
fn test_calc(s: &str, ans: impl Into<crate::eval::Value>) -> Res {
    let mut ctx = crate::Context::new();
    let expr = s.parse()?;
    let eval = ctx.eval(&expr)?;
    assert_eq!(ans.into(), eval);
    Ok(())
}

#[test]
fn one_plus_one() -> Res {
    test_calc("1+1", 2.)
}

#[test]
fn two_plus_four_times_three() -> Res {
    test_calc("2+4*3", 14.)
}

#[test]
fn point_one_plus_point_two() -> Res {
    // test_calc(".1+.2", 0.3) lol
    test_calc(".1+.2", 0.1 + 0.2)
}

//noinspection RsDoubleNeg
#[test]
#[allow(clippy::double_neg)]
fn neg_neg_neg_neg_five_plus_three_pow_two_over_fifteen() -> Res {
    test_calc("----5+3**2/15", ----5. + 3f64.powf(2.) / 15.)
}

#[test]
fn sin_three_times_whole_four_plus_ln_six_over_two() -> Res {
    test_calc("sin(3*(4+ln(6/2)))", (3. * (4. + (6. / 2f64).ln())).sin())
}
