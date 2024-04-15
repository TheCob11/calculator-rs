use std::collections::HashMap;

use once_cell::sync::Lazy;
use rand::random;
use thiserror::Error;

use crate::parser::ast::Identifier;
use crate::Number;

#[derive(Error, Debug)]
#[error("Expected {0} args, got {1}")]
pub struct WrongNArgs(usize, usize);

type Function = Box<dyn Send + Sync + Fn(Vec<Number>) -> Result<Number, WrongNArgs>>;

fn add_fn<const ARITY: usize>(
    map: &mut HashMap<Identifier, Function>,
    id: &str,
    func: fn([Number; ARITY]) -> Number,
) -> Option<Function> {
    map.insert(
        Identifier(id.to_string()),
        Box::new(move |args| match args.as_slice().try_into() {
            Ok(x) => Ok(func(x)),
            Err(_) => Err(WrongNArgs(ARITY, args.len())),
        }),
    )
}

fn add_fn_general(
    map: &mut HashMap<Identifier, Function>,
    id: &str,
    func: fn(Vec<Number>) -> Result<Number, WrongNArgs>,
) -> Option<Function> {
    map.insert(Identifier(id.to_string()), Box::new(func))
}
#[allow(clippy::cast_precision_loss)]
pub static FN_TABLE: Lazy<HashMap<Identifier, Function>> = Lazy::new(|| {
    let mut map = HashMap::new();
    let m = &mut map;
    add_fn(m, "sin", |[x]| x.sin());
    add_fn(m, "asin", |[x]| x.asin());
    add_fn(m, "cos", |[x]| x.cos());
    add_fn(m, "acos", |[x]| x.acos());
    add_fn(m, "tan", |[x]| x.tan());
    add_fn(m, "atan", |[x]| x.atan());
    add_fn(m, "atan2", |[x, y]| x.atan2(y));
    add_fn(m, "sqrt", |[x]| x.sqrt());
    add_fn(m, "cbrt", |[x]| x.cbrt());
    add_fn(m, "nrt", |[n, x]| x.powf(n.recip()));
    add_fn(m, "abs", |[x]| x.abs());
    add_fn(m, "exp", |[x]| x.exp());
    add_fn(m, "ln", |[x]| x.ln());
    add_fn(m, "log2", |[x]| x.log2());
    add_fn(m, "log10", |[x]| x.log10());
    add_fn(m, "logn", |[base, x]| x.log(base));
    add_fn(m, "floor", |[x]| x.floor());
    add_fn(m, "ceil", |[x]| x.ceil());
    add_fn(m, "round", |[x]| x.round());
    add_fn(m, "trunc", |[x]| x.trunc());
    add_fn(m, "recip", |[x]| x.recip());
    add_fn(m, "rand", |[]| random());
    add_fn(m, "e", |[]| std::f64::consts::E);
    add_fn(m, "E", |[]| std::f64::consts::E);
    add_fn(m, "pi", |[]| std::f64::consts::PI);
    add_fn(m, "PI", |[]| std::f64::consts::PI);
    add_fn(m, "π", |[]| std::f64::consts::PI);
    add_fn_general(m, "max", |args| {
        args.into_iter().reduce(Number::max).ok_or(WrongNArgs(2, 0))
    });
    add_fn_general(m, "min", |args| {
        args.into_iter().reduce(Number::min).ok_or(WrongNArgs(2, 0))
    });
    add_fn_general(m, "total", |args| Ok(args.into_iter().sum()));
    add_fn_general(m, "product", |args| Ok(args.into_iter().product()));
    add_fn_general(m, "mean", |args| match args.len() {
        n @ ..=1 => Err(WrongNArgs(n, 2)),
        n => Ok(args.into_iter().sum::<Number>() / (n as f64)),
    });
    map
});
