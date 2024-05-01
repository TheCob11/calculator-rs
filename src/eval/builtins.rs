use std::collections::HashMap;

use once_cell::sync::Lazy;
use rand::random;

use crate::parser::ast::Identifier;
use crate::Number;

use super::WrongNArgs;

pub enum BuiltInFunction {
    Nullary(fn() -> Number),
    Unary(fn(Number) -> Number),
    Binary(fn(Number, Number) -> Number),
    Variadic(fn(Vec<Number>) -> Result<Number, WrongNArgs>),
}

impl BuiltInFunction {
    pub fn call(&self, args: Vec<Number>) -> Result<Number, WrongNArgs> {
        use BuiltInFunction as BF;
        Ok(match self {
            BF::Variadic(f) => return f(args),
            BF::Nullary(f) => {
                let [] = args[..] else {
                    return Err(WrongNArgs(0, args.len()));
                };
                f()
            }
            BF::Unary(f) => {
                let [x] = args[..] else {
                    return Err(WrongNArgs(1, args.len()));
                };
                f(x)
            }
            BF::Binary(f) => {
                let [x, y] = args[..] else {
                    return Err(WrongNArgs(2, args.len()));
                };
                f(x, y)
            }
        })
    }
}

impl From<fn() -> Number> for BuiltInFunction {
    fn from(value: fn() -> Number) -> Self {
        Self::Nullary(value)
    }
}

impl From<fn(Number) -> Number> for BuiltInFunction {
    fn from(value: fn(Number) -> Number) -> Self {
        Self::Unary(value)
    }
}

impl From<fn(Number, Number) -> Number> for BuiltInFunction {
    fn from(value: fn(Number, Number) -> Number) -> Self {
        Self::Binary(value)
    }
}

impl From<fn(Vec<Number>) -> Result<Number, WrongNArgs>> for BuiltInFunction {
    fn from(value: fn(Vec<Number>) -> Result<Number, WrongNArgs>) -> Self {
        Self::Variadic(value)
    }
}

//noinspection SpellCheckingInspection
#[allow(clippy::cast_precision_loss)]
pub static BUILTIN_FNS: Lazy<HashMap<Identifier, BuiltInFunction>> = Lazy::new(|| {
    use BuiltInFunction::{Binary as Bi, Nullary as Nul, Unary as Un, Variadic};
    use Number as N;
    let mut map = HashMap::new();
    let mut add = |x: &str, y: BuiltInFunction| map.insert(x.into(), y);
    add("sin", Un(N::sin));
    add("asin", Un(N::asin));
    add("cos", Un(N::cos));
    add("acos", Un(N::acos));
    add("tan", Un(N::tan));
    add("atan", Un(N::atan));
    add("atan2", Bi(f64::atan2));
    add("sqrt", Un(N::sqrt));
    add("cbrt", Un(N::cbrt));
    add("nrt", Bi(|n, x| x.powf(n.recip())));
    add("abs", Un(N::abs));
    add("exp", Un(N::exp));
    add("ln", Un(N::ln));
    add("log2", Un(N::log2));
    add("log10", Un(N::log10));
    add("logn", Bi(|base, x| x.log(base)));
    add("floor", Un(N::floor));
    add("ceil", Un(N::ceil));
    add("round", Un(N::round));
    add("trunc", Un(N::trunc));
    add("recip", Un(N::recip));
    add("rand", Nul(random));
    add(
        "max",
        Variadic(|args| args.into_iter().reduce(Number::max).ok_or(WrongNArgs(2, 0))),
    );
    add(
        "min",
        Variadic(|args| args.into_iter().reduce(Number::min).ok_or(WrongNArgs(2, 0))),
    );
    add("total", Variadic(|args| Ok(args.into_iter().sum())));
    add("product", Variadic(|args| Ok(args.into_iter().product())));
    add(
        "mean",
        Variadic(|args| match args.len() {
            n @ ..=1 => Err(WrongNArgs(n, 2)),
            n => Ok(args.into_iter().sum::<Number>() / (n as f64)),
        }),
    );
    map
});

pub static BUILTIN_CONSTS: Lazy<HashMap<Identifier, Number>> = Lazy::new(|| {
    use std::f64::consts;
    let mut map = HashMap::new();
    let mut add = |id: &str, val: Number| map.insert(id.into(), val);
    add("e", consts::E);
    add("pi", consts::PI);
    add("tau", consts::TAU);
    map
});
