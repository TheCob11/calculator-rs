use std::collections::HashMap;

use once_cell::sync::Lazy;
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

pub static FN_TABLE: Lazy<HashMap<Identifier, Function>> = Lazy::new(|| {
    let mut m = HashMap::new();
    add_fn(&mut m, "sin", |[x]| x.sin());
    add_fn(&mut m, "cos", |[x]| x.cos());
    add_fn(&mut m, "tan", |[x]| x.tan());
    add_fn(&mut m, "sqrt", |[x]| x.sqrt());
    add_fn(&mut m, "abs", |[x]| x.abs());
    add_fn(&mut m, "exp", |[x]| x.exp());
    add_fn(&mut m, "ln", |[x]| x.ln());
    m
});
