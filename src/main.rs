use std::io::{stdin, stdout, BufRead, Write};

use calculator::lex::Lexer;
use calculator::Expr;

fn main() -> std::io::Result<()> {
    let mut stdout = stdout().lock();
    let mut lines = stdin().lock().lines();
    let mut ctx = calculator::Context::new();
    loop {
        print!(">> ");
        stdout.flush()?;
        let input = match lines.next() {
            Some(x) => x?,
            None => break,
        };
        if let "q" | "quit" | "exit" = input.to_lowercase().trim() {
            break;
        }
        let lex = Lexer::new(&input);
        match lex.clone().collect::<Result<Vec<_>, _>>() {
            Ok(x) => println!("tokens: {x:?}"),
            Err(e) => {
                eprintln!("Lexing Error: {e}");
                continue;
            }
        };
        let expr = match Expr::try_from(lex) {
            Ok(x) => {
                println!("ast: {x:#?}");
                x
            }
            Err(e) => {
                eprintln!("Parsing Error: {e}");
                continue;
            }
        };
        match ctx.eval(&expr) {
            Ok(x) => println!("Evaluation: {x:#?}"),
            Err(e) => eprintln!("Evaluation Error: {e}"),
        }
    }
    Ok(())
}
