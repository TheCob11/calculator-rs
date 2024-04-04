use std::io::{BufRead, stdin, stdout, Write};

use calculator::calc_parts;

fn main() -> std::io::Result<()> {
    let mut stdout = stdout().lock();
    print!(">> ");
    stdout.flush()?;
    for line in stdin().lock().lines() {
        let input = line?;
        if let "q" | "quit" | "exit" = input.to_lowercase().trim() {
            return Ok(());
        }
        match calc_parts(&input) {
            Ok((lex, expr, out)) => {
                let toks = lex.collect::<Result<Vec<_>, _>>().unwrap();
                println!("tokens: {toks:?}");
                println!("ast: {expr:?}");
                println!("evaluation: {out}");
            }
            Err(err) => eprintln!("Error: {err}"),
        }
        print!(">> ");
        stdout.flush()?;
    };
    Ok(())
}
