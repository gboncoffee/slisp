mod parser;
mod vm;

use std::io::{self, BufRead, Write};

fn main() {
    let mut vm = vm::Lisp::new();

    print!("lisp> ");
    io::stdout().flush().unwrap();
    for line in io::stdin().lock().lines() {
        match vm.eval(&line.unwrap(), None) {
            Ok(value) => {
                if let Err(err) = vm.println(&[value]) {
                    println!("{}", err);
                }
            }
            Err(err) => println!("{}", err),
        }
        print!("lisp> ");
        io::stdout().flush().unwrap();
    }
    println!("");
    println!("bye!");
}
