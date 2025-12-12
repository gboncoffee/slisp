mod parser;
mod vm;

use std::env;
use std::io::{self, BufRead, Write};

fn repl(mut vm: vm::Lisp) {
    print!("lisp> ");
    io::stdout().flush().unwrap();
    for line in io::stdin().lock().lines() {
        match vm.eval(&line.unwrap(), None) {
            Ok(value) => {
                vm::Lisp::println(&[value]);
            }
            Err(err) => println!("{}", err),
        }
        print!("lisp> ");
        io::stdout().flush().unwrap();
    }
    println!("");
    println!("bye!");
}

fn main() {
    let mut vm = vm::Lisp::new();
    let args = env::args().collect::<Vec<String>>();
    if args.len() == 1 {
        repl(vm);
    } else {
        for file in args[1..].iter() {
            let content = std::fs::read_to_string(file).unwrap();
            match vm.eval(&content, None) {
                Ok(value) => {
                    vm::Lisp::println(&[value]);
                }
                Err(err) => println!("{}", err),
            }
        }
    }
}
