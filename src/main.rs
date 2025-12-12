mod parser;
mod vm;

use std::env;
use std::io::{self, Write};

fn repl(mut vm: vm::Lisp) {
    print!("lisp> ");
    io::stdout().flush().unwrap();

    let mut line = String::new();
    while let Ok(size) = io::stdin().read_line(&mut line)
        && size != 0
    {
        match vm.eval(&line, None) {
            Ok(value) => {
                vm::Lisp::println(&[value]);
            }
            Err(err) => println!("{}", err),
        }
        print!("lisp> ");
        io::stdout().flush().unwrap();
        line.clear();
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
