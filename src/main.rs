use std::env;
use std::fs::{read_to_string};
use std::io::{self, Write};
use std::process;

use rslox::vm::Vm;

struct Cli {
  pub filename: Option<String>,
}

impl Cli {
  pub fn new() -> Cli {
    let args: Vec<String> = env::args().collect();

    match args.as_slice() {
      [_] => Cli { filename: None },
      [_, filename] => Cli { filename: Some(filename.to_string()) },
      _ => {
        eprintln!("usage: rslox [path]");
        process::exit(64);
      }
    }
  }
}

fn main() {
  let cli = Cli::new();
  let mut vm = Vm::new();

  if let Some(filename) = cli.filename {
    let source = read_to_string(filename).expect("Could not read file");
    vm.interpret(source);
  } else {
    loop {
      let mut buffer = String::new();

      print!("> ");
      io::stdout()
        .flush()
        .expect("Could not write to stdout");

      io::stdin()
        .read_line(&mut buffer)
        .expect("Couldn't read input");

      vm.interpret(buffer.to_string());
    }
  }

  process::exit(0);
}
