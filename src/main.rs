#![allow(unused_braces)]

use std::io::Read;
use atty::Stream;
use clap::Clap;

mod ast;
mod lexer;
mod parser;
//mod codegen;

fn main() {
    let opts: Opts = Opts::parse();

    match opts.infile {
        Some(fname) => { eprintln!("you provided an input file named '{}'.", fname); },
        None => { 
            eprintln!("you didn't provide an input file.");
            if atty::is(Stream::Stdin) {
                eprintln!("additionally, stdin is a tty.");
            }
        },
    }
     match opts.outfile {
        Some(fname) => { eprintln!("you provided an output file named '{}'.", fname); },
        None => {
            eprintln!("you didn't provide an output file.");
            if atty::is(Stream::Stdout) {
                eprintln!("additionally, stdout is a tty. no code will be output.");
            }
        },
    }

    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();
    // lex the input, and print the tokens as it munches them
    let lexer = lexer::Lexer::new(&s).inspect(|tok| eprintln!("tok: {:?}", tok));
    // parse the tokens
    let program = parser::parse(lexer).unwrap();
    // print the ast
    eprintln!("prog: {:?}", program);
    // generate the code!
    //let codegen = codegen::CodeGen::new("main");
    //let code = codegen.gen(&program).unwrap();
}

#[derive(Clap)]
#[clap(version = "0.1", author = "Micah Switzer")]
struct Opts {
    #[clap(short = "o", long = "outfile")]
    outfile: Option<String>,
    infile: Option<String>,
}
