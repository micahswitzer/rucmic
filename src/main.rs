#![allow(unused_braces)]

use std::io::{ Read, Write };
use std::fs::File;
use std::error::Error;
use atty::Stream;
use clap::Clap;

use inkwell::passes::PassManager;
use inkwell::module::Module;

mod ast;
mod lexer;
mod parser;
mod codegen;

fn main() -> Result<(), Box<dyn Error>> {
    let opts: Opts = Opts::parse();

    let mut infile: Box<dyn Read> = match opts.infile {
        Some(fname) => {
            eprintln!("you provided an input file named '{}'.", fname);
            Box::new(File::open(fname)?)
        },
        None => { 
            eprintln!("you didn't provide an input file.");
            if atty::is(Stream::Stdin) {
                eprintln!("additionally, stdin is a tty. this is probably not what you wanted.");
            }
            Box::new(std::io::stdin())
        },
    };
    let mut outfile: Box<dyn Write> = match opts.outfile {
        Some(fname) => {
            eprintln!("you provided an output file named '{}'.", fname);
            Box::new(File::create(fname)?)
        },
        None => {
            eprintln!("you didn't provide an output file.");
            if atty::is(Stream::Stdout) {
                eprintln!("additionally, stdout is a tty. no code will be output.");
                // this is a dummy sink for the output data
                //Box::new(std::io::sink())
            }
            else { }
            // always output to stdout for testing purposes
            Box::new(std::io::stdout())
        },
    };

    let mut s = String::new();
    infile.read_to_string(&mut s)?;
    // lex the input, and print the tokens as it munches them
    let lexer = lexer::Lexer::new(&s).inspect(|tok| if cfg!(debug_assertions) { eprintln!("tok: {:?}", tok) });
    // parse the tokens
    let program = parser::parse(lexer)?;
    // print the ast
    #[cfg(debug_assertions)]
    eprintln!("prog: {:?}", program);
    // generate the code!
    let context = inkwell::context::Context::create();
    let mut codegen = codegen::CodeGen::new(&context, "main");
    // unwrap a present from the code generator
    codegen.compile_program(&program).unwrap();

    eprintln!("Unoptimized:");
    codegen.module.print_to_stderr();

    let fpm: PassManager<Module> = PassManager::create(());
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.run_on(&codegen.module);

    let llvm_str = codegen.module.print_to_string();
    outfile.write_fmt(format_args!("{}", llvm_str.to_string()))?;

    // ok...we made it
    Ok(())
}

#[derive(Clap)]
#[clap(version = "0.1", author = "Micah Switzer")]
struct Opts {
    #[clap(short = "o", long = "outfile")]
    outfile: Option<String>,
    infile: Option<String>,
}
