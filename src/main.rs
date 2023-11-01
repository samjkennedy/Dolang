pub mod compiler;
pub mod diagnostic;
pub mod lexer;
pub mod parser;
pub mod type_checker_2;

use crate::compiler::*;
use crate::diagnostic::*;
use crate::lexer::*;
use crate::parser::*;
use crate::type_checker_2::*;

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::{fs::File, io::Read, vec};

fn main() {
    let args: Vec<String> = env::args().collect();

    //TODO option flags: -t disable type checking, -c dont delete intermediate c file

    let file_path = &args[1];
    let mut file = File::open(&file_path).expect("could not open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("could not read file");

    let lines = contents.lines().map(str::to_string).collect();
    let mut lexer = Lexer::new(file_path.to_string(), lines);

    let mut diag = StdoutDiagnoster {};

    let mut tokens: Vec<Token> = vec![];
    let mut tok = lexer.next_token();

    while (tok.is_ok() && tok.as_ref().unwrap().kind != TokenKind::EOF) || tok.is_err() {
        match tok {
            Ok(token) => {
                if token.kind != TokenKind::Whitespace {
                    tokens.push(token);
                }
            }
            Err(d) => diag.report(&d.loc, d.severity, &d.message, d.hint.as_deref()),
        }
        tok = lexer.next_token();
    }
    //println!("{:#?}", tokens);
    let mut ops: Vec<Operation> = vec![];
    let tokens_len = tokens.len();
    let eof_loc = &tokens[tokens_len - 1].loc.clone();
    let mut parser = Parser::new(tokens);

    while parser.cursor < tokens_len {
        match parser.parse_next() {
            Ok(op) => ops.push(op),
            Err(d) => diag.report(&d.loc, d.severity, &d.message, d.hint.as_deref()),
        }
    }
    //println!("{:#?}", ops);

    let enable_type_checking = true;

    let mut type_checker = TypeChecker2::new();

    let mut clean = true;
    if enable_type_checking {
        if let Err(d) = type_checker.check_program(&ops) {
            clean = false;
            diag.report(&d.loc, d.severity, &d.message, d.hint.as_deref());
        }
    }

    if !clean {
        return;
    }

    let c_file_name = Path::new(file_path)
        .with_extension("c")
        .file_name()
        .expect("todo")
        .to_owned();
    let out_file = File::create(&c_file_name).expect("could not open file");
    let mut compiler = Compiler::new(out_file);
    if compiler.compile(&ops).is_err() {
        panic!();
    }
    Command::new("gcc")
        .arg("-o")
        .arg(
            Path::new(file_path)
                .with_extension("")
                .file_name()
                .expect("todo"),
        )
        .arg(&c_file_name)
        .output()
        .expect("couldn't make executable");
    fs::remove_file(c_file_name.clone()).expect(&format!(
        "Couldn't remove intermediate c file {:?}",
        c_file_name
    ));
}
