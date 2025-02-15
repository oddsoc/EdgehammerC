//  SPDX-License-Identifier: MIT
/*
 *  Copyright (c) 2025 Andrew Scott-Jones <andrew@edgehammer.io>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a 
 *  copy of this software and associated documentation files (the "Software"), 
 *  to deal in the Software without restriction, including without limitation 
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense, 
 *  and/or sell copies of the Software, and to permit persons to whom the 
 *  Software is furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in 
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
 *  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
 *  DEALINGS IN THE SOFTWARE.
 */

use std::env;
use std::path::PathBuf;
use std::process::Command;

use crate::codegen;
use crate::ir;
use crate::lexer;
use crate::parser;
use crate::x86_64;

#[derive(Debug, PartialEq, Clone)]
pub enum Argument {
    Lex,
    Parse,
    Codegen,
    OutputAsm,
    OutputTo(PathBuf),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    c_file: PathBuf,
    i_file: PathBuf,
    s_file: PathBuf,
}

fn build_translations(
    files: Vec<PathBuf>,
    args: &Vec<Argument>,
) -> Vec<Translation> {
    let mut translations: Vec<Translation> = Vec::new();
    let temp_dir = env::temp_dir();

    for c_file in files {
        if let Some(stem) = c_file.file_stem() {
            let i_file = temp_dir.join(format!("{}.i", stem.to_string_lossy()));

            let s_file =
                if args.iter().any(|i| matches!(i, Argument::OutputAsm)) {
                    env::current_dir()
                        .unwrap()
                        .join(format!("{}.s", stem.to_string_lossy()))
                } else {
                    temp_dir.join(format!("{}.s", stem.to_string_lossy()))
                };

            translations.push(Translation {
                c_file,
                i_file,
                s_file,
            });
        } else {
            eprintln!("invalid C file path: {}", c_file.display());
            std::process::exit(1);
        }
    }

    translations
}

pub fn process_args(args: &[String]) -> (Vec<Translation>, Vec<Argument>) {
    let mut arguments: Vec<Argument> = Vec::new();
    let mut o_arg = false;
    let mut s_arg = false;
    let mut codegen_arg = false;
    let mut files = Vec::new();
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "-S" => {
                arguments.push(Argument::OutputAsm);
                s_arg = true;
            }
            "-o" => {
                if o_arg {
                    eprintln!("-o already specified");
                    std::process::exit(1);
                }
                i += 1;
                if i < args.len() {
                    arguments.push(Argument::OutputTo(PathBuf::from(&args[i])));
                    o_arg = true;
                } else {
                    eprintln!("missing argument for -o");
                    std::process::exit(1);
                }
            }
            "--lex" => {
                arguments.push(Argument::Lex);
            }
            "--parse" => {
                arguments.push(Argument::Parse);
            }
            "--codegen" => {
                arguments.push(Argument::Codegen);
                codegen_arg = true;
            }
            _ => files.push(PathBuf::from(&args[i])),
        }
        i += 1;
    }

    if files.is_empty() {
        eprintln!("no source files provided.");
        std::process::exit(1);
    }

    if !o_arg && !codegen_arg && !s_arg {
        if files.len() == 1 {
            let c_file = &files[0];
            if let Some(path) = c_file.parent() {
                if let Some(stem) = c_file.file_stem() {
                    arguments.push(Argument::OutputTo(PathBuf::from(
                        path.join(stem),
                    )));
                } else {
                    eprintln!("invalid C file path: {}", c_file.display());
                    std::process::exit(1);
                }
            } else {
                eprintln!("invalid C file path: {}", c_file.display());
                std::process::exit(1);
            }
        } else {
            arguments.push(Argument::OutputTo(PathBuf::from("a.out")));
        }
    }

    let translations = build_translations(files, &arguments);

    (translations, arguments)
}

fn lex_translation(
    translation: &Translation,
    _arguments: &[Argument],
) -> Result<(), ()> {
    let c_file = translation.c_file.to_str().unwrap();
    let i_file = translation.i_file.to_str().unwrap();
    preprocess_gcc(c_file, i_file);

    let mut lexer = lexer::Lexer::new(i_file);
    let mut token = lexer.lex().unwrap();

    while token.kind != lexer::TokenKind::EOF {
        println!("{:?}", token);

        if token.kind == lexer::TokenKind::Bad {
            return Err(());
        }

        token = lexer.lex().unwrap();
    }

    Ok(())
}

fn lex(translations: &[Translation], arguments: &[Argument]) {
    for translation in translations {
        lex_translation(translation, arguments).unwrap();
    }
}

fn parse_translation(translation: &Translation, _arguments: &[Argument]) {
    let c_file = translation.c_file.to_str().unwrap();
    let i_file = translation.i_file.to_str().unwrap();
    preprocess_gcc(c_file, i_file);

    let mut parser = parser::Parser::new(i_file);
    let ast = parser.parse().unwrap();
    println!("{:#?}", ast);
}

fn parse(translations: &[Translation], arguments: &[Argument]) {
    for translation in translations {
        parse_translation(translation, arguments);
    }
}

fn codegen_translation(translation: &Translation, arguments: &[Argument]) {
    let c_file = translation.c_file.to_str().unwrap();
    let i_file = translation.i_file.to_str().unwrap();
    preprocess_gcc(c_file, i_file);

    let mut parser = parser::Parser::new(i_file);
    let ast = parser.parse().unwrap();
    let ir = ir::generate(ast);

    if arguments.iter().any(|i| matches!(i, Argument::Codegen)) {
        println!("TAC: {:#?}", ir);
    }

    let asm = codegen::generate(ir);

    if arguments.iter().any(|i| matches!(i, Argument::Codegen)) {
        println!("ASM: {:#?}", asm);
    }

    if arguments.iter().any(|i| matches!(i, Argument::OutputTo(_))) {
        let asm_filename = translation.s_file.to_str().unwrap();
        x86_64::emit(asm_filename, asm);
    }
}

fn codegen(translations: &[Translation], arguments: &[Argument]) {
    let mut s_files: Vec<String> = Vec::new();

    for translation in translations {
        codegen_translation(translation, arguments);
        let s_file = translation.s_file.to_str().unwrap();
        s_files.push(s_file.to_string());
    }

    match arguments
        .iter()
        .find(|i| matches!(i, Argument::OutputTo(_)))
    {
        Some(Argument::OutputTo(output)) => {
            assemble_gcc(&s_files, output.to_str().unwrap());
        }
        _ => {}
    }

    if !arguments.iter().any(|i| matches!(i, Argument::OutputAsm)) {
        for s_file in s_files {
            let _ = std::fs::remove_file(s_file);
        }
    }
}

fn preprocess_gcc(c_file: &str, i_file: &str) {
    Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(c_file)
        .arg("-o")
        .arg(i_file)
        .status()
        .expect("failed to preprocess {c_file}");
}

fn assemble_gcc(s_files: &Vec<String>, o_file: &str) {
    Command::new("gcc")
        .args(s_files.iter().map(|s| s.as_str()))
        .arg("-o")
        .arg(o_file)
        .status()
        .expect("failed to assemble {filename}");
}

pub fn run(translations: &[Translation], arguments: &[Argument]) {
    if arguments.iter().any(|i| matches!(i, Argument::Codegen)) {
        codegen(translations, arguments);
    } else if arguments.iter().any(|i| matches!(i, Argument::Parse)) {
        parse(translations, arguments);
    } else if arguments.iter().any(|i| matches!(i, Argument::Lex)) {
        lex(translations, arguments);
    } else {
        codegen(translations, arguments);
    }
}
