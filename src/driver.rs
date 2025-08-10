//  SPDX-License-Identifier: MIT
/*
 *  Copyright (c) 2025 Andrew Scott-Jones
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

use crate::ast::*;
use crate::codegen::CodeGenerator;
use crate::ir::IrGenerator;
use crate::lexer;
use crate::parser;
use crate::semantics::*;
use crate::types::*;
use crate::x64::linux::codegen::CodeGenerator as X64CodeGenerator;
use crate::x64::linux::out;

#[derive(Debug, PartialEq, Clone)]
pub enum Argument {
    Lex,
    Parse,
    Validate,
    Codegen,
    NoLink,
    OutputAsm,
    OutputTo(PathBuf),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    c_file: PathBuf,
    i_file: PathBuf,
    s_file: PathBuf,
    o_file: PathBuf,
}

fn build_translations(
    files: Vec<PathBuf>,
    args: &[Argument],
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

            let o_file = env::current_dir()
                .unwrap()
                .join(format!("{}.o", stem.to_string_lossy()));

            translations.push(Translation {
                c_file,
                i_file,
                s_file,
                o_file,
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
            "-c" => {
                arguments.push(Argument::NoLink);
            }
            "--lex" => {
                arguments.push(Argument::Lex);
            }
            "--parse" => {
                arguments.push(Argument::Parse);
            }
            "--validate" => {
                arguments.push(Argument::Validate);
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
                    if arguments.iter().any(|i| matches!(i, Argument::NoLink)) {
                        arguments.push(Argument::OutputTo(
                            path.join(format!("{}.o", stem.to_string_lossy())),
                        ));
                    } else {
                        arguments.push(Argument::OutputTo(path.join(stem)));
                    }
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

fn verify(ast: Vec<AstRef>) {
    let mut annotator = Annotator::new();
    let mut res = annotator.run(&ast);

    if res.is_err() {
        panic!("Type checking failed: {:?}", res);
    }

    let analyser = Analyser::new();

    res = analyser.run(&ast);

    if res.is_err() {
        panic!("semantic analysis failed: {:?}", res);
    }
}

fn parse_translation(translation: &Translation, arguments: &[Argument]) {
    let c_file = translation.c_file.to_str().unwrap();
    let i_file = translation.i_file.to_str().unwrap();
    preprocess_gcc(c_file, i_file);

    let validate = arguments.iter().any(|i| matches!(i, Argument::Validate));

    let mut parser = parser::Parser::new(i_file);

    let ast = parser.parse().unwrap();

    if validate {
        verify(ast.clone());
    }
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

    verify(ast.clone());

    let mut irgen: crate::ir::tac::TacGenerator =
        crate::ir::tac::TacGenerator::new();

    let ir = irgen.lower(ast.clone());

    if arguments.iter().any(|i| matches!(i, Argument::Codegen)) {
        println!("Tac: {:#?}", &ir);
    }

    let mut codegen = X64CodeGenerator::new();
    let asm = codegen.lower(ir);

    if arguments.iter().any(|i| matches!(i, Argument::Codegen)) {
        println!("ASM: {:#?}", asm);
    }

    if arguments.iter().any(|i| matches!(i, Argument::OutputAsm))
        || arguments.iter().any(|i| matches!(i, Argument::OutputTo(_)))
    {
        let asm_filename = translation.s_file.to_str().unwrap();
        out::emit(asm_filename, asm);
    }
}

fn codegen(translations: &[Translation], arguments: &[Argument]) {
    let mut s_files: Vec<String> = Vec::new();

    for translation in translations {
        codegen_translation(translation, arguments);
        let s_file = translation.s_file.to_str().unwrap();
        s_files.push(s_file.to_string());
    }

    if let Some(Argument::OutputTo(output)) = arguments
        .iter()
        .find(|i| matches!(i, Argument::OutputTo(_)))
    {
        let link = !arguments.iter().any(|i| matches!(i, Argument::NoLink));

        assemble_gcc(&s_files, output.to_str().unwrap(), link);
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

fn assemble_gcc(s_files: &[String], o_file: &str, link: bool) {
    let mut cmd = Command::new("gcc");

    if !link {
        cmd.arg("-c");
    }

    cmd.args(s_files.iter().map(|s| s.as_str()))
        .arg("-no-pie")
        .arg("-o")
        .arg(o_file);

    cmd.status().expect("failed to assemble {filename}");
}

pub fn run(translations: &[Translation], arguments: &[Argument]) {
    if arguments.iter().any(|i| matches!(i, Argument::Codegen)) {
        codegen(translations, arguments);
    } else if arguments.iter().any(|i| matches!(i, Argument::Validate))
        || arguments.iter().any(|i| matches!(i, Argument::Parse))
    {
        parse(translations, arguments);
    } else if arguments.iter().any(|i| matches!(i, Argument::Lex)) {
        lex(translations, arguments);
    } else {
        codegen(translations, arguments);
    }
}
