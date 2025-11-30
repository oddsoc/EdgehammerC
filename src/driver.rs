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
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use crate::air::AirGenerator;
use crate::ast::*;
use crate::lexing::*;
use crate::mir::MirGenerator;
use crate::preprocessing::*;
use crate::semantics::*;
use crate::symtab::SymTab;
use crate::types::*;
use crate::x64::linux::asm;
use crate::x64::linux::mir::MirGenerator as X64MirGenerator;

#[derive(Debug, PartialEq, Clone)]
pub enum Argument {
    Lex,
    Parse,
    Validate,
    Codegen,
    NoLink,
    LinkTo(String),
    OutputAsm,
    OutputTo(PathBuf),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    c_file: PathBuf,
    s_file: PathBuf,
}

/// Returns the `-o` output path if one was specified.
fn output_path(args: &[Argument]) -> Option<&PathBuf> {
    args.iter().find_map(|a| {
        if let Argument::OutputTo(p) = a {
            Some(p)
        } else {
            None
        }
    })
}

/// Parses an option that may be written as `-Xvalue` or `-X value`.
fn parse_option_arg<'a>(
    args: &'a [String],
    i: &mut usize,
    prefix: &str,
) -> &'a str {
    let rest = &args[*i][prefix.len()..];
    if !rest.is_empty() {
        rest
    } else {
        *i += 1;
        if *i < args.len() {
            &args[*i]
        } else {
            eprintln!("missing argument for {}", prefix);
            std::process::exit(1);
        }
    }
}

pub fn parse_args(args: &[String]) -> (Vec<Translation>, Vec<Argument>) {
    let mut arguments: Vec<Argument> = Vec::new();
    let mut files: Vec<PathBuf> = Vec::new();
    let mut i = 1;

    while i < args.len() {
        let arg = args[i].as_str();
        match arg {
            "-S" => arguments.push(Argument::OutputAsm),
            "-c" => arguments.push(Argument::NoLink),
            "--lex" => arguments.push(Argument::Lex),
            "--parse" => arguments.push(Argument::Parse),
            "--validate" => arguments.push(Argument::Validate),
            "--codegen" => arguments.push(Argument::Codegen),
            _ if arg.starts_with("-o") => {
                if output_path(&arguments).is_some() {
                    eprintln!("-o already specified");
                    std::process::exit(1);
                }
                let path = parse_option_arg(args, &mut i, "-o");
                arguments.push(Argument::OutputTo(PathBuf::from(path)));
            }
            _ if arg.starts_with("-l") => {
                let lib = parse_option_arg(args, &mut i, "-l");
                arguments.push(Argument::LinkTo(lib.to_string()));
            }
            _ if arg.starts_with('-') => {
                eprintln!("unknown option: {}", arg);
                std::process::exit(1);
            }
            _ => files.push(PathBuf::from(arg)),
        }
        i += 1;
    }

    if files.is_empty() {
        eprintln!("no source files provided.");
        std::process::exit(1);
    }

    // Derive a default output path when none was given and we are not in a
    // debug-only mode (--codegen / -S just dump intermediate output; no file
    // needs to be produced).
    if output_path(&arguments).is_none()
        && !arguments.contains(&Argument::Codegen)
        && !arguments.contains(&Argument::OutputAsm)
    {
        let default = if files.len() == 1 {
            let c_file = &files[0];
            let parent = c_file.parent().unwrap_or_else(|| {
                eprintln!("invalid C file path: {}", c_file.display());
                std::process::exit(1);
            });
            let stem = c_file.file_stem().unwrap_or_else(|| {
                eprintln!("invalid C file path: {}", c_file.display());
                std::process::exit(1);
            });
            if arguments.contains(&Argument::NoLink) {
                parent.join(format!("{}.o", stem.to_string_lossy()))
            } else {
                parent.join(stem)
            }
        } else {
            PathBuf::from("a.out")
        };
        arguments.push(Argument::OutputTo(default));
    }

    let translations = build_translations(&files, &arguments);
    (translations, arguments)
}

fn build_translations(
    files: &[PathBuf],
    args: &[Argument],
) -> Vec<Translation> {
    let temp_dir = env::temp_dir();
    let asm_in_cwd = args.contains(&Argument::OutputAsm);

    files
        .iter()
        .map(|c_file| {
            let stem = c_file.file_stem().unwrap_or_else(|| {
                eprintln!("invalid C file path: {}", c_file.display());
                std::process::exit(1);
            });
            let s_file = if asm_in_cwd {
                env::current_dir()
                    .unwrap()
                    .join(format!("{}.s", stem.to_string_lossy()))
            } else {
                temp_dir.join(format!("{}.s", stem.to_string_lossy()))
            };
            Translation {
                c_file: c_file.clone(),
                s_file,
            }
        })
        .collect()
}

/// Reads and preprocesses a source file, returning the resulting text.
fn load_source(c_file: &PathBuf) -> String {
    let bytes = fs::read(c_file).unwrap();
    preprocess(bytes).unwrap()
}

/// Reads, preprocesses, and parses a source file.
fn parse_source(c_file: &PathBuf) -> AstStage {
    let text = load_source(c_file);
    let mut symtab = SymTab::new();
    let (mut arena, ast) = {
        let mut parser =
            crate::parsing::Parser::new(&text, &mut symtab).unwrap();
        parser.parse().unwrap()
    };
    arena.source = text;
    AstStage {
        arena,
        root: ast,
        symtab,
    }
}

fn verify(stage: &mut AstStage) {
    let mut annotator = TypeAnnotator::new();
    if let Err(e) = annotator.run(stage) {
        panic!("Type checking failed: {:?}", e);
    }
    let analyser = Analyser::new();
    if let Err(e) = analyser.run(stage) {
        panic!("semantic analysis failed: {:?}", e);
    }
}

fn lex(translations: &[Translation], _arguments: &[Argument]) {
    for translation in translations {
        let text = load_source(&translation.c_file);
        for tok in Tokeniser::new(&text) {
            tok.unwrap();
        }
    }
}

fn parse(translations: &[Translation], arguments: &[Argument]) {
    let validate = arguments.contains(&Argument::Validate);
    for translation in translations {
        let mut stage = parse_source(&translation.c_file);
        if validate {
            verify(&mut stage);
        }
    }
}

fn codegen(translations: &[Translation], arguments: &[Argument]) {
    let debug = arguments.contains(&Argument::Codegen);
    let emit_asm = arguments.contains(&Argument::OutputAsm);
    let emit = emit_asm || output_path(arguments).is_some();
    let mut s_files: Vec<String> = Vec::new();

    for translation in translations {
        let mut stage = parse_source(&translation.c_file);
        verify(&mut stage);

        let mut irgen = crate::air::tac::TacGenerator::new();
        let ir = irgen.lower(stage);
        if debug {
            println!("Tac: {:#?}", &ir);
        }

        let mut x64gen = X64MirGenerator::new();
        let asm = x64gen.lower(ir);
        if debug {
            println!("ASM: {:#?}", asm);
        }

        if emit {
            asm::emit(translation.s_file.to_str().unwrap(), &asm);
        }
        s_files.push(translation.s_file.to_str().unwrap().to_string());
    }

    if let Some(output) = output_path(arguments) {
        let link = !arguments.contains(&Argument::NoLink);
        assemble_cc(&s_files, output.to_str().unwrap(), link, arguments);
    }

    if !emit_asm {
        for s_file in &s_files {
            let _ = fs::remove_file(s_file);
        }
    }
}

#[allow(unused)]
fn preprocess_cc(c_file: &str, i_file: &str) {
    Command::new("cc")
        .arg("-E")
        .arg("-P")
        .arg(c_file)
        .arg("-o")
        .arg(i_file)
        .status()
        .expect(&format!("failed to preprocess {}", c_file));
}

fn assemble_cc(
    s_files: &[String],
    output: &str,
    link: bool,
    args: &[Argument],
) {
    let mut cmd = Command::new("cc");

    if !link {
        cmd.arg("-c");
    }

    cmd.args(s_files).arg("-no-pie").arg("-o").arg(output);

    for lib in args.iter().filter_map(|a| {
        if let Argument::LinkTo(lib) = a {
            Some(lib.as_str())
        } else {
            None
        }
    }) {
        cmd.arg(format!("-l{}", lib));
    }

    cmd.status().expect("failed to assemble");
}

pub fn run(translations: &[Translation], arguments: &[Argument]) {
    if arguments.contains(&Argument::Codegen) {
        codegen(translations, arguments);
    } else if arguments.contains(&Argument::Validate)
        || arguments.contains(&Argument::Parse)
    {
        parse(translations, arguments);
    } else if arguments.contains(&Argument::Lex) {
        lex(translations, arguments);
    } else {
        codegen(translations, arguments);
    }
}
