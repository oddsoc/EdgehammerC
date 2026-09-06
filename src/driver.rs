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

use crate::errors::{report_error, report_errors};
use crate::preprocessing::*;
use crate::tokenising::*;
use crate::{
    air::AirGenerator, air::tac::TacGenerator,
    asm::rv64::linux::asm as rv64_asm, asm::x86_64::linux::asm,
    mir::MirGenerator, mir::abi::set_abi, mir::rv64::lp64d::Lp64dAbi,
    mir::rv64::lp64d::mir::MirGenerator as Rv64Codegen,
    mir::x86_64::sysv::SysvAbi,
    mir::x86_64::sysv::mir::MirGenerator as Codegen,
};
use crate::{ast::*, parsing::Parser, semantics::*, symtab::SymTab, types::*};

#[derive(Debug, Default, Clone)]
pub struct Config {
    pub tokenise: bool,
    pub parse: bool,
    pub validate: bool,
    pub codegen: bool,
    pub no_link: bool,
    pub output_asm: bool,
    pub output_dir: Option<PathBuf>,
    pub link_to: Vec<String>,
    pub target: String,
}

fn usage(program: &str) {
    eprintln!("Usage: {} [options] <c-file> [<c-file> ...]", program);
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --lex              Only lex the input file(s)");
    eprintln!("  --parse            Only parse the input file(s)");
    eprintln!("  --validate         Parse and validate the input file(s)");
    eprintln!("  --codegen          Generate code and print debug output");
    eprintln!("  -S                 Produce assembly output (.s) files");
    eprintln!("  -c                 Compile only; do not link");
    eprintln!("  -o <output>        Write output to <output>");
    eprintln!("  -l <library>       Link against <library>");
    eprintln!(
        "  --target <arch>    Target architecture: x86_64 (default) or rv64"
    );
    eprintln!("  --help             Show this help message and exit");
}

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    c_file: PathBuf,
    s_file: PathBuf,
}

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

pub fn parse_args(args: &[String]) -> (Vec<Translation>, Config) {
    let mut config = Config::default();
    config.target = "x86_64".to_string();
    let mut files: Vec<PathBuf> = Vec::new();
    let mut i = 1;

    while i < args.len() {
        let arg = args[i].as_str();
        match arg {
            "-S" => config.output_asm = true,
            "-c" => config.no_link = true,
            "--lex" => config.tokenise = true,
            "--parse" => config.parse = true,
            "--validate" => config.validate = true,
            "--codegen" => config.codegen = true,
            "--help" => {
                let program = args.first().map_or("edgehammer", |s| s.as_str());
                usage(program);
                std::process::exit(0);
            }
            _ if arg.starts_with("-o") => {
                if config.output_dir.is_some() {
                    eprintln!("-o already specified");
                    std::process::exit(1);
                }
                let path = parse_option_arg(args, &mut i, "-o");
                config.output_dir = Some(PathBuf::from(path));
            }
            _ if arg.starts_with("-l") => {
                let lib = parse_option_arg(args, &mut i, "-l");
                config.link_to.push(lib.to_string());
            }
            _ if arg == "--target" => {
                let target = parse_option_arg(args, &mut i, "--target");
                if target != "x86_64" && target != "rv64" {
                    eprintln!("unsupported target: {}", target);
                    std::process::exit(1);
                }
                config.target = target.to_string();
            }
            _ if arg.starts_with("--target=") => {
                let target = &arg["--target=".len()..];
                if target != "x86_64" && target != "rv64" {
                    eprintln!("unsupported target: {}", target);
                    std::process::exit(1);
                }
                config.target = target.to_string();
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

    if config.output_dir.is_none() && !config.codegen && !config.output_asm {
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
            if config.no_link {
                parent.join(format!("{}.o", stem.to_string_lossy()))
            } else {
                parent.join(stem)
            }
        } else {
            PathBuf::from("a.out")
        };
        config.output_dir = Some(default);
    }

    let translations = build_translations(&files, &config);
    (translations, config)
}

fn build_translations(files: &[PathBuf], config: &Config) -> Vec<Translation> {
    let temp_dir = env::temp_dir();
    let asm_in_cwd = config.output_asm;

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

fn load_source(c_file: &PathBuf) -> String {
    let bytes = fs::read(c_file).unwrap();
    preprocess(bytes).unwrap()
}

fn parse_source(c_file: &PathBuf) -> Option<AstStage> {
    let text = load_source(c_file);
    let mut symtab = SymTab::new();
    let (mut arena, ast) = match Parser::new(&text, &mut symtab) {
        Ok(mut parser) => match parser.parse() {
            Ok(result) => result,
            Err(errors) => {
                let filename = c_file.file_name().unwrap().to_str().unwrap();
                report_errors(&errors, &text, filename);
                std::process::exit(1);
            }
        },
        Err(e) => {
            let filename = c_file.file_name().unwrap().to_str().unwrap();
            report_error(&e, &text, filename);
            std::process::exit(1);
        }
    };
    arena.source = text;
    Some(AstStage {
        arena,
        root: ast,
        symtab,
    })
}

fn verify(stage: &mut AstStage, filename: &str) {
    let mut annotator = TypeAnnotator::new();
    if let Err(e) = annotator.run(stage) {
        report_error(&e, &stage.arena.source, filename);
        std::process::exit(1);
    }
    let analyser = Analyser::new();
    if let Err(e) = analyser.run(stage) {
        report_error(&e, &stage.arena.source, filename);
        std::process::exit(1);
    }
}

fn tokenise(translations: &[Translation], _config: &Config) {
    for translation in translations {
        let text = load_source(&translation.c_file);
        let mut tokeniser = Tokeniser::new(&text);
        let mut nr_tokens: usize = 0;
        for tok in &mut tokeniser {
            tok.unwrap();
            nr_tokens += 1;
        }
        println!("{} tokens", nr_tokens);
    }
}

fn parse(translations: &[Translation], config: &Config) {
    let validate = config.validate;
    for translation in translations {
        if let Some(mut stage) = parse_source(&translation.c_file)
            && validate
        {
            let filename =
                translation.c_file.file_name().unwrap().to_str().unwrap();
            verify(&mut stage, filename);
        }
    }
}

fn codegen(translations: &[Translation], config: &Config) {
    let debug = config.codegen;
    let emit_asm = config.output_asm;
    let emit = emit_asm || config.output_dir.is_some();
    let mut s_files: Vec<String> = Vec::new();

    for translation in translations {
        let Some(mut stage) = parse_source(&translation.c_file) else {
            continue;
        };
        let filename =
            translation.c_file.file_name().unwrap().to_str().unwrap();
        verify(&mut stage, filename);

        let mut irgen = TacGenerator::new();
        let ir = irgen.lower(stage);
        if debug {
            println!("Tac: {:#?}", ir);
        }

        let emit_rv64 = config.target == "rv64";

        if emit_rv64 {
            let mut codegen = Rv64Codegen::new();
            let mir = codegen.lower(ir);
            if debug {
                println!("Mir: {:#?}", mir);
            }

            if emit {
                rv64_asm::emit(translation.s_file.to_str().unwrap(), &mir);
            }
        } else {
            let mut codegen = Codegen::new();
            let mir = codegen.lower(ir);
            if debug {
                println!("Mir: {:#?}", mir);
            }

            if emit {
                asm::emit(translation.s_file.to_str().unwrap(), &mir);
            }
        }

        s_files.push(translation.s_file.to_str().unwrap().to_string());
    }

    if let Some(ref output) = config.output_dir {
        let link = !config.no_link;
        assemble_cc(&s_files, output.to_str().unwrap(), link, config);
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
        .unwrap_or_else(|_| panic!("failed to preprocess {}", c_file));
}

fn assemble_cc(s_files: &[String], output: &str, link: bool, config: &Config) {
    let mut cmd = Command::new("cc");

    if !link {
        cmd.arg("-c");
    }

    cmd.args(s_files).arg("-no-pie").arg("-o").arg(output);

    for lib in &config.link_to {
        cmd.arg(format!("-l{}", lib));
    }

    cmd.status().expect("failed to assemble");
}

pub fn run(translations: &[Translation], config: &Config) {
    match config.target.as_str() {
        "rv64" => set_abi(&Lp64dAbi),
        _ => set_abi(&SysvAbi),
    }

    if config.codegen {
        codegen(translations, config);
    } else if config.validate || config.parse {
        parse(translations, config);
    } else if config.tokenise {
        tokenise(translations, config);
    } else {
        codegen(translations, config);
    }
}
