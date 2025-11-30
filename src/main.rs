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

#![cfg_attr(feature = "simd", feature(portable_simd))]

#[cfg(feature = "tracing")]
use tracing_subscriber::prelude::*;
#[cfg(feature = "tracing")]
use tracing_tree::HierarchicalLayer;

use std::env;

mod air;
mod ast;
mod driver;
mod errors;
mod expr;
mod lexing;
mod mir;
mod parsing;
mod preprocessing;
mod semantics;
mod symtab;
mod types;
mod x64;

fn main() -> Result<(), ()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!(
            "Usage: {} [--lex|--parse|--validate|--codegen] [-S] [-o <output>] <c-file> [<c-file> ...]",
            args[0]
        );
        std::process::exit(1);
    }

    #[cfg(feature = "tracing")]
    let subscriber =
        tracing_subscriber::registry().with(HierarchicalLayer::new(2)); // 2 = indentation spaces
    #[cfg(feature = "tracing")]
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let (translations, arguments) = driver::parse_args(&args);
    driver::run(&translations, &arguments);

    Ok(())
}
