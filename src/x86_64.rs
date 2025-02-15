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

use std::io::Write;
use std::rc::Rc;

use crate::codegen::Code;

fn emit_instruction(file: &mut std::fs::File, instr: Rc<Code>) {
    match instr.as_ref() {
        Code::Imm(val) => {
            writeln!(file, "\tmovl ${}, %eax", val).unwrap();
        }

        Code::Ret => {
            writeln!(file, "\tret").unwrap();
        }

        _ => {
            unimplemented!();
        }
    }
}

pub fn emit(filename: &str, code: Vec<Rc<Code>>) {
    let mut file = std::fs::File::create(filename).unwrap();

    for instr in code {
        match &*instr {
            Code::Function { name, code } => {
                writeln!(file, "\t.globl {}", name).unwrap();
                writeln!(file, "{}:", name).unwrap();

                for instr in code {
                    emit_instruction(&mut file, instr.clone());
                }
            }

            _ => {
                emit_instruction(&mut file, instr.clone());
            }
        }
    }

    writeln!(file, "\n.section .note.GNU-stack,\"\",@progbits").unwrap();
}
