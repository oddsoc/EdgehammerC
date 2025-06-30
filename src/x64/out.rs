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

use std::cell::RefCell;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

use crate::x64::codegen::{Code, Register};

use super::codegen::CondCode;

fn cc_to_str(cc: &CondCode) -> String {
    match cc {
        CondCode::E => "e",
        CondCode::NE => "ne",
        CondCode::L => "l",
        CondCode::LE => "le",
        CondCode::G => "g",
        CondCode::GE => "ge",
    }
    .to_string()
}

fn code_to_str(code: Rc<RefCell<Code>>) -> String {
    match &*code.borrow() {
        Code::Imm { val, .. } => format!("${}", val),
        Code::Var { off, .. } => {
            format!("{}(%rbp)", -off)
        }

        Code::Label(idx) => format!(".L{}", idx),
        Code::FunctionRef(name, defined) => {
            if *defined {
                format!("{}", name)
            } else {
                format!("{}@PLT", name)
            }
        }

        Code::Reg { reg, size, .. } => match &*reg {
            Register::RAX => match size {
                1 => "%al".to_string(),
                2 => "%ax".to_string(),
                4 => "%eax".to_string(),
                8 => "%rax".to_string(),
                _ => unreachable!(),
            },
            Register::RBX => match size {
                1 => "%bl".to_string(),
                2 => "%bx".to_string(),
                4 => "%ebx".to_string(),
                8 => "%rbx".to_string(),
                _ => unreachable!(),
            },
            Register::RCX => match size {
                1 => "%cl".to_string(),
                2 => "%cx".to_string(),
                4 => "%ecx".to_string(),
                8 => "%rcx".to_string(),
                _ => unreachable!(),
            },
            Register::RDX => match size {
                1 => "%dl".to_string(),
                2 => "%dx".to_string(),
                4 => "%edx".to_string(),
                8 => "%rdx".to_string(),
                _ => unreachable!(),
            },
            Register::RSI => match size {
                1 => "%sil".to_string(),
                2 => "%si".to_string(),
                4 => "%esi".to_string(),
                8 => "%rsi".to_string(),
                _ => unreachable!(),
            },
            Register::RDI => match size {
                1 => "%dil".to_string(),
                2 => "%di".to_string(),
                4 => "%edi".to_string(),
                8 => "%rdi".to_string(),
                _ => unreachable!(),
            },
            Register::RSP => match size {
                1 => "%spl".to_string(),
                2 => "%sp".to_string(),
                4 => "%esp".to_string(),
                8 => "%rsp".to_string(),
                _ => unreachable!(),
            },
            Register::RBP => match size {
                1 => "%bpl".to_string(),
                2 => "%bp".to_string(),
                4 => "%ebp".to_string(),
                8 => "%rbp".to_string(),
                _ => unreachable!(),
            },
            Register::R8 => match size {
                1 => "%r8b".to_string(),
                2 => "%r8w".to_string(),
                4 => "%r8d".to_string(),
                8 => "%r8".to_string(),
                _ => unreachable!(),
            },
            Register::R9 => match size {
                1 => "%r9b".to_string(),
                2 => "%r9w".to_string(),
                4 => "%r9d".to_string(),
                8 => "%r9".to_string(),
                _ => unreachable!(),
            },
            Register::R10 => match size {
                1 => "%r10b".to_string(),
                2 => "%r10w".to_string(),
                4 => "%r10d".to_string(),
                8 => "%r10".to_string(),
                _ => unreachable!(),
            },
            Register::R11 => match size {
                1 => "%r11b".to_string(),
                2 => "%r11w".to_string(),
                4 => "%r11d".to_string(),
                8 => "%r11".to_string(),
                _ => unreachable!(),
            },
            Register::R12 => match size {
                1 => "%r12b".to_string(),
                2 => "%r12w".to_string(),
                4 => "%r12d".to_string(),
                8 => "%r12".to_string(),
                _ => unreachable!(),
            },
            Register::R13 => match size {
                1 => "%r13b".to_string(),
                2 => "%r13w".to_string(),
                4 => "%r13d".to_string(),
                8 => "%r13".to_string(),
                _ => unreachable!(),
            },
            Register::R14 => match size {
                1 => "%r14b".to_string(),
                2 => "%r14w".to_string(),
                4 => "%r14d".to_string(),
                8 => "%r14".to_string(),
                _ => unreachable!(),
            },
            Register::R15 => match size {
                1 => "%r15b".to_string(),
                2 => "%r15w".to_string(),
                4 => "%r15d".to_string(),
                8 => "%r15".to_string(),
                _ => unreachable!(),
            },
        },

        _ => unreachable!(),
    }
}

fn emit_operand(file: &mut std::fs::File, operand: Rc<RefCell<Code>>) {
    write!(file, "{}", code_to_str(operand)).unwrap();
}

fn op_suffix(size: usize) -> String {
    match size {
        1 => "b".to_string(),
        2 => "s".to_string(),
        4 => "l".to_string(),
        8 => "q".to_string(),
        _ => unreachable!(),
    }
}

fn emit_op(file: &mut std::fs::File, instr: Rc<RefCell<Code>>) {
    match &*instr.borrow() {
        Code::Mov(src, dst, size) => {
            write!(file, "\tmov{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::MovSignExt(src, dst, size) => {
            write!(file, "\tmovsx{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::MovZeroExt(src, dst, size) => {
            write!(file, "\tmovzx{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Neg(dst, size) => {
            write!(file, "\tneg{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Not(dst, size) => {
            write!(file, "\tnot{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::IMul(src, dst, size) => {
            write!(file, "\timul{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::IDiv(src, size) => {
            write!(file, "\tidiv{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            writeln!(file, "").unwrap();
        }

        Code::Add(src, dst, size) => {
            write!(file, "\tadd{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Sub(src, dst, size) => {
            write!(file, "\tsub{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Shl(src, dst, size) => {
            write!(file, "\tshl{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Sar(src, dst, size) => {
            write!(file, "\tsar{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::And(src, dst, size) => {
            write!(file, "\tand{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Or(src, dst, size) => {
            write!(file, "\tor{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Xor(src, dst, size) => {
            write!(file, "\txor{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Cdq => {
            writeln!(file, "\tcdq").unwrap();
        }

        Code::Ret => {
            writeln!(file, "\tmovq\t%rbp, %rsp").unwrap();
            writeln!(file, "\tpopq\t%rbp").unwrap();
            writeln!(file, "\tret").unwrap();
        }

        Code::Cmp(src, dst, size) => {
            write!(file, "\tcmp{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Jmp(label) => {
            write!(file, "\tjmp\t").unwrap();
            emit_operand(file, label.clone());
            writeln!(file, "").unwrap();
        }

        Code::JmpCC { cond, label } => {
            let cc = cc_to_str(cond);
            write!(file, "\tj{} \t", cc).unwrap();
            emit_operand(file, label.clone());
            writeln!(file, "").unwrap();
        }

        Code::SetCC { cond, dst } => {
            let cc = cc_to_str(cond);
            write!(file, "\tset{}\t", cc).unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Code::Label(idx) => {
            writeln!(file, ".L{}:", *idx).unwrap();
        }

        Code::Call(func) => {
            write!(file, "\tcall\t").unwrap();
            emit_operand(file, func.clone());
            writeln!(file, "").unwrap();
        }

        Code::Push(val) => {
            write!(file, "\tpushq\t").unwrap();
            emit_operand(file, val.clone());
            writeln!(file, "").unwrap();
        }

        Code::PushBytes(n) => {
            write!(file, "\tsubq\t${}, %rsp\n", n).unwrap();
        }

        Code::PopBytes(n) => {
            write!(file, "\taddq\t${}, %rsp\n", n).unwrap();
        }

        _ => {
            println!("Cannot emit {:?}", instr);
            unreachable!();
        }
    }
}

pub fn emit(filepath: &str, code: Vec<Rc<RefCell<Code>>>) {
    let mut file = std::fs::File::create(filepath).unwrap();
    let path = Path::new(filepath);
    let filename = path.file_stem().unwrap().to_str().unwrap();

    writeln!(file, "// Generated by Andrew's C~ Compiler\n").unwrap();
    writeln!(file, "\t.file \"{}.c\"", filename).unwrap();
    writeln!(file, "\t.text\n").unwrap();

    for instr in code {
        match &*instr.borrow() {
            Code::Function { name, stack, code } => {
                writeln!(file, "\t.globl {}", name).unwrap();
                writeln!(file, "\t.type {}, @function", name).unwrap();
                writeln!(file, "{}:", name).unwrap();

                writeln!(file, "\tpushq\t%rbp").unwrap();
                writeln!(file, "\tmovq\t%rsp, %rbp").unwrap();
                writeln!(file, "\tsubq\t${}, %rsp", stack).unwrap();

                for instr in code {
                    emit_op(&mut file, instr.clone());
                }

                writeln!(file, "").unwrap();
            }

            _ => {
                emit_op(&mut file, instr.clone());
            }
        }
    }

    writeln!(file, "\t.ident\t\"EdgehammerC 0.1.0\"").unwrap();
    writeln!(file, "\t.section .note.GNU-stack,\"\",@progbits").unwrap();
}
