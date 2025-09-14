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

use std::cell::RefCell;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

use crate::x64::linux::codegen::{Code, Register};

use super::codegen::CondCode;

fn cc_to_str(cc: &CondCode) -> String {
    match cc {
        CondCode::Eq => "e",
        CondCode::NotEq => "ne",
        CondCode::Less => "l",
        CondCode::LessOrEq => "le",
        CondCode::Greater => "g",
        CondCode::GreaterOrEq => "ge",
        CondCode::Above => "a",
        CondCode::AboveOrEq => "ae",
        CondCode::Below => "b",
        CondCode::BelowOrEq => "be",
        CondCode::Parity => "p",
        CondCode::NoParity => "np",
    }
    .to_string()
}

fn code_to_str(code: Rc<RefCell<Code>>) -> String {
    match &*code.borrow() {
        Code::Imm { val, .. } => format!("${}", val),
        Code::Var { off, .. } => {
            format!("{}(%rbp)", -off)
        }

        Code::Data { name, .. } => {
            format!("{}(%rip)", name)
        }

        Code::Label(idx) => format!(".L{}", idx),
        Code::FunctionRef(name, defined) => {
            if *defined {
                name.to_string()
            } else {
                format!("{}@PLT", name)
            }
        }

        Code::Reg { reg, size, .. } => match reg {
            Register::Rax => match size {
                1 => "%al".to_string(),
                2 => "%ax".to_string(),
                4 => "%eax".to_string(),
                8 => "%rax".to_string(),
                _ => unreachable!(),
            },
            Register::Rbx => match size {
                1 => "%bl".to_string(),
                2 => "%bx".to_string(),
                4 => "%ebx".to_string(),
                8 => "%rbx".to_string(),
                _ => unreachable!(),
            },
            Register::Rcx => match size {
                1 => "%cl".to_string(),
                2 => "%cx".to_string(),
                4 => "%ecx".to_string(),
                8 => "%rcx".to_string(),
                _ => unreachable!(),
            },
            Register::Rdx => match size {
                1 => "%dl".to_string(),
                2 => "%dx".to_string(),
                4 => "%edx".to_string(),
                8 => "%rdx".to_string(),
                _ => unreachable!(),
            },
            Register::Rsi => match size {
                1 => "%sil".to_string(),
                2 => "%si".to_string(),
                4 => "%esi".to_string(),
                8 => "%rsi".to_string(),
                _ => unreachable!(),
            },
            Register::Rdi => match size {
                1 => "%dil".to_string(),
                2 => "%di".to_string(),
                4 => "%edi".to_string(),
                8 => "%rdi".to_string(),
                _ => unreachable!(),
            },
            Register::Rsp => match size {
                1 => "%spl".to_string(),
                2 => "%sp".to_string(),
                4 => "%esp".to_string(),
                8 => "%rsp".to_string(),
                _ => unreachable!(),
            },
            Register::Rbp => match size {
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
            Register::Xmm0 => "%xmm0".to_string(),
            Register::Xmm1 => "%xmm1".to_string(),
            Register::Xmm2 => "%xmm2".to_string(),
            Register::Xmm3 => "%xmm3".to_string(),
            Register::Xmm4 => "%xmm4".to_string(),
            Register::Xmm5 => "%xmm5".to_string(),
            Register::Xmm6 => "%xmm6".to_string(),
            Register::Xmm7 => "%xmm7".to_string(),
            Register::Xmm8 => "%xmm8".to_string(),
            Register::Xmm9 => "%xmm9".to_string(),
            Register::Xmm10 => "%xmm10".to_string(),
            Register::Xmm11 => "%xmm11".to_string(),
            Register::Xmm12 => "%xmm12".to_string(),
            Register::Xmm13 => "%xmm13".to_string(),
            Register::Xmm14 => "%xmm14".to_string(),
            Register::Xmm15 => "%xmm15".to_string(),
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
            writeln!(file).unwrap();
        }

        Code::Movsd(src, dst) => {
            write!(file, "\tmovsd\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::MovSignExt(src, dst, size) => {
            write!(file, "\tmovsl{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::MovAbs(src, dst, size) => {
            write!(file, "\tmovabs{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Neg(dst, size) => {
            write!(file, "\tneg{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Not(dst, size) => {
            write!(file, "\tnot{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::IMul(src, dst, size) => {
            write!(file, "\timul{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Mul(src, size) => {
            write!(file, "\tmul{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            writeln!(file).unwrap();
        }

        Code::Mulsd(src, dst) => {
            write!(file, "\tmulsd\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::IDiv(src, size) => {
            write!(file, "\tidiv{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            writeln!(file).unwrap();
        }

        Code::Div(src, size) => {
            write!(file, "\tdiv{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            writeln!(file).unwrap();
        }

        Code::Divsd(src, dst) => {
            write!(file, "\tdivsd\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Add(src, dst, size) => {
            write!(file, "\tadd{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Addsd(src, dst) => {
            write!(file, "\taddsd\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Sub(src, dst, size) => {
            write!(file, "\tsub{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Subsd(src, dst) => {
            write!(file, "\tsubsd\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Shl(src, dst, size) => {
            write!(file, "\tshl{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Sar(src, dst, size) => {
            write!(file, "\tsar{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Shr(src, dst, size) => {
            write!(file, "\tshr{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::And(src, dst, size) => {
            write!(file, "\tand{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Or(src, dst, size) => {
            write!(file, "\tor{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Xor(src, dst, size) => {
            write!(file, "\txor{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Xorpd(src, dst) => {
            write!(file, "\txorpd\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Cvttsd2si(src, dst, size) => {
            write!(file, "\tcvttsd2si{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }
        Code::Cvtsi2sd(src, dst, size) => {
            write!(file, "\tcvtsi2sd{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Cdq(size) => {
            if *size == 8 {
                writeln!(file, "\tcqo").unwrap();
            } else {
                writeln!(file, "\tcdq").unwrap();
            }
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
            writeln!(file).unwrap();
        }

        Code::Comisd(src, dst) => {
            write!(file, "\tucomisd\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Test(src, dst) => {
            write!(file, "\ttest\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Jmp(label) => {
            write!(file, "\tjmp\t").unwrap();
            emit_operand(file, label.clone());
            writeln!(file).unwrap();
        }

        Code::JmpNotZero(label) => {
            write!(file, "\tjnz\t").unwrap();
            emit_operand(file, label.clone());
            writeln!(file).unwrap();
        }

        Code::JmpCC { cond, label } => {
            let cc = cc_to_str(cond);
            write!(file, "\tj{} \t", cc).unwrap();
            emit_operand(file, label.clone());
            writeln!(file).unwrap();
        }

        Code::SetCC { cond, dst } => {
            let cc = cc_to_str(cond);
            write!(file, "\tset{}\t", cc).unwrap();
            emit_operand(file, dst.clone());
            writeln!(file).unwrap();
        }

        Code::Label(idx) => {
            writeln!(file, ".L{}:", *idx).unwrap();
        }

        Code::Call(func) => {
            write!(file, "\tcall\t").unwrap();
            emit_operand(file, func.clone());
            writeln!(file).unwrap();
        }

        Code::Push(val, size) => {
            write!(file, "\tpush{}\t", op_suffix(*size)).unwrap();
            emit_operand(file, val.clone());
            writeln!(file).unwrap();
        }

        Code::PushBytes(n) => {
            writeln!(file, "\tsubq\t${}, %rsp", n).unwrap();
        }

        Code::PopBytes(n) => {
            writeln!(file, "\taddq\t${}, %rsp", n).unwrap();
        }

        Code::StaticVar { name, global, init } => {
            if *global {
                writeln!(file, "\t.globl {}", name).unwrap();
            }

            match &*init.borrow() {
                Code::InitDouble(value) => {
                    writeln!(file, "\t.data").unwrap();
                    writeln!(file, "\t.align 16").unwrap();
                    writeln!(file, "{}:", name).unwrap();
                    writeln!(file, "\t.quad {}", value.to_bits() as u64)
                        .unwrap();
                }
                Code::InitInteger {
                    signed,
                    size,
                    value,
                } => {
                    if *value == 0 {
                        writeln!(file, "\t.bss").unwrap();
                        writeln!(file, "\t.align {}", *size).unwrap();
                        writeln!(file, "{}:", name).unwrap();
                        writeln!(file, "\t.zero {}", *size).unwrap();
                    } else {
                        writeln!(file, "\t.data").unwrap();
                        writeln!(file, "\t.align {}", *size).unwrap();
                        writeln!(file, "{}:", name).unwrap();
                        if *size == 8 {
                            if *signed {
                                writeln!(file, "\t.quad {}", *value as i64)
                                    .unwrap();
                            } else {
                                writeln!(file, "\t.quad {}", *value).unwrap();
                            }
                        } else {
                            if *signed {
                                writeln!(file, "\t.long {}", *value as i32)
                                    .unwrap();
                            } else {
                                writeln!(file, "\t.long {}", *value as u32)
                                    .unwrap();
                            }
                        }
                    }
                }
                _ => unreachable!(),
            }

            writeln!(file).unwrap();
        }
        Code::RoData { name, bits } => {
            writeln!(file, "\t.section .rodata").unwrap();
            writeln!(file, "\t.align 8").unwrap();
            writeln!(file, "{}:", name).unwrap();
            writeln!(file, "\t.quad 0x{:016x}", bits).unwrap();
            writeln!(file).unwrap();
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

    writeln!(file, "\t.file \"{}.c\"", filename).unwrap();
    writeln!(file, "\t.text\n").unwrap();

    for instr in code {
        match &*instr.borrow() {
            Code::Function {
                name,
                global,
                stack,
                code,
            } => {
                writeln!(file, "\t.text").unwrap();
                if *global {
                    writeln!(file, "\t.globl {}", name).unwrap();
                }
                writeln!(file, "\t.type {}, @function", name).unwrap();
                writeln!(file, "{}:", name).unwrap();

                writeln!(file, "\tpushq\t%rbp").unwrap();
                writeln!(file, "\tmovq\t%rsp, %rbp").unwrap();
                writeln!(file, "\tsubq\t${}, %rsp", stack).unwrap();

                for instr in code {
                    emit_op(&mut file, instr.clone());
                }

                writeln!(file).unwrap();
            }

            _ => {
                emit_op(&mut file, instr.clone());
            }
        }
    }

    writeln!(file, "\t.ident\t\"EdgehammerC 0.1.0\"").unwrap();
    writeln!(file, "\t.section .note.GNU-stack,\"\",@progbits").unwrap();
}
