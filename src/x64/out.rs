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

use crate::x64::codegen::Asm;

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

fn asm_to_str(asm: Rc<RefCell<Asm>>) -> String {
    match &*asm.borrow() {
        Asm::Imm(val) => format!("${}", val),
        Asm::Stack(idx, _size) => format!("-{}(%rbp)", idx),
        Asm::Label(idx) => format!(".L{}", idx),
        Asm::Al => format!("%al"),
        Asm::Ax => format!("%ax"),
        Asm::Eax => format!("%eax"),
        Asm::Rax => format!("%Rax"),
        Asm::Bl => format!("%bl"),
        Asm::Bx => format!("%bx"),
        Asm::Ebx => format!("%ebx"),
        Asm::Rbx => format!("%rbx"),
        Asm::Cl => format!("%cl"),
        Asm::Cx => format!("%cx"),
        Asm::Ecx => format!("%ecx"),
        Asm::Rcx => format!("%rcx"),
        Asm::Dl => format!("%dl"),
        Asm::Dx => format!("%dx"),
        Asm::Edx => format!("%edx"),
        Asm::Rdx => format!("%rdx"),
        Asm::Sil => format!("%sil"),
        Asm::Si => format!("%si"),
        Asm::Esi => format!("%esi"),
        Asm::Rsi => format!("%rsi"),
        Asm::Dil => format!("%dil"),
        Asm::Di => format!("%di"),
        Asm::Edi => format!("%edi"),
        Asm::Rdi => format!("%rdi"),
        Asm::Spl => format!("%spl"),
        Asm::Sp => format!("%sp"),
        Asm::Esp => format!("%esp"),
        Asm::Rsp => format!("%rsp"),
        Asm::Bpl => format!("%bpl"),
        Asm::Bp => format!("%bp"),
        Asm::Ebp => format!("%ebp"),
        Asm::Rbp => format!("%rbp"),
        Asm::R8b => format!("%r8b"),
        Asm::R8w => format!("%r8w"),
        Asm::R8d => format!("%r8d"),
        Asm::R8 => format!("%r8"),
        Asm::R9b => format!("%r9b"),
        Asm::R9w => format!("%r9w"),
        Asm::R9d => format!("%r9d"),
        Asm::R9 => format!("%r9"),
        Asm::R10b => format!("%r10b"),
        Asm::R10w => format!("%r10w"),
        Asm::R10d => format!("%r10d"),
        Asm::R10 => format!("%r10"),
        Asm::R11b => format!("%r11b"),
        Asm::R11w => format!("%r11w"),
        Asm::R11d => format!("%r11d"),
        Asm::R11 => format!("%r11"),
        Asm::R12b => format!("%r12b"),
        Asm::R12w => format!("%r12w"),
        Asm::R12d => format!("%r12d"),
        Asm::R12 => format!("%r12"),
        Asm::R13b => format!("%r13b"),
        Asm::R13w => format!("%r13w"),
        Asm::R13d => format!("%r13d"),
        Asm::R13 => format!("%r13"),
        Asm::R14b => format!("%r14b"),
        Asm::R14w => format!("%r14w"),
        Asm::R14d => format!("%r14d"),
        Asm::R14 => format!("%r14"),
        Asm::R15b => format!("%r15b"),
        Asm::R15w => format!("%r15w"),
        Asm::R15d => format!("%r15d"),
        Asm::R15 => format!("%r15"),

        _ => unreachable!(),
    }
}

fn emit_operand(file: &mut std::fs::File, operand: Rc<RefCell<Asm>>) {
    write!(file, "{}", asm_to_str(operand)).unwrap();
}

fn emit_op(file: &mut std::fs::File, instr: Rc<RefCell<Asm>>) {
    match &*instr.borrow() {
        Asm::Movb(src, dst) => {
            write!(file, "\tmovb\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Movw(src, dst) => {
            write!(file, "\tmovw\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Movl(src, dst) => {
            write!(file, "\tmovl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Movq(src, dst) => {
            write!(file, "\tmovq\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Negl(dst) => {
            write!(file, "\tnegl\t").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Notl(dst) => {
            write!(file, "\tnotl\t").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::IMull(src, dst) => {
            write!(file, "\timull\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::IDivl(src) => {
            write!(file, "\tidivl\t").unwrap();
            emit_operand(file, src.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Addl(src, dst) => {
            write!(file, "\taddl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Subl(src, dst) => {
            write!(file, "\tsubl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Shll(src, dst) => {
            write!(file, "\tshll\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Sarl(src, dst) => {
            write!(file, "\tsarl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Andl(src, dst) => {
            write!(file, "\tandl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Orl(src, dst) => {
            write!(file, "\torl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Xorl(src, dst) => {
            write!(file, "\txorl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Cdq => {
            writeln!(file, "\tcdq").unwrap();
        }

        Asm::Ret => {
            writeln!(file, "\tmovq\t%rbp, %rsp").unwrap();
            writeln!(file, "\tpopq\t%rbp").unwrap();
            writeln!(file, "\tret").unwrap();
        }

        Asm::Cmpl(src, dst) => {
            write!(file, "\tcmpl\t").unwrap();
            emit_operand(file, src.clone());
            write!(file, ", ").unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Jmp(label) => {
            write!(file, "\tjmp\t").unwrap();
            emit_operand(file, label.clone());
            writeln!(file, "").unwrap();
        }

        Asm::JmpCC { cond, label } => {
            let cc = cc_to_str(cond);
            write!(file, "\tj{} \t", cc).unwrap();
            emit_operand(file, label.clone());
            writeln!(file, "").unwrap();
        }

        Asm::SetCC { cond, dst } => {
            let cc = cc_to_str(cond);
            write!(file, "\tset{}\t", cc).unwrap();
            emit_operand(file, dst.clone());
            writeln!(file, "").unwrap();
        }

        Asm::Label(idx) => {
            writeln!(file, ".L{}:", *idx).unwrap();
        }

        _ => {
            unreachable!();
        }
    }
}

pub fn emit(filepath: &str, asm: Vec<Rc<RefCell<Asm>>>) {
    let mut file = std::fs::File::create(filepath).unwrap();
    let path = Path::new(filepath);
    let filename = path.file_stem().unwrap().to_str().unwrap();

    writeln!(file, "// Generated by Andrew's C~ Compiler\n").unwrap();
    writeln!(file, "\t.file \"{}.c\"", filename).unwrap();
    writeln!(file, "\t.text").unwrap();

    for instr in asm {
        match &*instr.borrow() {
            Asm::Function { name, stack, asm } => {
                writeln!(file, "\t.globl {}", name).unwrap();
                writeln!(file, "\t.type {}, @function", name).unwrap();
                writeln!(file, "{}:", name).unwrap();

                writeln!(file, "\tpushq\t%rbp").unwrap();
                writeln!(file, "\tmovq\t%rsp, %rbp").unwrap();
                writeln!(file, "\tsubq\t${}, %rsp", stack).unwrap();

                for instr in asm {
                    emit_op(&mut file, instr.clone());
                }
            }

            _ => {
                emit_op(&mut file, instr.clone());
            }
        }
    }

    writeln!(file, "\n\t.ident\t\"EdgehammerC 0.1.0\"").unwrap();
    writeln!(file, "\t.section .note.GNU-stack,\"\",@progbits").unwrap();
}
