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

use std::fmt::Display;
use std::io::Write;
use std::path::Path;

use crate::x64::linux::mir::{Mir, MirArena, MirStage, Object, Op, Operand};

struct SizeSuffix(usize);

impl Display for SizeSuffix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            1 => write!(f, "b"),
            2 => write!(f, "s"),
            4 => write!(f, "l"),
            8 => write!(f, "q"),
            _ => unreachable!(),
        }
    }
}

fn operand_size(arena: &MirArena, id: crate::x64::linux::mir::MirId) -> usize {
    match &arena[id] {
        Mir::Operand(op) => match op {
            Operand::Reg { size, .. }
            | Operand::Imm { size, .. }
            | Operand::Mem { size, .. }
            | Operand::Data { size, .. }
            | Operand::Indexed { size, .. } => *size,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn mir_to_str(
    arena: &MirArena,
    mir_id: crate::x64::linux::mir::MirId,
) -> String {
    match &arena[mir_id] {
        Mir::Operand(Operand::Imm { val, .. }) => format!("${}", val),
        Mir::Operand(Operand::Mem { reg, off, .. }) => {
            format!("{}({})", off, mir_to_str(arena, *reg))
        }
        Mir::Operand(Operand::Data { name, .. }) => {
            format!("{}(%rip)", name)
        }
        Mir::Operand(Operand::Indexed {
            base, index, scale, ..
        }) => {
            let base_str = mir_to_str(arena, *base);
            if let Mir::Operand(Operand::Imm { val: 0, .. }) = &arena[*index] {
                format!("0({})", base_str)
            } else {
                let index_str = mir_to_str(arena, *index);
                format!("0({}, {}, {})", base_str, index_str, scale)
            }
        }
        Mir::Op(Op::Label(idx)) => format!(".L{}", idx),
        Mir::Operand(Operand::FunctionRef(name, defined)) => {
            if *defined {
                name.to_string()
            } else {
                format!("{}@PLT", name)
            }
        }
        Mir::Operand(Operand::Reg { reg, size }) => reg.name(*size).to_string(),
        _ => unreachable!(),
    }
}

fn emit_operand(
    file: &mut std::fs::File,
    arena: &MirArena,
    operand: crate::x64::linux::mir::MirId,
) {
    write!(file, "{}", mir_to_str(arena, operand)).unwrap();
}

fn emit_binary_size(
    file: &mut std::fs::File,
    arena: &MirArena,
    mnemonic: &str,
    size: usize,
    src: crate::x64::linux::mir::MirId,
    dst: crate::x64::linux::mir::MirId,
) {
    write!(file, "\t{}{}\t", mnemonic, SizeSuffix(size)).unwrap();
    emit_operand(file, arena, src);
    write!(file, ", ").unwrap();
    emit_operand(file, arena, dst);
    writeln!(file).unwrap();
}

fn emit_binary(
    file: &mut std::fs::File,
    arena: &MirArena,
    mnemonic: &str,
    src: crate::x64::linux::mir::MirId,
    dst: crate::x64::linux::mir::MirId,
) {
    write!(file, "\t{}\t", mnemonic).unwrap();
    emit_operand(file, arena, src);
    write!(file, ", ").unwrap();
    emit_operand(file, arena, dst);
    writeln!(file).unwrap();
}

fn emit_unary_size(
    file: &mut std::fs::File,
    arena: &MirArena,
    mnemonic: &str,
    size: usize,
    op: crate::x64::linux::mir::MirId,
) {
    write!(file, "\t{}{}\t", mnemonic, SizeSuffix(size)).unwrap();
    emit_operand(file, arena, op);
    writeln!(file).unwrap();
}

fn emit_op(
    file: &mut std::fs::File,
    arena: &MirArena,
    instr: crate::x64::linux::mir::MirId,
) {
    match &arena[instr] {
        Mir::Op(Op::Lea(src, dst, size)) => {
            emit_binary_size(file, arena, "lea", *size, *src, *dst);
        }
        Mir::Op(Op::Mov(src, dst, size)) => {
            emit_binary_size(file, arena, "mov", *size, *src, *dst);
        }
        Mir::Op(Op::Movsd(src, dst)) => {
            emit_binary(file, arena, "movsd", *src, *dst);
        }
        Mir::Op(Op::MovSignExt(src, dst, size)) => {
            let src_size = operand_size(arena, *src);
            let src_suffix = match src_size {
                1 => "b",
                2 => "w",
                4 => "l",
                _ => unreachable!(),
            };
            write!(file, "\tmovs{}{}\t", src_suffix, SizeSuffix(*size))
                .unwrap();
            emit_operand(file, arena, *src);
            write!(file, ", ").unwrap();
            emit_operand(file, arena, *dst);
            writeln!(file).unwrap();
        }
        Mir::Op(Op::MovAbs(src, dst, size)) => {
            emit_binary_size(file, arena, "movabs", *size, *src, *dst);
        }
        Mir::Op(Op::Neg(dst, size)) => {
            emit_unary_size(file, arena, "neg", *size, *dst);
        }
        Mir::Op(Op::Not(dst, size)) => {
            emit_unary_size(file, arena, "not", *size, *dst);
        }
        Mir::Op(Op::SignedMul(src, dst, size)) => {
            emit_binary_size(file, arena, "imul", *size, *src, *dst);
        }
        Mir::Op(Op::UnsignedMul(src, size)) => {
            emit_unary_size(file, arena, "mul", *size, *src);
        }
        Mir::Op(Op::MulDouble(src, dst)) => {
            emit_binary(file, arena, "mulsd", *src, *dst);
        }
        Mir::Op(Op::SignedDiv(src, size)) => {
            emit_unary_size(file, arena, "idiv", *size, *src);
        }
        Mir::Op(Op::UnsignedDiv(src, size)) => {
            emit_unary_size(file, arena, "div", *size, *src);
        }
        Mir::Op(Op::DivDouble(src, dst)) => {
            emit_binary(file, arena, "divsd", *src, *dst);
        }
        Mir::Op(Op::Add(src, dst, size)) => {
            emit_binary_size(file, arena, "add", *size, *src, *dst);
        }
        Mir::Op(Op::AddDouble(src, dst)) => {
            emit_binary(file, arena, "addsd", *src, *dst);
        }
        Mir::Op(Op::Sub(src, dst, size)) => {
            emit_binary_size(file, arena, "sub", *size, *src, *dst);
        }
        Mir::Op(Op::SubDouble(src, dst)) => {
            emit_binary(file, arena, "subsd", *src, *dst);
        }
        Mir::Op(Op::LeftShift(src, dst, size)) => {
            emit_binary_size(file, arena, "shl", *size, *src, *dst);
        }
        Mir::Op(Op::ArithRightShift(src, dst, size)) => {
            emit_binary_size(file, arena, "sar", *size, *src, *dst);
        }
        Mir::Op(Op::RightShift(src, dst, size)) => {
            emit_binary_size(file, arena, "shr", *size, *src, *dst);
        }
        Mir::Op(Op::And(src, dst, size)) => {
            emit_binary_size(file, arena, "and", *size, *src, *dst);
        }
        Mir::Op(Op::Or(src, dst, size)) => {
            emit_binary_size(file, arena, "or", *size, *src, *dst);
        }
        Mir::Op(Op::Xor(src, dst, size)) => {
            emit_binary_size(file, arena, "xor", *size, *src, *dst);
        }
        Mir::Op(Op::XorDouble(src, dst)) => {
            emit_binary(file, arena, "xorpd", *src, *dst);
        }
        Mir::Op(Op::DoubleToInt(src, dst, size)) => {
            emit_binary_size(file, arena, "cvttsd2si", *size, *src, *dst);
        }
        Mir::Op(Op::IntToDouble(src, dst, size)) => {
            emit_binary_size(file, arena, "cvtsi2sd", *size, *src, *dst);
        }
        Mir::Op(Op::SignExtend(size)) => {
            if *size == 8 {
                writeln!(file, "\tcqo").unwrap();
            } else {
                writeln!(file, "\tcdq").unwrap();
            }
        }
        Mir::Op(Op::Ret) => {
            writeln!(file, "\tmovq\t%rbp, %rsp").unwrap();
            writeln!(file, "\tpopq\t%rbp").unwrap();
            writeln!(file, "\tret").unwrap();
        }
        Mir::Op(Op::Cmp(src, dst, size)) => {
            emit_binary_size(file, arena, "cmp", *size, *src, *dst);
        }
        Mir::Op(Op::CompareDouble(src, dst)) => {
            emit_binary(file, arena, "ucomisd", *src, *dst);
        }
        Mir::Op(Op::Test(src, dst)) => {
            write!(file, "\ttest\t").unwrap();
            emit_operand(file, arena, *src);
            write!(file, ", ").unwrap();
            emit_operand(file, arena, *dst);
            writeln!(file).unwrap();
        }
        Mir::Op(Op::Jump(label)) => {
            write!(file, "\tjmp\t").unwrap();
            emit_operand(file, arena, *label);
            writeln!(file).unwrap();
        }
        Mir::Op(Op::JumpNotZero(label)) => {
            write!(file, "\tjnz\t").unwrap();
            emit_operand(file, arena, *label);
            writeln!(file).unwrap();
        }
        Mir::Op(Op::JumpCond { cond, label }) => {
            write!(file, "\tj{} \t", cond).unwrap();
            emit_operand(file, arena, *label);
            writeln!(file).unwrap();
        }
        Mir::Op(Op::SetCond { cond, dst }) => {
            write!(file, "\tset{}\t", cond).unwrap();
            emit_operand(file, arena, *dst);
            writeln!(file).unwrap();
        }
        Mir::Op(Op::Label(idx)) => {
            writeln!(file, ".L{}:", idx).unwrap();
        }
        Mir::Op(Op::Call(func)) => {
            write!(file, "\tcall\t").unwrap();
            emit_operand(file, arena, *func);
            writeln!(file).unwrap();
        }
        Mir::Op(Op::Push(val, size)) => {
            emit_unary_size(file, arena, "push", *size, *val);
        }
        Mir::Op(Op::PushBytes(n)) => {
            writeln!(file, "\tsubq\t${}, %rsp", n).unwrap();
        }
        Mir::Op(Op::PopBytes(n)) => {
            writeln!(file, "\taddq\t${}, %rsp", n).unwrap();
        }
        Mir::Object(Object::StaticVar {
            name,
            global,
            inits,
            alignment,
            total_size,
        }) => {
            if *global {
                writeln!(file, "\t.globl {}", name).unwrap();
            }

            let all_zero = inits.iter().all(|init| match &arena[*init] {
                Mir::Object(Object::InitInteger { value, .. }) => *value == 0,
                Mir::Object(Object::InitDouble(v)) => v.to_bits() == 0,
                _ => false,
            });

            if all_zero {
                writeln!(file, "\t.bss").unwrap();
                writeln!(file, "\t.balign {}", alignment).unwrap();
                writeln!(file, "{}:", name).unwrap();
                writeln!(file, "\t.zero {}", total_size).unwrap();
            } else {
                writeln!(file, "\t.data").unwrap();
                writeln!(file, "\t.balign {}", alignment).unwrap();
                writeln!(file, "{}:", name).unwrap();

                let last_nonzero =
                    inits.iter().rposition(|init| match &arena[*init] {
                        Mir::Object(Object::InitInteger { value, .. }) => {
                            *value != 0
                        }
                        Mir::Object(Object::InitDouble(v)) => v.to_bits() != 0,
                        _ => false,
                    });

                let emit_up_to = last_nonzero.map(|i| i + 1).unwrap_or(0);
                let mut emitted_bytes = 0usize;

                for init in &inits[..emit_up_to] {
                    match &arena[*init] {
                        Mir::Object(Object::InitDouble(value)) => {
                            writeln!(
                                file,
                                "\t.quad {}",
                                value.to_bits() as u64
                            )
                            .unwrap();
                            emitted_bytes += 8;
                        }
                        Mir::Object(Object::InitInteger { size, value }) => {
                            match size {
                                1 => {
                                    writeln!(file, "\t.byte {}", *value as u8)
                                        .unwrap();
                                }
                                2 => {
                                    writeln!(
                                        file,
                                        "\t.value {}",
                                        *value as u16
                                    )
                                    .unwrap();
                                }
                                4 => {
                                    writeln!(file, "\t.long {}", *value as u32)
                                        .unwrap();
                                }
                                8 => {
                                    writeln!(file, "\t.quad {}", *value)
                                        .unwrap();
                                }
                                _ => unreachable!(),
                            }
                            emitted_bytes += size;
                        }
                        _ => unreachable!(),
                    }
                }

                let trailing_zeros = total_size - emitted_bytes;
                if trailing_zeros > 0 {
                    writeln!(file, "\t.zero {}", trailing_zeros).unwrap();
                }
            }

            writeln!(file).unwrap();
        }
        Mir::Object(Object::RoData { name, bits }) => {
            writeln!(file, "\t.section .rodata").unwrap();
            writeln!(file, "\t.balign 8").unwrap();
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

pub fn emit(filepath: &str, stage: &MirStage) {
    let mut file = std::fs::File::create(filepath).unwrap();
    let path = Path::new(filepath);
    let filename = path.file_stem().unwrap().to_str().unwrap();

    writeln!(file, "\t.file \"{}.c\"", filename).unwrap();
    writeln!(file, "\t.text\n").unwrap();

    for instr in &stage.mir.top_level {
        match &stage.mir[*instr] {
            Mir::Object(Object::Function {
                name,
                global,
                stack,
                mir,
            }) => {
                writeln!(file, "\t.text").unwrap();
                if *global {
                    writeln!(file, "\t.globl {}", name).unwrap();
                }
                writeln!(file, "\t.type {}, @function", name).unwrap();
                writeln!(file, "{}:", name).unwrap();

                writeln!(file, "\tpushq\t%rbp").unwrap();
                writeln!(file, "\tmovq\t%rsp, %rbp").unwrap();
                writeln!(file, "\tsubq\t${}, %rsp", stack).unwrap();

                for instr in mir {
                    emit_op(&mut file, &stage.mir, *instr);
                }

                writeln!(file).unwrap();
            }
            _ => {
                emit_op(&mut file, &stage.mir, *instr);
            }
        }
    }

    writeln!(file, "\t.ident\t\"EdgehammerC 0.1.0\"").unwrap();
    writeln!(file, "\t.section .note.GNU-stack,\"\",@progbits").unwrap();
}
