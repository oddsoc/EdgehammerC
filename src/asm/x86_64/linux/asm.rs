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

use crate::mir::x86_64::sysv::mir::{
    Mir, MirArena, MirId, MirStage, Object, Op, Operand,
};

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

fn operand_size(arena: &MirArena, id: MirId) -> usize {
    match &arena[id] {
        Mir::Operand(_, size) => *size,
        _ => unreachable!(),
    }
}

fn is_init_zero(arena: &MirArena, init: MirId) -> bool {
    match &arena[init] {
        Mir::Object(Object::InitInteger { value, .. }) => *value == 0,
        Mir::Object(Object::InitDouble(v)) => v.to_bits() == 0,
        _ => false,
    }
}

struct AsmMir<'a> {
    arena: &'a MirArena,
    id: MirId,
}

impl<'a> Display for AsmMir<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match &arena[self.id] {
            Mir::Operand(Operand::Imm(val), _) => write!(f, "${}", val),
            Mir::Operand(Operand::Mem(reg, off), _) => {
                write!(f, "{}({})", off, AsmMir { arena, id: *reg })
            }
            Mir::Operand(Operand::Sym(name), _) => {
                write!(f, "{}(%rip)", name)
            }
            Mir::Operand(
                Operand::Idx {
                    base, index, scale, ..
                },
                _,
            ) => {
                let base_str = AsmMir { arena, id: *base }.to_string();
                if let Mir::Operand(Operand::Imm(0), _) = &arena[*index] {
                    write!(f, "0({})", base_str)
                } else {
                    let index_str = AsmMir { arena, id: *index }.to_string();
                    write!(f, "0({}, {}, {})", base_str, index_str, scale)
                }
            }
            Mir::Operand(Operand::Reg(reg), size) => {
                write!(f, "{}", reg.name(*size))
            }
            Mir::Op(Op::Label(idx)) => write!(f, ".L{}", idx),
            Mir::Op(op) => Self::fmt_op(f, arena, op),
            Mir::Object(Object::Data {
                name,
                global,
                read_only,
                inits,
                alignment,
                total_size,
            }) => {
                if *global {
                    writeln!(f, "\t.globl {}", name)?;
                }

                if *read_only {
                    writeln!(f, "\t.section .rodata")?;
                    writeln!(f, "\t.balign {}", alignment)?;
                    writeln!(f, "{}:", name)?;

                    let mut emitted_bytes = 0usize;
                    for init in inits {
                        emitted_bytes += fmt_init_value(f, arena, *init);
                    }

                    let trailing_zeros = total_size - emitted_bytes;
                    if trailing_zeros > 0 {
                        writeln!(f, "\t.zero {}", trailing_zeros)?;
                    }
                } else if inits.iter().all(|init| is_init_zero(arena, *init)) {
                    writeln!(f, "\t.bss")?;
                    writeln!(f, "\t.balign {}", alignment)?;
                    writeln!(f, "{}:", name)?;
                    writeln!(f, "\t.zero {}", total_size)?;
                } else {
                    writeln!(f, "\t.data")?;
                    writeln!(f, "\t.balign {}", alignment)?;
                    writeln!(f, "{}:", name)?;

                    let last_nonzero = inits
                        .iter()
                        .rposition(|init| !is_init_zero(arena, *init));
                    let emit_up_to = last_nonzero.map(|i| i + 1).unwrap_or(0);
                    let mut emitted_bytes = 0usize;

                    for init in &inits[..emit_up_to] {
                        emitted_bytes += fmt_init_value(f, arena, *init);
                    }

                    let trailing_zeros = total_size - emitted_bytes;
                    if trailing_zeros > 0 {
                        writeln!(f, "\t.zero {}", trailing_zeros)?;
                    }
                }

                Ok(())
            }
            _ => unreachable!(),
        }
    }
}

impl<'a> AsmMir<'a> {
    fn fmt_op(
        f: &mut std::fmt::Formatter<'_>,
        arena: &'a MirArena,
        op: &Op,
    ) -> std::fmt::Result {
        match op {
            // Data movement
            Op::Lea(src, dst, size) => {
                fmt_binary_size(f, arena, "lea", *size, *src, *dst)
            }
            Op::Mov(src, dst, size) => {
                fmt_binary_size(f, arena, "mov", *size, *src, *dst)
            }
            Op::Movsd(src, dst) => fmt_binary(f, arena, "movsd", *src, *dst),
            Op::Movsx(src, dst, size) => fmt_movsx(f, arena, *src, *dst, *size),
            Op::MovAbs(src, dst, size) => {
                fmt_binary_size(f, arena, "movabs", *size, *src, *dst)
            }

            // Unary integer ops
            Op::Neg(dst, size) => fmt_unary_size(f, arena, "neg", *size, *dst),
            Op::Not(dst, size) => fmt_unary_size(f, arena, "not", *size, *dst),

            // Multiplication / division
            Op::Imul(src, dst, size) => {
                fmt_binary_size(f, arena, "imul", *size, *src, *dst)
            }
            Op::Mul(src, size) => fmt_unary_size(f, arena, "mul", *size, *src),
            Op::Mulsd(src, dst) => fmt_binary(f, arena, "mulsd", *src, *dst),
            Op::Idiv(src, size) => {
                fmt_unary_size(f, arena, "idiv", *size, *src)
            }
            Op::Div(src, size) => fmt_unary_size(f, arena, "div", *size, *src),
            Op::Divsd(src, dst) => fmt_binary(f, arena, "divsd", *src, *dst),

            // Arithmetic
            Op::Add(src, dst, size) => {
                fmt_binary_size(f, arena, "add", *size, *src, *dst)
            }
            Op::Addsd(src, dst) => fmt_binary(f, arena, "addsd", *src, *dst),
            Op::Sub(src, dst, size) => {
                fmt_binary_size(f, arena, "sub", *size, *src, *dst)
            }
            Op::Subsd(src, dst) => fmt_binary(f, arena, "subsd", *src, *dst),

            // Bitwise / shifts
            Op::Shl(src, dst, size) => {
                fmt_binary_size(f, arena, "shl", *size, *src, *dst)
            }
            Op::Sar(src, dst, size) => {
                fmt_binary_size(f, arena, "sar", *size, *src, *dst)
            }
            Op::Shr(src, dst, size) => {
                fmt_binary_size(f, arena, "shr", *size, *src, *dst)
            }
            Op::And(src, dst, size) => {
                fmt_binary_size(f, arena, "and", *size, *src, *dst)
            }
            Op::Or(src, dst, size) => {
                fmt_binary_size(f, arena, "or", *size, *src, *dst)
            }
            Op::Xor(src, dst, size) => {
                fmt_binary_size(f, arena, "xor", *size, *src, *dst)
            }
            Op::Xorpd(src, dst) => fmt_binary(f, arena, "xorpd", *src, *dst),

            // Conversion
            Op::Cvttsd2si(src, dst, size) => {
                fmt_binary_size(f, arena, "cvttsd2si", *size, *src, *dst)
            }
            Op::Cvtsi2sd(src, dst, size) => {
                fmt_binary_size(f, arena, "cvtsi2sd", *size, *src, *dst)
            }
            Op::Cqo(size) => {
                if *size == 8 {
                    write!(f, "\tcqo")
                } else {
                    write!(f, "\tcdq")
                }
            }

            // Comparison
            Op::Cmp(src, dst, size) => {
                fmt_binary_size(f, arena, "cmp", *size, *src, *dst)
            }
            Op::Ucomisd(src, dst) => {
                fmt_binary(f, arena, "ucomisd", *src, *dst)
            }
            Op::Test(src, dst) => write!(
                f,
                "\ttest\t{}, {}",
                AsmMir { arena, id: *src },
                AsmMir { arena, id: *dst }
            ),

            // Control flow
            Op::Jmp(label) => {
                write!(f, "\tjmp\t{}", AsmMir { arena, id: *label })
            }
            Op::Jnz(label) => {
                write!(f, "\tjnz\t{}", AsmMir { arena, id: *label })
            }
            Op::Jcc { cond, label } => {
                write!(f, "\tj{} \t{}", cond, AsmMir { arena, id: *label })
            }
            Op::Setcc { cond, dst } => {
                write!(f, "\tset{}\t{}", cond, AsmMir { arena, id: *dst })
            }
            Op::Label(idx) => write!(f, ".L{}:", idx),
            Op::Call(func) => {
                write!(f, "\tcall\t")?;
                match &arena[*func] {
                    Mir::Operand(Operand::Sym(name), _) => {
                        write!(f, "{}", name)
                    }
                    _ => write!(f, "{}", AsmMir { arena, id: *func }),
                }
            }

            // Stack
            Op::Push(val, size) => {
                fmt_unary_size(f, arena, "push", *size, *val)
            }
            Op::PushBytes(n) => write!(f, "\tsubq\t${}, %rsp", n),
            Op::PopBytes(n) => write!(f, "\taddq\t${}, %rsp", n),

            // Return
            Op::Ret => {
                writeln!(f, "\tmovq\t%rbp, %rsp")?;
                writeln!(f, "\tpopq\t%rbp")?;
                write!(f, "\tret")
            }
        }
    }
}

fn fmt_binary_size(
    f: &mut std::fmt::Formatter<'_>,
    arena: &MirArena,
    mnemonic: &str,
    size: usize,
    src: MirId,
    dst: MirId,
) -> std::fmt::Result {
    write!(
        f,
        "\t{}{}\t{}, {}",
        mnemonic,
        SizeSuffix(size),
        AsmMir { arena, id: src },
        AsmMir { arena, id: dst }
    )
}

fn fmt_binary(
    f: &mut std::fmt::Formatter<'_>,
    arena: &MirArena,
    mnemonic: &str,
    src: MirId,
    dst: MirId,
) -> std::fmt::Result {
    write!(
        f,
        "\t{}\t{}, {}",
        mnemonic,
        AsmMir { arena, id: src },
        AsmMir { arena, id: dst }
    )
}

fn fmt_unary_size(
    f: &mut std::fmt::Formatter<'_>,
    arena: &MirArena,
    mnemonic: &str,
    size: usize,
    op: MirId,
) -> std::fmt::Result {
    write!(
        f,
        "\t{}{}\t{}",
        mnemonic,
        SizeSuffix(size),
        AsmMir { arena, id: op }
    )
}

fn fmt_movsx(
    f: &mut std::fmt::Formatter<'_>,
    arena: &MirArena,
    src: MirId,
    dst: MirId,
    dst_size: usize,
) -> std::fmt::Result {
    let src_size = operand_size(arena, src);
    let src_suffix = match src_size {
        1 => "b",
        2 => "w",
        4 => "l",
        _ => unreachable!(),
    };
    write!(
        f,
        "\tmovs{}{}\t{}, {}",
        src_suffix,
        SizeSuffix(dst_size),
        AsmMir { arena, id: src },
        AsmMir { arena, id: dst }
    )
}

fn fmt_init_value(
    f: &mut std::fmt::Formatter<'_>,
    arena: &MirArena,
    init: MirId,
) -> usize {
    match &arena[init] {
        Mir::Object(Object::InitDouble(value)) => {
            writeln!(f, "\t.quad 0x{:016x}", value.to_bits()).unwrap();
            8
        }
        Mir::Object(Object::InitInteger { size, value }) => {
            match size {
                1 => {
                    writeln!(f, "\t.byte {}", *value as u8).unwrap();
                }
                2 => {
                    writeln!(f, "\t.value {}", *value as u16).unwrap();
                }
                4 => {
                    writeln!(f, "\t.long {}", *value as u32).unwrap();
                }
                8 => {
                    writeln!(f, "\t.quad {}", *value).unwrap();
                }
                _ => unreachable!(),
            }
            *size
        }
        _ => unreachable!(),
    }
}

fn emit_op(file: &mut std::fs::File, arena: &MirArena, instr: MirId) {
    match &arena[instr] {
        Mir::Op(Op::Label(idx)) => {
            writeln!(file, ".L{}:", idx).unwrap();
        }
        Mir::Op(_) | Mir::Operand(..) => {
            writeln!(file, "{}", AsmMir { arena, id: instr }).unwrap();
        }
        Mir::Object(Object::Data { .. }) => {
            writeln!(file, "{}", AsmMir { arena, id: instr }).unwrap();
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

    writeln!(file, "\t.ident\t\"asjcc 0.1.0\"").unwrap();
    writeln!(file, "\t.section .note.GNU-stack,\"\",@progbits").unwrap();
}
