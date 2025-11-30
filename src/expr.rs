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

use crate::ast::*;
use crate::errors::{
    Error,
    ErrorClass::{Parsing, Semantic},
    ParsingError, SemanticError, error,
};
use crate::symtab::*;
use crate::types::*;

fn as_const_int(arena: &mut AstArena, n: i32, scope: ScopeId) -> AstId {
    arena.alloc(AstKind::ConstInt(n), Some(int_type(true)), scope)
}

fn as_const_unsigned_int(
    arena: &mut AstArena,
    n: u32,
    scope: ScopeId,
) -> AstId {
    arena.alloc(AstKind::ConstUnsignedInt(n), Some(int_type(false)), scope)
}

fn as_const_long(arena: &mut AstArena, n: i64, scope: ScopeId) -> AstId {
    arena.alloc(AstKind::ConstLong(n), Some(long_type(true)), scope)
}

fn as_const_unsigned_long(
    arena: &mut AstArena,
    n: u64,
    scope: ScopeId,
) -> AstId {
    arena.alloc(AstKind::ConstUnsignedLong(n), Some(long_type(false)), scope)
}

fn fold_child(arena: &mut AstArena, id: AstId) {
    let folded = fold(arena, id);
    arena[id] = arena[folded].clone();
}

pub fn fold(arena: &mut AstArena, id: AstId) -> AstId {
    let scope = arena[id].scope;
    let kind = arena[id].kind.clone();

    match &kind {
        AstKind::Identifier { .. } => id,
        AstKind::Assign { .. } => id,
        AstKind::CompoundAssign { .. } => id,
        AstKind::LogicAnd { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::LogicOr { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Add { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_add(*rhs);
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_add(*rhs);
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_add(*rhs as u32) as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_add(*rhs as u64) as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Subtract { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_sub(*rhs);
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_sub(*rhs);
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_sub(*rhs as u32) as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_sub(*rhs as u64) as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Multiply { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_mul(*rhs);
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_mul(*rhs);
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_mul(*rhs as u32) as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_mul(*rhs as u64) as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Divide { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs / *rhs };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs / *rhs };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u32) / (*rhs as u32) as u32
                    } as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u64) / (*rhs as u64) as u64
                    } as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Modulo { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs % *rhs };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs % *rhs };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u32) % (*rhs as u32) as u32
                    } as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u64) % (*rhs as u64) as u64
                    } as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::And { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = *lhs & *rhs;
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = *lhs & *rhs;
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) & (*rhs as u32) as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) & (*rhs as u64) as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Or { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = *lhs | *rhs;
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = *lhs | *rhs;
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) | (*rhs as u32) as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) | (*rhs as u64) as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::LeftShift { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_shl(*rhs as u32);
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_shl(*rhs as u32);
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_shl(*rhs as u32) as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_shl(*rhs as u32) as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::RightShift { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *rhs >= 0 { *lhs >> *rhs } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *rhs >= 0 { *lhs >> *rhs } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) >> (*rhs as u32);
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) >> (*rhs as u32);
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Xor { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = *lhs ^ *rhs;
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = *lhs ^ *rhs;
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) ^ (*rhs as u32) as u32;
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) ^ (*rhs as u64) as u64;
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Equal { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::NotEq { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Less { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs < *rhs { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs < *rhs { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if (*lhs as u32) < (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if (*lhs as u64) < (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::LessOrEq { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs <= *rhs { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs <= *rhs { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res =
                        if (*lhs as u32) <= (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res =
                        if (*lhs as u64) <= (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::Greater { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs > *rhs { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs > *rhs { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if (*lhs as u32) > (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if (*lhs as u64) > (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }
        AstKind::GreaterOrEq { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);

            match (&arena[*left].kind, &arena[*right].kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs >= *rhs { 1 } else { 0 };
                    as_const_int(arena, res, scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs >= *rhs { 1 } else { 0 };
                    as_const_long(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res =
                        if (*lhs as u32) >= (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(arena, res, scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res =
                        if (*lhs as u64) >= (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(arena, res, scope)
                }
                _ => id,
            }
        }

        AstKind::Ternary {
            left,
            middle,
            right,
        } => {
            fold_child(arena, *left);
            fold_child(arena, *middle);
            fold_child(arena, *right);

            match &arena[*left].kind {
                AstKind::ConstInt(n) => {
                    if *n != 0 {
                        *middle
                    } else {
                        *right
                    }
                }
                AstKind::ConstLong(n) => {
                    if *n != 0 {
                        *middle
                    } else {
                        *right
                    }
                }
                _ => id,
            }
        }

        AstKind::Not { expr: inner } => {
            fold_child(arena, *inner);

            match &arena[*inner].kind {
                AstKind::ConstInt(n) => {
                    if *n != 0 {
                        as_const_int(arena, 0, scope)
                    } else {
                        as_const_int(arena, 1, scope)
                    }
                }
                AstKind::ConstLong(n) => {
                    if *n != 0 {
                        as_const_int(arena, 0, scope)
                    } else {
                        as_const_int(arena, 1, scope)
                    }
                }
                AstKind::ConstUnsignedInt(n) => {
                    if *n != 0 {
                        as_const_unsigned_int(arena, 0, scope)
                    } else {
                        as_const_unsigned_int(arena, 1, scope)
                    }
                }
                AstKind::ConstUnsignedLong(n) => {
                    if *n != 0 {
                        as_const_unsigned_long(arena, 0, scope)
                    } else {
                        as_const_unsigned_long(arena, 1, scope)
                    }
                }
                _ => id,
            }
        }
        AstKind::Negate { expr: inner } => {
            fold_child(arena, *inner);
            match &arena[*inner].kind {
                AstKind::ConstInt(n) => as_const_int(arena, -(*n), scope),
                AstKind::ConstLong(n) => as_const_long(arena, -(*n), scope),
                AstKind::ConstUnsignedInt(n) => {
                    as_const_int(arena, -(*n as i32), scope)
                }
                AstKind::ConstUnsignedLong(n) => {
                    as_const_long(arena, -(*n as i64), scope)
                }
                _ => id,
            }
        }
        AstKind::Complement { expr: inner } => {
            fold_child(arena, *inner);

            match &arena[*inner].kind {
                AstKind::ConstInt(n) => as_const_int(arena, !(*n), scope),
                AstKind::ConstLong(n) => as_const_long(arena, !(*n), scope),
                AstKind::ConstUnsignedInt(n) => {
                    as_const_int(arena, !(*n as i32), scope)
                }
                AstKind::ConstUnsignedLong(n) => {
                    as_const_long(arena, !(*n as i64), scope)
                }
                _ => id,
            }
        }
        AstKind::AddrOf { expr: inner } => {
            fold_child(arena, *inner);
            id
        }
        AstKind::Deref { expr: inner } => {
            fold_child(arena, *inner);
            id
        }
        AstKind::Subscript { left, right } => {
            fold_child(arena, *left);
            fold_child(arena, *right);
            id
        }
        AstKind::Cast { expr: inner, .. } => {
            fold_child(arena, *inner);
            let ty = arena[id].ty.clone();
            let signed = is_signed(&ty);

            match &arena[*inner].kind {
                AstKind::ConstInt(n) => {
                    if ty.borrow().kind == TypeKind::Int {
                        as_const_int(arena, *n, scope)
                    } else if ty.borrow().kind == TypeKind::Long {
                        as_const_long(arena, *n as i64, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(arena, *n as u32, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(arena, *n as u64, scope)
                    } else {
                        id
                    }
                }
                AstKind::ConstLong(n) => {
                    if signed && ty.borrow().kind == TypeKind::Int {
                        as_const_int(arena, *n as i32, scope)
                    } else if signed && ty.borrow().kind == TypeKind::Long {
                        as_const_long(arena, *n, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(arena, *n as u32, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(arena, *n as u64, scope)
                    } else {
                        id
                    }
                }
                AstKind::ConstUnsignedInt(n) => {
                    if signed && ty.borrow().kind == TypeKind::Int {
                        as_const_int(arena, *n as i32, scope)
                    } else if signed && ty.borrow().kind == TypeKind::Long {
                        as_const_long(arena, *n as i64, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(arena, *n as u32, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(arena, *n as u64, scope)
                    } else {
                        id
                    }
                }
                AstKind::ConstUnsignedLong(n) => {
                    if signed && ty.borrow().kind == TypeKind::Int {
                        as_const_int(arena, *n as i32, scope)
                    } else if signed && ty.borrow().kind == TypeKind::Long {
                        as_const_long(arena, *n as i64, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(arena, *n as u32, scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(arena, *n as u64, scope)
                    } else {
                        id
                    }
                }
                _ => id,
            }
        }
        AstKind::Call { expr: callee, .. } => {
            fold_child(arena, *callee);
            id
        }
        AstKind::ConstInt(_) => id,
        AstKind::ConstLong(_) => id,
        AstKind::ConstUnsignedInt(_) => id,
        AstKind::ConstUnsignedLong(_) => id,
        AstKind::ConstDouble(_) => id,
        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(expr_id) = value {
                fold_child(arena, *expr_id);
            }
            id
        }
        AstKind::CompoundInitialiser(initialiser) => {
            for elem_initialiser in &initialiser.initialisers {
                fold_child(arena, *elem_initialiser);
            }

            id
        }
        AstKind::PreIncr { .. }
        | AstKind::PreDecr { .. }
        | AstKind::PostIncr { .. }
        | AstKind::PostDecr { .. } => id,
        _ => {
            unreachable!();
        }
    }
}

pub fn is_callable(arena: &AstArena, symtab: &SymTab, id: AstId) -> bool {
    if let AstKind::Identifier { .. } = &arena[id].kind {
        if let Some(sym) = resolve(symtab, arena, &id) {
            if let Some(node) = sym_as_node(symtab, sym) {
                match &arena[node].kind {
                    AstKind::Function { .. } => return true,
                    _ => return false,
                }
            }
        }
    }

    false
}

pub fn is_lvalue(arena: &AstArena, id: AstId) -> bool {
    match &arena[id].kind {
        AstKind::Identifier { .. } => true,
        AstKind::Deref { .. } => true,
        AstKind::Subscript { .. } => true,
        _ => false,
    }
}

pub fn is_const_unsigned_int_expr(arena: &AstArena, id: AstId) -> bool {
    match &arena[id].kind {
        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(subexpr) = value {
                is_const_unsigned_int_expr(arena, *subexpr)
            } else {
                true
            }
        }
        AstKind::Cast { expr: subexpr, .. } => {
            is_const_unsigned_int_expr(arena, *subexpr)
        }
        AstKind::ConstUnsignedInt(_) | AstKind::ConstUnsignedLong(_) => true,
        _ => false,
    }
}

pub fn is_const_int_expr(arena: &AstArena, id: AstId) -> bool {
    match &arena[id].kind {
        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(subexpr) = value {
                is_const_int_expr(arena, *subexpr)
            } else {
                true
            }
        }
        AstKind::Cast { expr: subexpr, .. } => {
            is_const_int_expr(arena, *subexpr)
        }
        AstKind::ConstInt(_) | AstKind::ConstLong(_) => true,
        _ => false,
    }
}

pub fn is_const_double_expr(arena: &AstArena, id: AstId) -> bool {
    match &arena[id].kind {
        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(subexpr) = value {
                is_const_double_expr(arena, *subexpr)
            } else {
                true
            }
        }
        AstKind::Cast { expr: subexpr, .. } => {
            is_const_double_expr(arena, *subexpr)
        }
        AstKind::ConstDouble(_) => true,
        _ => false,
    }
}

pub fn is_const_expr(arena: &AstArena, id: AstId) -> bool {
    match &arena[id].kind {
        AstKind::CompoundInitialiser(initialiser) => {
            for elem_initialiser in &initialiser.initialisers {
                if !is_const_expr(arena, *elem_initialiser) {
                    return false;
                }
            }

            true
        }
        _ => {
            is_const_int_expr(arena, id)
                || is_const_unsigned_int_expr(arena, id)
                || is_const_double_expr(arena, id)
        }
    }
}

pub fn is_null_pointer_const_expr(arena: &AstArena, id: AstId) -> bool {
    if is_const_int_expr(arena, id) && const_int_value(arena, id) == 0 {
        true
    } else if is_const_unsigned_int_expr(arena, id)
        && const_unsigned_int_value(arena, id) == 0
    {
        true
    } else {
        false
    }
}

pub fn const_int_value(arena: &AstArena, id: AstId) -> i64 {
    assert!(is_const_int_expr(arena, id));
    match &arena[id].kind {
        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(subexpr) = value {
                const_int_value(arena, *subexpr)
            } else {
                unreachable!()
            }
        }
        AstKind::Cast { expr: subexpr, .. } => const_int_value(arena, *subexpr),
        AstKind::ConstInt(value) => *value as i32 as i64,
        AstKind::ConstLong(value) => *value,
        _ => {
            unreachable!()
        }
    }
}

pub fn const_unsigned_int_value(arena: &AstArena, id: AstId) -> u64 {
    assert!(is_const_unsigned_int_expr(arena, id));
    match &arena[id].kind {
        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(subexpr) = value {
                const_unsigned_int_value(arena, *subexpr)
            } else {
                unreachable!()
            }
        }
        AstKind::Cast { expr: subexpr, .. } => {
            const_unsigned_int_value(arena, *subexpr)
        }
        AstKind::ConstUnsignedInt(value) => *value as u32 as u64,
        AstKind::ConstUnsignedLong(value) => *value,
        _ => unreachable!(),
    }
}

#[allow(unused)]
pub fn const_double_value(arena: &AstArena, id: AstId) -> f64 {
    assert!(is_const_double_expr(arena, id));
    match &arena[id].kind {
        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(subexpr) = value {
                const_double_value(arena, *subexpr)
            } else {
                unreachable!()
            }
        }
        AstKind::Cast { expr: subexpr, .. } => {
            const_double_value(arena, *subexpr)
        }
        AstKind::ConstDouble(value) => *value,
        _ => unreachable!(),
    }
}

pub fn check(
    arena: &AstArena,
    symtab: &SymTab,
    id: AstId,
) -> Result<(), Error> {
    match &arena[id].kind {
        AstKind::Identifier { name, .. } => {
            if resolve(symtab, arena, &id).is_some() {
                Ok(())
            } else {
                Err(error(Parsing(ParsingError::UndeclaredIdentifier(
                    arena.token_str(name).to_string(),
                ))))
            }
        }

        AstKind::CompoundAssign { left, right }
        | AstKind::Assign { left, right } => {
            if !is_lvalue(arena, *left) {
                return Err(error(Semantic(SemanticError::NotAnLvalue)));
            } else {
                check(arena, symtab, *left)?;
                check(arena, symtab, *right)?;
            }

            Ok(())
        }

        AstKind::Add { left, right }
        | AstKind::Subtract { left, right }
        | AstKind::Multiply { left, right }
        | AstKind::Divide { left, right }
        | AstKind::Modulo { left, right }
        | AstKind::LeftShift { left, right }
        | AstKind::RightShift { left, right }
        | AstKind::And { left, right }
        | AstKind::Or { left, right }
        | AstKind::Xor { left, right } => {
            check(arena, symtab, *left)?;
            check(arena, symtab, *right)?;

            Ok(())
        }

        AstKind::LogicAnd { left, right }
        | AstKind::LogicOr { left, right } => {
            check(arena, symtab, *left)?;
            check(arena, symtab, *right)?;

            Ok(())
        }

        AstKind::Equal { left, right }
        | AstKind::NotEq { left, right }
        | AstKind::Less { left, right }
        | AstKind::LessOrEq { left, right }
        | AstKind::Greater { left, right }
        | AstKind::GreaterOrEq { left, right } => {
            check(arena, symtab, *left)?;
            check(arena, symtab, *right)?;

            Ok(())
        }

        AstKind::Ternary {
            left,
            middle,
            right,
        } => {
            check(arena, symtab, *left)?;
            check(arena, symtab, *middle)?;
            check(arena, symtab, *right)?;

            Ok(())
        }

        AstKind::Negate { expr: inner }
        | AstKind::Complement { expr: inner }
        | AstKind::Not { expr: inner } => {
            check(arena, symtab, *inner)?;

            Ok(())
        }

        AstKind::AddrOf { expr: inner } => {
            if !is_lvalue(arena, *inner) {
                return Err(error(Semantic(SemanticError::NotAnLvalue)));
            } else {
                check(arena, symtab, *inner)?;
            }

            Ok(())
        }

        AstKind::Deref { expr: inner } => {
            check(arena, symtab, *inner)?;

            Ok(())
        }

        AstKind::Subscript { left, right } => {
            check(arena, symtab, *left)?;
            check(arena, symtab, *right)?;

            Ok(())
        }

        AstKind::PreIncr { expr: inner }
        | AstKind::PreDecr { expr: inner }
        | AstKind::PostIncr { expr: inner }
        | AstKind::PostDecr { expr: inner } => {
            check(arena, symtab, *inner)?;
            if !is_lvalue(arena, *inner) {
                return Err(error(Semantic(SemanticError::NotAnLvalue)));
            }

            Ok(())
        }

        AstKind::Call {
            expr: callee,
            args: _,
        } => {
            check(arena, symtab, *callee)?;

            if !is_callable(arena, symtab, *callee) {
                return Err(error(Semantic(SemanticError::NotAFunction)));
            }

            Ok(())
        }

        AstKind::Cast {
            type_spec: _,
            expr: subexpr,
        } => {
            check(arena, symtab, *subexpr)?;

            Ok(())
        }

        AstKind::ConstInt(_) => Ok(()),

        AstKind::ConstLong(_) => Ok(()),

        AstKind::ConstUnsignedInt(_) => Ok(()),

        AstKind::ConstUnsignedLong(_) => Ok(()),

        AstKind::ConstDouble(_) => Ok(()),

        AstKind::Initialiser {
            type_spec: _,
            value,
        } => {
            if let Some(subexpr) = value {
                check(arena, symtab, *subexpr)?;
            }
            Ok(())
        }

        AstKind::CompoundInitialiser(initialiser) => {
            for elem_initialiser in &initialiser.initialisers {
                check(arena, symtab, *elem_initialiser)?;
            }

            Ok(())
        }
        _ => unreachable!(),
    }
}
