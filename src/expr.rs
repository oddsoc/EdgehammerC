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
use std::rc::Rc;

use crate::ast::*;
use crate::scope::*;
use crate::types::*;

fn as_const_int(n: i32, scope: &ScopeRef) -> AstRef {
    Rc::new(RefCell::new(Ast {
        id: 0,
        ty: int_type(true),
        kind: AstKind::ConstInt(n),
        scope: scope.clone(),
    }))
}

fn as_const_unsigned_int(n: i32, scope: &ScopeRef) -> AstRef {
    Rc::new(RefCell::new(Ast {
        id: 0,
        ty: int_type(false),
        kind: AstKind::ConstInt(n),
        scope: scope.clone(),
    }))
}

fn as_const_long(n: i64, scope: &ScopeRef) -> AstRef {
    Rc::new(RefCell::new(Ast {
        id: 0,
        ty: long_type(true),
        kind: AstKind::ConstLong(n),
        scope: scope.clone(),
    }))
}

fn as_const_unsigned_long(n: i64, scope: &ScopeRef) -> AstRef {
    Rc::new(RefCell::new(Ast {
        id: 0,
        ty: long_type(false),
        kind: AstKind::ConstLong(n),
        scope: scope.clone(),
    }))
}

pub fn fold(ast: &AstRef) -> AstRef {
    let scope = scope_of(ast);

    match &ast.borrow().kind {
        AstKind::Identifier { .. } => ast.clone(),
        AstKind::Assign { .. } => ast.clone(),
        AstKind::LogicAnd { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs != 0 && *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::LogicOr { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs != 0 || *rhs != 0 { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Add { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_add(*rhs);
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_add(*rhs);
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_add(*rhs as u32) as i32;
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_add(*rhs as u64) as i64;
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Subtract { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_sub(*rhs);
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_sub(*rhs);
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_sub(*rhs as u32) as i32;
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_sub(*rhs as u64) as i64;
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Multiply { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_mul(*rhs);
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_mul(*rhs);
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_mul(*rhs as u32) as i32;
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_mul(*rhs as u64) as i64;
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Divide { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs / *rhs };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs / *rhs };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u32) / (*rhs as u32) as u32
                    } as i32;
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u64) / (*rhs as u64) as u64
                    } as i64;
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Modulo { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs % *rhs };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *rhs == 0 { 0 } else { *lhs % *rhs };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u32) % (*rhs as u32) as u32
                    } as i32;
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *rhs == 0 {
                        0
                    } else {
                        (*lhs as u64) % (*rhs as u64) as u64
                    } as i64;
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::And { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = *lhs & *rhs;
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = *lhs & *rhs;
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) & (*rhs as u32) as u32;
                    as_const_unsigned_int(res as i32, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) & (*rhs as u64) as u64;
                    as_const_unsigned_long(res as i64, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Or { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = *lhs | *rhs;
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = *lhs | *rhs;
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) | (*rhs as u32) as u32;
                    as_const_unsigned_int(res as i32, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) | (*rhs as u64) as u64;
                    as_const_unsigned_long(res as i64, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::LShift { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = (*lhs).wrapping_shl(*rhs as u32);
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = (*lhs).wrapping_shl(*rhs as u32);
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32).wrapping_shl(*rhs as u32) as i32;
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64).wrapping_shl(*rhs as u32) as i64;
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::RShift { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *rhs >= 0 { *lhs >> *rhs } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *rhs >= 0 { *lhs >> *rhs } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) >> (*rhs as u32);
                    as_const_unsigned_int(res as i32, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) >> (*rhs as u32);
                    as_const_unsigned_long(res as i64, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Xor { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = *lhs ^ *rhs;
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = *lhs ^ *rhs;
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = (*lhs as u32) ^ (*rhs as u32) as u32;
                    as_const_unsigned_int(res as i32, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = (*lhs as u64) ^ (*rhs as u64) as u64;
                    as_const_unsigned_long(res as i64, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Equal { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs == *rhs { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::NotEq { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if *lhs != *rhs { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::LessThan { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs < *rhs { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs < *rhs { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if (*lhs as u32) < (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if (*lhs as u64) < (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::LessOrEq { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs <= *rhs { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs <= *rhs { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res =
                        if (*lhs as u32) <= (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res =
                        if (*lhs as u64) <= (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::GreaterThan { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs > *rhs { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs > *rhs { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res = if (*lhs as u32) > (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res = if (*lhs as u64) > (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::GreaterOrEq { left, right } => {
            replace(left, &fold(left));
            replace(right, &fold(right));

            match (&left.borrow().kind, &right.borrow().kind) {
                (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) => {
                    let res = if *lhs >= *rhs { 1 } else { 0 };
                    as_const_int(res, &scope)
                }
                (AstKind::ConstLong(lhs), AstKind::ConstLong(rhs)) => {
                    let res = if *lhs >= *rhs { 1 } else { 0 };
                    as_const_long(res, &scope)
                }
                (
                    AstKind::ConstUnsignedInt(lhs),
                    AstKind::ConstUnsignedInt(rhs),
                ) => {
                    let res =
                        if (*lhs as u32) >= (*rhs as u32) { 1 } else { 0 };
                    as_const_unsigned_int(res, &scope)
                }
                (
                    AstKind::ConstUnsignedLong(lhs),
                    AstKind::ConstUnsignedLong(rhs),
                ) => {
                    let res =
                        if (*lhs as u64) >= (*rhs as u64) { 1 } else { 0 };
                    as_const_unsigned_long(res, &scope)
                }
                _ => ast.clone(),
            }
        }

        AstKind::Ternary {
            left,
            middle,
            right,
        } => {
            replace(left, &fold(left));
            replace(middle, &fold(middle));
            replace(right, &fold(right));

            match &left.borrow().kind {
                AstKind::ConstInt(n) => {
                    if *n != 0 {
                        fold(middle).clone()
                    } else {
                        fold(right).clone()
                    }
                }
                AstKind::ConstLong(n) => {
                    if *n != 0 {
                        fold(middle).clone()
                    } else {
                        fold(right).clone()
                    }
                }
                _ => ast.clone(),
            }
        }

        AstKind::Not { expr: inner } => {
            replace(inner, &fold(inner));

            match &inner.borrow().kind {
                AstKind::ConstInt(n) => {
                    if *n != 0 {
                        as_const_int(0, &scope)
                    } else {
                        as_const_int(1, &scope)
                    }
                }
                AstKind::ConstLong(n) => {
                    if *n != 0 {
                        as_const_int(0, &scope)
                    } else {
                        as_const_int(1, &scope)
                    }
                }
                AstKind::ConstUnsignedInt(n) => {
                    if *n != 0 {
                        as_const_unsigned_int(0, &scope)
                    } else {
                        as_const_unsigned_int(1, &scope)
                    }
                }
                AstKind::ConstUnsignedLong(n) => {
                    if *n != 0 {
                        as_const_unsigned_long(0, &scope)
                    } else {
                        as_const_unsigned_long(1, &scope)
                    }
                }
                _ => ast.clone(),
            }
        }
        AstKind::Negate { expr: inner } => {
            replace(inner, &fold(inner));
            match &inner.borrow().kind {
                AstKind::ConstInt(n) => as_const_int(-(*n), &scope),
                AstKind::ConstLong(n) => as_const_long(-(*n), &scope),
                AstKind::ConstUnsignedInt(n) => {
                    as_const_int(-(*n as i32), &scope)
                }
                AstKind::ConstUnsignedLong(n) => {
                    as_const_long(-(*n as i64), &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Complement { expr: inner } => {
            replace(inner, &fold(inner));

            match &inner.borrow().kind {
                AstKind::ConstInt(n) => as_const_int(!(*n), &scope),
                AstKind::ConstLong(n) => as_const_long(!(*n), &scope),
                AstKind::ConstUnsignedInt(n) => {
                    as_const_int(!(*n as i32), &scope)
                }
                AstKind::ConstUnsignedLong(n) => {
                    as_const_long(!(*n as i64), &scope)
                }
                _ => ast.clone(),
            }
        }
        AstKind::Cast { expr: inner, .. } => {
            replace(inner, &fold(inner));
            let ty = type_of(ast);
            let signed = is_signed(&ty);

            match &inner.borrow().kind {
                AstKind::ConstInt(n) => {
                    if ty.borrow().kind == TypeKind::Int {
                        as_const_int(*n, &scope)
                    } else if ty.borrow().kind == TypeKind::Long {
                        as_const_long(*n as i64, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(*n, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(*n as i64, &scope)
                    } else {
                        ast.clone()
                    }
                }
                AstKind::ConstLong(n) => {
                    if ty.borrow().kind == TypeKind::Int {
                        as_const_int(*n as i32, &scope)
                    } else if ty.borrow().kind == TypeKind::Long {
                        as_const_long(*n, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(*n as i32, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(*n, &scope)
                    } else {
                        ast.clone()
                    }
                }
                AstKind::ConstUnsignedInt(n) => {
                    if ty.borrow().kind == TypeKind::Int {
                        as_const_int(*n as i32, &scope)
                    } else if ty.borrow().kind == TypeKind::Long {
                        as_const_long(*n as i64, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(*n as i32, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(*n as i64, &scope)
                    } else {
                        ast.clone()
                    }
                }
                AstKind::ConstUnsignedLong(n) => {
                    if ty.borrow().kind == TypeKind::Int {
                        as_const_int(*n as i32, &scope)
                    } else if ty.borrow().kind == TypeKind::Long {
                        as_const_long(*n as i64, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Int {
                        as_const_unsigned_int(*n as i32, &scope)
                    } else if !signed && ty.borrow().kind == TypeKind::Long {
                        as_const_unsigned_long(*n as i64, &scope)
                    } else {
                        ast.clone()
                    }
                }
                _ => ast.clone(),
            }
        }
        AstKind::Call { expr: callee, .. } => {
            replace(callee, &fold(callee));
            ast.clone()
        }
        AstKind::ConstInt(_) => ast.clone(),
        AstKind::ConstLong(_) => ast.clone(),
        AstKind::ConstUnsignedInt(_) => ast.clone(),
        AstKind::ConstUnsignedLong(_) => ast.clone(),
        AstKind::StaticInitializer(c_expr) => {
            replace(c_expr, &fold(c_expr));
            c_expr.clone()
        }
        AstKind::Initializer(expr) => {
            replace(expr, &fold(expr));
            expr.clone()
        }
        AstKind::PostIncr { .. } | AstKind::PostDecr { .. } => ast.clone(),
        _ => {
            unreachable!();
        }
    }
}

pub fn is_callable(expr: &AstRef) -> bool {
    if let AstKind::Identifier { .. } = &expr.borrow().kind {
        if let Some(sym) = resolve(expr) {
            if let Some(node) = sym_as_node(sym.clone()) {
                match &node.borrow().kind {
                    AstKind::Function { .. } => return true,
                    _ => return false,
                }
            }
        }
    }

    false
}

pub fn is_lvalue(expr: &AstRef) -> bool {
    matches!(&expr.borrow().kind, AstKind::Identifier { .. })
}

pub fn is_const_unsigned_int_expr(expr: &AstRef) -> bool {
    match &expr.borrow().kind {
        AstKind::StaticInitializer(subexpr) => {
            is_const_unsigned_int_expr(subexpr)
        }
        AstKind::Initializer(subexpr) => is_const_unsigned_int_expr(subexpr),
        AstKind::ConstUnsignedInt(_) | AstKind::ConstUnsignedLong(_) => true,
        _ => false,
    }
}

pub fn is_const_int_expr(expr: &AstRef) -> bool {
    match &expr.borrow().kind {
        AstKind::StaticInitializer(subexpr) => is_const_int_expr(subexpr),
        AstKind::Initializer(subexpr) => is_const_int_expr(subexpr),
        AstKind::ConstInt(_) | AstKind::ConstLong(_) => true,
        _ => false,
    }
}

pub fn const_int_value(expr: &AstRef) -> i64 {
    assert!(is_const_int_expr(expr));
    match &expr.borrow().kind {
        AstKind::ConstInt(value) => *value as i64,
        AstKind::ConstLong(value) => *value,
        _ => unreachable!(),
    }
}

pub fn const_unsigned_int_value(expr: &AstRef) -> u64 {
    assert!(is_const_int_expr(expr));
    match &expr.borrow().kind {
        AstKind::ConstUnsignedInt(value) => *value as u64,
        AstKind::ConstUnsignedLong(value) => *value,
        _ => unreachable!(),
    }
}

pub fn check(expr: &AstRef) -> Result<(), String> {
    let node = expr.borrow();
    match &node.kind {
        AstKind::Identifier { name, .. } => {
            if resolve(&expr).is_some() {
                Ok(())
            } else {
                Err(format!("{} not found", name))
            }
        }

        AstKind::Assign { left, right } => {
            if !is_lvalue(left) {
                return Err("not an lvalue".to_string());
            } else {
                check(left)?;
                check(right)?;
            }

            Ok(())
        }

        AstKind::Add { left, right }
        | AstKind::Subtract { left, right }
        | AstKind::Multiply { left, right }
        | AstKind::Divide { left, right }
        | AstKind::Modulo { left, right }
        | AstKind::LShift { left, right }
        | AstKind::RShift { left, right }
        | AstKind::And { left, right }
        | AstKind::Or { left, right }
        | AstKind::Xor { left, right } => {
            check(left)?;
            check(right)?;

            Ok(())
        }

        AstKind::LogicAnd { left, right }
        | AstKind::LogicOr { left, right } => {
            check(left)?;
            check(right)?;

            Ok(())
        }

        AstKind::Equal { left, right }
        | AstKind::NotEq { left, right }
        | AstKind::LessThan { left, right }
        | AstKind::LessOrEq { left, right }
        | AstKind::GreaterThan { left, right }
        | AstKind::GreaterOrEq { left, right } => {
            check(left)?;
            check(right)?;

            Ok(())
        }

        AstKind::Ternary {
            left,
            middle,
            right,
        } => {
            check(left)?;
            check(middle)?;
            check(right)?;

            Ok(())
        }

        AstKind::Negate { expr: inner }
        | AstKind::Complement { expr: inner }
        | AstKind::Not { expr: inner } => {
            check(inner)?;

            Ok(())
        }

        AstKind::PostIncr { expr: inner }
        | AstKind::PostDecr { expr: inner } => {
            check(inner)?;
            if !is_lvalue(inner) {
                return Err("not an lvalue".to_string());
            }

            Ok(())
        }

        AstKind::Call {
            expr: callee,
            args: _,
        } => {
            check(callee)?;

            if !is_callable(callee) {
                return Err("not a function".to_string());
            }

            Ok(())
        }

        AstKind::Cast {
            type_spec: _,
            expr: subexpr,
        } => {
            check(subexpr)?;

            Ok(())
        }

        AstKind::ConstInt(_) => Ok(()),

        AstKind::ConstLong(_) => Ok(()),

        AstKind::ConstUnsignedInt(_) => Ok(()),

        AstKind::ConstUnsignedLong(_) => Ok(()),

        AstKind::StaticInitializer(c_expr) => {
            check(c_expr)?;
            Ok(())
        }

        AstKind::Initializer(expr) => {
            check(expr)?;
            Ok(())
        }

        _ => unreachable!(),
    }
}
