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

use crate::as_a;
use crate::parser::{ASTKind, ASTRef};

pub fn eval(expr: ASTRef) -> Option<ASTRef> {
    let node = expr.borrow();
    let scope = node.scope.clone();

    match &node.kind {
        ASTKind::Identifier { name } => {
            scope.borrow().find_sym(name).and_then(|outer_rc| {
                outer_rc
                    .downcast::<ASTRef>()
                    .ok()
                    .map(|inner_rc| (*inner_rc).clone())
            })
        }

        ASTKind::Assign { left, right } => {
            as_a!(Some(left), ASTKind::Identifier {
                name: _
            } => {
                if eval(left.clone()).is_some() && eval(right.clone()).is_some() {
                    return Some(expr.clone());
                }
            });

            return None;
        }

        ASTKind::Add { left, right }
        | ASTKind::Subtract { left, right }
        | ASTKind::Multiply { left, right }
        | ASTKind::Divide { left, right }
        | ASTKind::Modulo { left, right }
        | ASTKind::LShift { left, right }
        | ASTKind::RShift { left, right }
        | ASTKind::And { left, right }
        | ASTKind::Or { left, right }
        | ASTKind::Xor { left, right }
        | ASTKind::LogicAnd { left, right }
        | ASTKind::LogicOr { left, right }
        | ASTKind::Equal { left, right }
        | ASTKind::NotEq { left, right }
        | ASTKind::LessThan { left, right }
        | ASTKind::LessOrEq { left, right }
        | ASTKind::GreaterThan { left, right }
        | ASTKind::GreaterOrEq { left, right } => {
            if eval(left.clone()).is_some() && eval(right.clone()).is_some() {
                Some(expr.clone())
            } else {
                None
            }
        }

        ASTKind::Negate { expr }
        | ASTKind::Complement { expr }
        | ASTKind::Not { expr } => {
            if eval(expr.clone()).is_some() {
                Some(expr.clone())
            } else {
                None
            }
        }

        ASTKind::PreIncr { expr }
        | ASTKind::PreDecr { expr }
        | ASTKind::PostIncr { expr }
        | ASTKind::PostDecr { expr } => {
            if eval(expr.clone()).is_some() {
                as_a!(Some(expr), ASTKind::Identifier {
                    name: _
                } => {
                    if eval(expr.clone()).is_some() {
                        return Some(expr.clone());
                    } else {
                        return None;
                    }
                } else {
                    None
                })
            } else {
                None
            }
        }

        ASTKind::ConstInt(_) => Some(expr.clone()),

        _ => None,
    }
}
