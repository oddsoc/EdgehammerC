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

use crate::expr::*;
use crate::parser::{ASTKind, ASTRef, ASTVec};

fn walk(ast: &ASTRef) -> Result<(), ()> {
    let node = ast.borrow();
    let scope = node.scope.clone();

    match &node.kind {
        ASTKind::Function {
            params,
            block,
            rtype,
            ..
        } => {
            for param in params {
                walk(param)?;
            }
            walk(rtype)?;
            if let Some(block) = block {
                walk(block)?;
            }
            Ok(())
        }
        ASTKind::Block { body } => {
            for stmt in body {
                walk(stmt)?;
            }
            Ok(())
        }
        ASTKind::Variable { typ, init, .. } => {
            walk(typ)?;
            if let Some(init) = init {
                _ = eval(init.clone())?;
            }
            Ok(())
        }
        ASTKind::Return { expr } => {
            _ = eval(expr.clone())?;
            Ok(())
        }
        ASTKind::If {
            cond,
            then,
            otherwise,
        } => {
            _ = eval(cond.clone())?;
            walk(then)?;
            if let Some(otherwise) = otherwise {
                walk(otherwise)?;
            }
            Ok(())
        }
        ASTKind::ExprStmt { expr } => {
            _ = eval(expr.clone())?;
            Ok(())
        }
        ASTKind::GoTo { label } => {
            let l = scope.borrow().find_label(label).and_then(|outer_rc| {
                outer_rc
                    .downcast::<ASTRef>()
                    .ok()
                    .map(|inner_rc| (*inner_rc).clone())
            });

            if l.is_some() {
                Ok(())
            } else {
                Err(())
            }
        }
        ASTKind::Label { stmt, .. } => {
            walk(stmt)?;
            Ok(())
        }

        ASTKind::EmptyStmt => Ok(()),

        _ => Ok(()),
    }
}

pub fn analyse(ast: &ASTVec) -> Result<(), ()> {
    for node in ast {
        walk(node)?;
    }
    Ok(())
}
