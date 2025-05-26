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

use std::rc::Rc;

use crate::ast::{ASTKind, AST};
use crate::expr::*;
use crate::scope::ScopeKind;

fn walk(ast: &Rc<AST>) -> Result<(), ()> {
    let scope = ast.scope.clone();

    match &ast.kind {
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
                _ = eval(init.clone(), false)?;
            }
            Ok(())
        }
        ASTKind::Return { expr } => {
            _ = eval(expr.clone(), false)?;
            Ok(())
        }
        ASTKind::If {
            cond,
            then,
            otherwise,
        } => {
            _ = eval(cond.clone(), false)?;
            walk(then)?;
            if let Some(otherwise) = otherwise {
                walk(otherwise)?;
            }
            Ok(())
        }
        ASTKind::DoWhile { cond, body } => {
            _ = eval(cond.clone(), false)?;
            walk(body)?;
            Ok(())
        }
        ASTKind::While { cond, body } => {
            _ = eval(cond.clone(), false)?;
            walk(body)?;
            Ok(())
        }
        ASTKind::For {
            init,
            cond,
            post,
            body,
        } => {
            if let Some(init) = init {
                walk(&init)?;
            }

            if let Some(cond) = cond {
                _ = eval(cond.clone(), false)?;
            }

            if let Some(post) = post {
                walk(&post)?;
            }

            walk(&body)?;

            Ok(())
        }

        ASTKind::Continue { .. } => {
            if scope.borrow().kind == ScopeKind::Loop
                || scope.borrow().loop_scope().is_some()
            {
                Ok(())
            } else {
                Err(())
            }
        }

        ASTKind::Break { .. } => {
            if scope.borrow().kind == ScopeKind::Loop
                || scope.borrow().kind == ScopeKind::Switch
                || scope.borrow().loop_or_switch_scope().is_some()
            {
                Ok(())
            } else {
                Err(())
            }
        }
        ASTKind::ExprStmt { expr } => {
            let _ = eval(expr.clone(), false)?;
            Ok(())
        }
        ASTKind::GoTo { label } => {
            if scope.borrow().find_label(label).is_some() {
                Ok(())
            } else {
                Err(())
            }
        }

        ASTKind::Label { stmt, .. } => {
            walk(stmt)?;
            Ok(())
        }

        ASTKind::Case { expr, stmt, idx: _ } => {
            if scope.borrow().kind == ScopeKind::Switch
                || scope.borrow().switch_scope().is_some()
            {
                _ = eval(expr.clone(), true)?;
                walk(stmt)?;
                return Ok(());
            }

            Err(())
        }

        ASTKind::Default { stmt } => {
            if scope.borrow().kind == ScopeKind::Switch
                || scope.borrow().switch_scope().is_some()
            {
                walk(stmt)?;
                return Ok(());
            }

            Err(())
        }

        ASTKind::Switch { cond, body, cases } => {
            _ = eval(cond.clone(), false)?;
            let mut case_values = std::collections::HashSet::new();
            let mut has_default = false;
            for case in cases {
                match &case.kind {
                    ASTKind::Case { expr, stmt, .. } => {
                        let case_eval = eval(expr.clone(), true)?;
                        if let ASTKind::ConstInt(value) = case_eval.kind {
                            if !case_values.insert(value) {
                                return Err(());
                            }
                        } else {
                            return Err(());
                        }
                        walk(stmt)?;
                    }
                    ASTKind::Default { stmt } => {
                        if has_default {
                            return Err(());
                        }
                        has_default = true;
                        walk(stmt)?;
                    }
                    _ => return Err(()),
                }
            }
            walk(body)?;
            Ok(())
        }
        ASTKind::EmptyStmt => Ok(()),

        _ => Ok(()),
    }
}

pub fn analyse(ast: &Vec<Rc<AST>>) -> Result<(), ()> {
    for node in ast {
        walk(node)?;
    }
    Ok(())
}
