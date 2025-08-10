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
use crate::expr::*;
use crate::scope::*;

pub struct Analyser;

impl Analyser {
    pub fn new() -> Analyser {
        Analyser {}
    }

    fn walk(&self, ast: &AstRef) -> Result<(), String> {
        let scope = ast.borrow().scope.clone();

        match &ast.borrow().kind {
            AstKind::Function {
                params,
                block,
                type_spec,
                ..
            } => {
                for param in params {
                    self.walk(param)?;
                }
                self.walk(type_spec)?;
                if let Some(block) = block {
                    self.walk(block)?;
                }
                Ok(())
            }
            AstKind::Block { body } => {
                for stmt in body {
                    self.walk(stmt)?;
                }
                Ok(())
            }

            AstKind::Variable {
                type_spec, init, ..
            } => {
                self.walk(type_spec)?;
                if let Some(init) = init {
                    check(init)?;
                    //replace(init, &fold(init));
                    if let Some(sym) = resolve(ast) {
                        if has_static_storage_duration(sym) {
                            if !is_const_int_expr(init) {
                                return Err(
                                    "not a const expression".to_string()
                                );
                            }
                        }
                    }
                }

                Ok(())
            }
            AstKind::Return { expr, .. } => {
                check(expr)?;
                replace(expr, &fold(expr));
                Ok(())
            }
            AstKind::If {
                cond,
                then,
                otherwise,
            } => {
                check(cond)?;
                replace(cond, &fold(cond));
                self.walk(then)?;
                if let Some(otherwise) = otherwise {
                    self.walk(otherwise)?;
                }
                Ok(())
            }
            AstKind::DoWhile { cond, body } => {
                check(cond)?;
                replace(cond, &fold(cond));
                self.walk(body)?;
                Ok(())
            }
            AstKind::While { cond, body } => {
                check(cond)?;
                replace(cond, &fold(cond));
                self.walk(body)?;
                Ok(())
            }
            AstKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init) = init {
                    self.walk(init)?;
                }

                if let Some(cond) = cond {
                    check(cond)?;
                    replace(cond, &fold(cond));
                }

                if let Some(post) = post {
                    self.walk(post)?;
                }

                self.walk(body)?;

                Ok(())
            }

            AstKind::Continue { .. } => {
                if upto(scope.clone(), ScopeKind::Loop).is_some() {
                    Ok(())
                } else {
                    Err("continue not in a loop".to_string())
                }
            }

            AstKind::Break { .. } => {
                if upto_any(
                    scope.clone(),
                    &[ScopeKind::Loop, ScopeKind::Switch],
                )
                .is_some()
                {
                    Ok(())
                } else {
                    Err("break not in a loop or switch".to_string())
                }
            }
            AstKind::ExprStmt { expr } => {
                check(expr)?;
                replace(expr, &fold(expr));
                Ok(())
            }
            AstKind::GoTo { label } => {
                if get_label(scope.clone(), label).is_some() {
                    Ok(())
                } else {
                    Err(format!("label {} not found", label))
                }
            }

            AstKind::Label { stmt, .. } => {
                self.walk(stmt)?;
                Ok(())
            }

            AstKind::Case { expr, stmt, idx: _ } => {
                if upto(scope.clone(), ScopeKind::Switch).is_some() {
                    check(expr)?;
                    replace(expr, &fold(expr));

                    if !is_const_int_expr(expr) {
                        return Err("not a const expression".to_string());
                    }

                    self.walk(stmt)?;

                    return Ok(());
                }

                Err("case outside of a switch statement".to_string())
            }

            AstKind::Default { stmt } => {
                if upto(scope.clone(), ScopeKind::Switch).is_some() {
                    self.walk(stmt)?;
                    return Ok(());
                }

                Err("default outside of a switch statement".to_string())
            }

            AstKind::Switch { cond, body, cases } => {
                check(cond)?;
                replace(cond, &fold(cond));
                self.walk(body)?;
                let mut case_values = std::collections::HashSet::new();
                let mut has_default = false;
                for case in cases {
                    match &case.borrow().kind {
                        AstKind::Case { expr, stmt, .. } => {
                            if is_const_int_expr(expr) {
                                if !case_values.insert(const_int_value(expr)) {
                                    return Err(
                                        "duplicate case expression".to_string()
                                    );
                                }
                            } else {
                                return Err(
                                    "not a const expression".to_string()
                                );
                            }

                            self.walk(stmt)?;
                        }
                        AstKind::Default { stmt } => {
                            if has_default {
                                return Err(
                                    "duplicate default case".to_string()
                                );
                            }
                            has_default = true;
                            self.walk(stmt)?;
                        }
                        _ => unreachable!(),
                    }
                }
                Ok(())
            }
            AstKind::EmptyStmt => Ok(()),

            AstKind::StaticInitializer(c_expr) => {
                check(c_expr)?;
                replace(c_expr, &fold(c_expr));
                Ok(())
            }

            _ => Ok(()),
        }
    }

    pub fn run(&self, ast: &Vec<AstRef>) -> Result<(), String> {
        for node in ast {
            self.walk(node)?;
        }

        Ok(())
    }
}
