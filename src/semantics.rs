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

use std::collections::HashSet;

use crate::ast::*;
use crate::errors::{Error, ErrorClass::Semantic, SemanticError, error};
use crate::expr::*;
use crate::symtab::*;

pub struct Analyser;

impl Analyser {
    pub fn new() -> Analyser {
        Analyser {}
    }

    fn walk(
        &self,
        arena: &mut AstArena,
        symtab: &SymTab,
        id: AstId,
    ) -> Result<(), Error> {
        let kind = arena[id].kind.clone();

        match &kind {
            AstKind::Function {
                params,
                block,
                type_spec,
                ..
            } => {
                for &param in params {
                    if let AstKind::Parameter { .. } = &arena[param].kind {
                        self.walk(arena, symtab, param)?;
                    } else if block.is_some() {
                        return Err(error(Semantic(
                            SemanticError::UnnamedParameterInFunctionDefinition,
                        )));
                    } else {
                        self.walk(arena, symtab, param)?;
                    }
                }

                self.walk(arena, symtab, *type_spec)?;

                if let Some(block) = block {
                    self.walk(arena, symtab, *block)?;
                }

                Ok(())
            }
            AstKind::Block { body } => {
                for &stmt in body {
                    self.walk(arena, symtab, stmt)?;
                }
                Ok(())
            }

            AstKind::Variable {
                type_spec, init, ..
            } => {
                self.walk(arena, symtab, *type_spec)?;
                if let Some(init) = init {
                    let init_val = *init;
                    check(arena, symtab, init_val)?;
                    let folded = fold(arena, init_val);
                    arena[init_val] = arena[folded].clone();
                    if let Some(sym) = resolve(symtab, arena, &id) {
                        if has_static_storage_duration(symtab, sym) {
                            if !is_const_expr(arena, init_val) {
                                return Err(error(Semantic(
                                    SemanticError::NotAConstExpression,
                                )));
                            }
                        }
                    }
                }

                Ok(())
            }
            AstKind::Parameter { type_spec, .. } => {
                self.walk(arena, symtab, *type_spec)?;
                Ok(())
            }
            AstKind::Return { expr, .. } => {
                let expr_val = *expr;
                check(arena, symtab, expr_val)?;
                let folded = fold(arena, expr_val);
                arena[expr_val] = arena[folded].clone();
                Ok(())
            }
            AstKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond_val = *cond;
                check(arena, symtab, cond_val)?;
                let folded = fold(arena, cond_val);
                arena[cond_val] = arena[folded].clone();
                self.walk(arena, symtab, *then)?;
                if let Some(otherwise) = otherwise {
                    self.walk(arena, symtab, *otherwise)?;
                }
                Ok(())
            }
            AstKind::DoWhile { cond, body } => {
                let cond_val = *cond;
                check(arena, symtab, cond_val)?;
                let folded = fold(arena, cond_val);
                arena[cond_val] = arena[folded].clone();
                self.walk(arena, symtab, *body)?;
                Ok(())
            }
            AstKind::While { cond, body } => {
                let cond_val = *cond;
                check(arena, symtab, cond_val)?;
                let folded = fold(arena, cond_val);
                arena[cond_val] = arena[folded].clone();
                self.walk(arena, symtab, *body)?;
                Ok(())
            }
            AstKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init) = init {
                    self.walk(arena, symtab, *init)?;
                }

                if let Some(cond) = cond {
                    let cond_val = *cond;
                    check(arena, symtab, cond_val)?;
                    let folded = fold(arena, cond_val);
                    arena[cond_val] = arena[folded].clone();
                }

                if let Some(post) = post {
                    self.walk(arena, symtab, *post)?;
                }

                self.walk(arena, symtab, *body)?;

                Ok(())
            }

            AstKind::Continue { .. } => {
                let scope = arena[id].scope;
                if symtab.upto(scope, ScopeKind::Loop).is_some() {
                    Ok(())
                } else {
                    Err(error(Semantic(SemanticError::ContinueNotInALoop)))
                }
            }

            AstKind::Break { .. } => {
                let scope = arena[id].scope;
                if symtab
                    .upto_any(scope, &[ScopeKind::Loop, ScopeKind::Switch])
                    .is_some()
                {
                    Ok(())
                } else {
                    Err(error(Semantic(SemanticError::BreakNotInALoopOrSwitch)))
                }
            }
            AstKind::ExprStmt { expr } => {
                let expr_val = *expr;
                check(arena, symtab, expr_val)?;
                let folded = fold(arena, expr_val);
                arena[expr_val] = arena[folded].clone();
                Ok(())
            }
            AstKind::GoTo { label } => {
                let scope = arena[id].scope;
                if symtab.get_label(scope, arena.token_str(label)).is_some() {
                    Ok(())
                } else {
                    Err(error(Semantic(SemanticError::LabelNotFound(
                        arena.token_str(label).to_string(),
                    ))))
                }
            }

            AstKind::Label { stmt, .. } => {
                self.walk(arena, symtab, *stmt)?;
                Ok(())
            }

            AstKind::Case { expr, stmt, idx: _ } => {
                let scope = arena[id].scope;
                if symtab.upto(scope, ScopeKind::Switch).is_some() {
                    let expr_val = *expr;
                    check(arena, symtab, expr_val)?;
                    let folded = fold(arena, expr_val);
                    arena[expr_val] = arena[folded].clone();

                    if !is_const_int_expr(arena, expr_val)
                        && !is_const_unsigned_int_expr(arena, expr_val)
                    {
                        return Err(error(Semantic(
                            SemanticError::NotAConstExpression,
                        )));
                    }

                    self.walk(arena, symtab, *stmt)?;

                    return Ok(());
                }

                Err(error(Semantic(SemanticError::CaseOutsideOfSwitch)))
            }

            AstKind::Default { stmt } => {
                let scope = arena[id].scope;
                if symtab.upto(scope, ScopeKind::Switch).is_some() {
                    self.walk(arena, symtab, *stmt)?;
                    return Ok(());
                }

                Err(error(Semantic(SemanticError::DefaultOutsideOfSwitch)))
            }

            AstKind::Switch { cond, body, cases } => {
                let cond_val = *cond;
                check(arena, symtab, cond_val)?;
                let folded = fold(arena, cond_val);
                arena[cond_val] = arena[folded].clone();
                self.walk(arena, symtab, *body)?;
                let mut case_values: HashSet<u64> = HashSet::new();
                let mut has_default = false;
                for &case in cases {
                    let case_kind = arena[case].kind.clone();
                    match &case_kind {
                        AstKind::Case { expr, stmt, .. } => {
                            if is_const_unsigned_int_expr(arena, *expr) {
                                if !case_values.insert(
                                    const_unsigned_int_value(arena, *expr),
                                ) {
                                    return Err(error(Semantic(
                                        SemanticError::DuplicateCaseExpression,
                                    )));
                                }
                            } else if is_const_int_expr(arena, *expr) {
                                if !case_values
                                    .insert(const_int_value(arena, *expr) as u64)
                                {
                                    return Err(error(Semantic(
                                        SemanticError::DuplicateCaseExpression,
                                    )));
                                }
                            } else {
                                return Err(error(Semantic(
                                    SemanticError::NotAConstExpression,
                                )));
                            }

                            self.walk(arena, symtab, *stmt)?;
                        }
                        AstKind::Default { stmt } => {
                            if has_default {
                                return Err(error(Semantic(
                                    SemanticError::DuplicateDefaultCase,
                                )));
                            }
                            has_default = true;
                            self.walk(arena, symtab, *stmt)?;
                        }
                        _ => unreachable!(),
                    }
                }
                Ok(())
            }
            AstKind::EmptyStmt => Ok(()),

            AstKind::Initialiser {
                type_spec: _,
                value,
            } => {
                if let Some(expr) = value {
                    let expr_val = *expr;
                    check(arena, symtab, expr_val)?;
                    let folded = fold(arena, expr_val);
                    arena[expr_val] = arena[folded].clone();
                }
                Ok(())
            }

            _ => Ok(()),
        }
    }

    pub fn run(&self, stage: &mut AstStage) -> Result<(), Error> {
        for &node in &stage.root {
            self.walk(&mut stage.arena, &stage.symtab, node)?;
        }

        Ok(())
    }
}
