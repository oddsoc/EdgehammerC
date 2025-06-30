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

use crate::ast::*;
use crate::scope::*;

pub struct Resolver;

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {}
    }

    fn walk(&self, node: &ASTRef) -> Result<(), String> {
        let scope = node.borrow().scope.clone();
        match &node.borrow().kind {
            ASTKind::Function {
                params, block, ty, ..
            } => {
                for param in params {
                    self.walk(param)?;
                }
                self.walk(ty)?;
                if let Some(block) = block {
                    self.walk(block)?;
                }
                Ok(())
            }
            ASTKind::Block { body } => {
                for stmt in body {
                    self.walk(stmt)?;
                }
                Ok(())
            }
            ASTKind::Variable { ty, init, .. } => {
                self.walk(ty)?;
                if let Some(init) = init {
                    self.walk(&init)?;
                }
                Ok(())
            }
            ASTKind::Return { expr } => {
                _ = self.walk(&expr)?;
                Ok(())
            }
            ASTKind::If {
                cond,
                then,
                otherwise,
            } => {
                self.walk(cond)?;
                self.walk(then)?;
                if let Some(otherwise) = otherwise {
                    self.walk(otherwise)?;
                }
                Ok(())
            }
            ASTKind::DoWhile { cond, body } => {
                self.walk(cond)?;
                self.walk(body)?;
                Ok(())
            }
            ASTKind::While { cond, body } => {
                self.walk(cond)?;
                self.walk(body)?;
                Ok(())
            }
            ASTKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init) = init {
                    self.walk(&init)?;
                }

                if let Some(cond) = cond {
                    self.walk(&cond)?;
                }

                if let Some(post) = post {
                    self.walk(&post)?;
                }

                self.walk(&body)?;

                Ok(())
            }

            ASTKind::Continue { .. } => Ok(()),

            ASTKind::Break { .. } => Ok(()),
            ASTKind::ExprStmt { expr } => {
                self.walk(expr)?;
                Ok(())
            }
            ASTKind::GoTo { label } => {
                if find_label(scope, label).is_some() {
                    Ok(())
                } else {
                    Err(format!("no such label: {}", label))
                }
            }

            ASTKind::Label { stmt, .. } => {
                self.walk(stmt)?;
                Ok(())
            }

            ASTKind::Case { expr, stmt, idx: _ } => {
                self.walk(expr)?;
                self.walk(stmt)?;
                Ok(())
            }

            ASTKind::Default { stmt } => {
                self.walk(stmt)?;

                Ok(())
            }

            ASTKind::Switch { cond, body, cases } => {
                self.walk(cond)?;
                self.walk(body)?;

                for case in cases {
                    self.walk(case)?;
                }
                Ok(())
            }

            ASTKind::EmptyStmt => Ok(()),

            ASTKind::Identifier { name, sym: _ } => {
                if node.borrow().resolve_node().is_some() {
                    Ok(())
                } else {
                    Err(format!("no such identifier: {}", name))
                }
            }

            ASTKind::Assign { left, right } => {
                self.walk(left)?;
                self.walk(right)?;
                Ok(())
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
            | ASTKind::Xor { left, right } => {
                self.walk(left)?;
                self.walk(right)?;
                Ok(())
            }

            ASTKind::LogicAnd { left, right }
            | ASTKind::LogicOr { left, right } => {
                self.walk(left)?;
                self.walk(right)?;
                Ok(())
            }

            ASTKind::Equal { left, right }
            | ASTKind::NotEq { left, right }
            | ASTKind::LessThan { left, right }
            | ASTKind::LessOrEq { left, right }
            | ASTKind::GreaterThan { left, right }
            | ASTKind::GreaterOrEq { left, right } => {
                self.walk(left)?;
                self.walk(right)?;
                Ok(())
            }

            ASTKind::Conditional {
                left,
                middle,
                right,
            } => {
                self.walk(left)?;
                self.walk(middle)?;
                self.walk(right)?;
                Ok(())
            }

            ASTKind::Negate { expr: inner }
            | ASTKind::Complement { expr: inner }
            | ASTKind::Not { expr: inner } => {
                self.walk(inner)?;
                Ok(())
            }

            ASTKind::PreIncr { expr: inner }
            | ASTKind::PostIncr { expr: inner }
            | ASTKind::PreDecr { expr: inner }
            | ASTKind::PostDecr { expr: inner } => {
                self.walk(inner)?;
                Ok(())
            }

            ASTKind::Call { expr: callee, args } => {
                self.walk(callee)?;

                for arg in args {
                    self.walk(arg)?
                }

                Ok(())
            }

            _ => Ok(()),
        }
    }

    pub fn run(&self, ast: &Vec<ASTRef>) -> Result<(), String> {
        for node in ast {
            self.walk(node)?;
        }

        Ok(())
    }
}
