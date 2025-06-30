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

#[allow(unused_imports)]
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::scope::*;

macro_rules! accept {
    ($parser:expr, $kind:path) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind) {
            $parser.advance();
            true
        } else {
            false
        }
    };

    ($parser:expr, $kind:path, $_:tt) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind(_)) {
            $parser.advance();
            true
        } else {
            false
        }
    };
}

macro_rules! expect {
    ($parser:expr, $kind:path) => {
        if !matches!($parser.next.as_ref().unwrap().kind, $kind) {
            let msg = format!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.next.as_ref().unwrap().kind
            );
            return Err(msg);
        } else {
            $parser.advance();
        }
    };

    ($parser:expr, $kind:path, $_:tt) => {
        if !matches!($parser.next.as_ref().unwrap().kind, $kind(_)) {
            let msg = format!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.next.as_ref().unwrap().kind
            );
            return Err(msg);
        } else {
            $parser.advance();
        }
    };
}

macro_rules! peek {
    ($parser:expr, $kind:path) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind) {
            true
        } else {
            false
        }
    };

    ($parser:expr, $kind:path, $_:tt) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind(_)) {
            true
        } else {
            false
        }
    };
}

macro_rules! peek_any {
    ($parser:expr) => {
        $parser.next.as_ref().unwrap().kind.clone()
    };
}

macro_rules! yank {
    ($parser:expr, $kind:path) => {
        match &$parser.last.as_ref().unwrap().kind {
            $kind(data) => data.clone(),
            _ => panic!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.last.as_ref().unwrap().kind
            ),
        }
    };
}

macro_rules! new_node {
    ($parser:expr, $variant:ident) => {
        AST::new($parser.next_node_id(), ASTKind::$variant, None, $parser.scope.clone())
    };

    ($parser:expr, $variant:ident ( $($args:expr),* $(,)? )) => {
        AST::new($parser.next_node_id(), ASTKind::$variant( $($args),* ), None, $parser.scope.clone())
    };

    ($parser:expr, $variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        AST::new($parser.next_node_id(), ASTKind::$variant { $($field: $value),*}, None, $parser.scope.clone())
    };

    ($parser:expr, $id:expr, $variant:ident) => {
        AST::new($id, ASTKind::$variant, None, $parser.scope.clone())
    };

    ($parser:expr, $id:expr, $variant:ident ( $($args:expr),* $(,)? )) => {
        AST::new($id, ASTKind::$variant( $($args),* ), None, $parser.scope.clone())
    };

    ($parser:expr, $id:expr, $variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        AST::new($id, ASTKind::$variant {
            $($field: $value),*
        }, None, $parser.scope.clone())
    };
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    last: Option<Token>,
    next: Option<Token>,
    nr_nodes: usize,
    scope: ScopeRef,
    cases: Vec<Vec<ASTRef>>,
    validate: bool,
}

fn precedence_of(kind: &TokenKind) -> i32 {
    return match kind {
        TokenKind::Mult | TokenKind::Div | TokenKind::Mod => 50,
        TokenKind::Plus | TokenKind::Minus => 45,
        TokenKind::LShift | TokenKind::RShift => 40,
        TokenKind::LThan
        | TokenKind::LThanEq
        | TokenKind::GThan
        | TokenKind::GThanEq => 35,
        TokenKind::Eq | TokenKind::NotEq => 30,
        TokenKind::And => 25,
        TokenKind::Xor => 20,
        TokenKind::Or => 15,
        TokenKind::LAnd => 10,
        TokenKind::LOr => 5,
        TokenKind::QMark => 3,
        TokenKind::Assign
        | TokenKind::PlusEq
        | TokenKind::MinusEq
        | TokenKind::MultEq
        | TokenKind::DivEq
        | TokenKind::ModEq
        | TokenKind::OrEq
        | TokenKind::AndEq
        | TokenKind::InvEq => 1,
        _ => 0,
    };
}

impl Parser {
    pub fn new(filename: &str, validate: bool) -> Self {
        let scope = Scope::open(ScopeKind::Global, None);
        let mut parser = Self {
            lexer: Lexer::new(filename),
            last: None,
            next: None,
            nr_nodes: 0,
            scope: scope.clone(),
            cases: vec![],
            validate: validate,
        };

        parser.advance();

        return parser;
    }

    fn next_node_id(&mut self) -> usize {
        let id = self.nr_nodes;
        self.nr_nodes += 1;
        id
    }

    fn open_scope(&mut self, kind: ScopeKind) {
        self.scope = Scope::open(kind, Some(self.scope.clone()));
    }

    fn close_scope(&mut self) {
        self.scope = self.scope.clone().borrow().close();
    }

    fn add_sym(
        &mut self,
        name: &str,
        linkage: Option<Linkage>,
        sym: Option<ASTRef>,
    ) -> Result<(), String> {
        let s = self.scope.as_ref().borrow_mut().add_sym(
            &name,
            linkage,
            sym.clone(),
            4,
            4,
        );

        if self.validate && s.is_err() {
            Err(format!("'{}' is already defined", name))
        } else {
            Ok(())
        }
    }

    fn update_sym(
        &mut self,
        name: &str,
        sym: ASTRef,
        defined: bool,
    ) -> Result<(), String> {
        let s = self.scope.as_ref().borrow_mut().update_sym(
            &name,
            sym.clone(),
            defined,
        );

        if self.validate && s.is_err() {
            Err(format!("'{}' not defined", name))
        } else {
            Ok(())
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ASTRef>, String> {
        return self.program();
    }

    fn program(&mut self) -> Result<Vec<ASTRef>, String> {
        let mut prog: Vec<ASTRef> = vec![];

        while !self.eof() {
            prog.push(self.declaration()?);
        }

        return Ok(prog);
    }

    fn declaration(&mut self) -> Result<ASTRef, String> {
        let ty = self.type_spec()?;
        expect!(self, TokenKind::Identifier, _);
        let name = yank!(self, TokenKind::Identifier);

        if peek!(self, TokenKind::LParen) {
            self.function(ty, name)
        } else {
            self.variable_decl(ty, name)
        }
    }

    fn parameter_list(&mut self) -> Result<Vec<ASTRef>, String> {
        let mut params: Vec<ASTRef> = vec![];
        let mut idx = 0;
        expect!(self, TokenKind::LParen);

        if !accept!(self, TokenKind::RParen) {
            loop {
                params.push(self.parameter(idx)?);

                if accept!(self, TokenKind::RParen) {
                    break;
                } else {
                    expect!(self, TokenKind::Comma);
                }
                idx += 1;
            }
        }

        Ok(params)
    }

    fn parameter(&mut self, idx: usize) -> Result<ASTRef, String> {
        let ty = self.type_spec()?;

        if peek!(self, TokenKind::RParen) {
            Ok(new_node!(
                self,
                Parameter {
                    name: None,
                    idx: idx,
                    ty: ty
                }
            ))
        } else {
            expect!(self, TokenKind::Identifier, _);
            let name = yank!(self, TokenKind::Identifier);
            let param = new_node!(
                self,
                Parameter {
                    name: Some(name.clone()),
                    idx: idx,
                    ty: ty,
                }
            );
            self.add_sym(&name, None, Some(param.clone()))?;
            Ok(param)
        }
    }

    fn function(&mut self, ty: ASTRef, name: String) -> Result<ASTRef, String> {
        let node_id = self.next_node_id();
        self.add_sym(&name, Some(Linkage::External), None)?;
        self.open_scope(ScopeKind::Function);
        let params = self.parameter_list()?;

        if accept!(self, TokenKind::Semicolon) {
            self.close_scope();

            let signature = new_node!(
                self,
                node_id,
                Function {
                    name: name.clone(),
                    params: params.clone(),
                    block: None,
                    ty: ty.clone(),
                    scope: self.scope.clone(),
                }
            );

            self.update_sym(&name, signature.clone(), false)?;

            return Ok(signature);
        } else {
            let body = self.function_body()?;
            self.close_scope();

            if self.validate
                && self.scope.as_ref().borrow().kind != ScopeKind::Global
            {
                return Err(
                    "function definition is not allowed here".to_string()
                );
            }

            let function = new_node!(
                self,
                node_id,
                Function {
                    name: name.clone(),
                    params: params.clone(),
                    block: Some(body),
                    ty: ty.clone(),
                    scope: self.scope.clone(),
                }
            );

            self.update_sym(&name, function.clone(), true)?;

            Ok(function)
        }
    }

    fn variable_decl(
        &mut self,
        ty: ASTRef,
        name: String,
    ) -> Result<ASTRef, String> {
        let node_id = self.next_node_id();
        self.add_sym(&name, None, None)?;

        let initializer = if accept!(self, TokenKind::Assign) {
            Some(self.expr(0)?)
        } else {
            None
        };

        expect!(self, TokenKind::Semicolon);

        let var = new_node!(
            self,
            node_id,
            Variable {
                name: name.clone(),
                ty: ty.clone(),
                init: initializer,
            }
        );

        self.update_sym(&name, var.clone(), true)?;

        Ok(var)
    }

    fn is_type_spec(&self) -> bool {
        return peek!(self, TokenKind::Int) || peek!(self, TokenKind::Void);
    }

    fn type_spec(&mut self) -> Result<ASTRef, String> {
        if accept!(self, TokenKind::Int) {
            Ok(new_node!(self, Int))
        } else {
            expect!(self, TokenKind::Void);
            Ok(new_node!(self, Void))
        }
    }

    fn statement(&mut self) -> Result<ASTRef, String> {
        if peek!(self, TokenKind::LCurly) {
            self.block()
        } else if peek!(self, TokenKind::Label, _)
            || peek!(self, TokenKind::Case)
            || peek!(self, TokenKind::Default)
        {
            self.labelled_stmt()
        } else if peek!(self, TokenKind::Return) {
            self.return_stmt()
        } else if peek!(self, TokenKind::If) {
            self.if_stmt()
        } else if peek!(self, TokenKind::While) {
            self.while_stmt()
        } else if peek!(self, TokenKind::Do) {
            self.do_while_stmt()
        } else if peek!(self, TokenKind::For) {
            self.for_stmt()
        } else if peek!(self, TokenKind::Switch) {
            self.switch_stmt()
        } else if peek!(self, TokenKind::GoTo) {
            self.goto_stmt()
        } else if peek!(self, TokenKind::Break) {
            self.break_stmt()
        } else if peek!(self, TokenKind::Continue) {
            self.continue_stmt()
        } else if accept!(self, TokenKind::Semicolon) {
            Ok(new_node!(self, EmptyStmt))
        } else {
            self.expr_stmt()
        }
    }

    fn while_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::While);
        expect!(self, TokenKind::LParen);
        let cond = self.expr(0)?;
        expect!(self, TokenKind::RParen);
        self.open_scope(ScopeKind::Loop);
        let body = self.statement()?;
        self.close_scope();

        Ok(new_node!(
            self,
            While {
                cond: cond,
                body: body
            }
        ))
    }

    fn do_while_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Do);
        self.open_scope(ScopeKind::Loop);
        let body = self.statement()?;
        self.close_scope();
        expect!(self, TokenKind::While);
        expect!(self, TokenKind::LParen);
        let cond = self.expr(0)?;
        expect!(self, TokenKind::RParen);
        expect!(self, TokenKind::Semicolon);

        Ok(new_node!(
            self,
            DoWhile {
                cond: cond,
                body: body
            }
        ))
    }

    fn for_init(&mut self) -> Result<ASTRef, String> {
        if self.is_type_spec() {
            let ty = self.type_spec()?;
            expect!(self, TokenKind::Identifier, _);
            let name = yank!(self, TokenKind::Identifier);
            Ok(self.variable_decl(ty, name)?)
        } else {
            let init = new_node!(
                self,
                ExprStmt {
                    expr: self.expr(0)?
                }
            );
            expect!(self, TokenKind::Semicolon);
            Ok(init)
        }
    }

    fn for_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::For);
        expect!(self, TokenKind::LParen);

        self.open_scope(ScopeKind::Loop);

        let init: Option<ASTRef> = if accept!(self, TokenKind::Semicolon) {
            None
        } else {
            Some(self.for_init()?)
        };

        let cond: Option<ASTRef> = if accept!(self, TokenKind::Semicolon) {
            None
        } else {
            let c = Some(self.expr(0)?);
            expect!(self, TokenKind::Semicolon);
            c
        };

        let post: Option<ASTRef> = if peek!(self, TokenKind::RParen) {
            None
        } else {
            Some(new_node!(
                self,
                ExprStmt {
                    expr: self.expr(0)?
                }
            ))
        };

        expect!(self, TokenKind::RParen);

        let body = self.statement()?;

        self.close_scope();

        Ok(new_node!(
            self,
            For {
                init: init,
                cond: cond,
                post: post,
                body: body
            }
        ))
    }

    fn switch_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Switch);
        expect!(self, TokenKind::LParen);
        let expr = self.expr(0)?;
        expect!(self, TokenKind::RParen);

        self.cases.push(vec![]);
        self.open_scope(ScopeKind::Switch);
        let stmt = self.statement()?;
        self.close_scope();

        let cases = self.cases.pop().unwrap();

        Ok(new_node!(
            self,
            Switch {
                cond: expr,
                body: stmt,
                cases: cases
            }
        ))
    }

    fn break_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Break);
        expect!(self, TokenKind::Semicolon);

        Ok(new_node!(self, Break { to: None }))
    }

    fn continue_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Continue);
        expect!(self, TokenKind::Semicolon);

        Ok(new_node!(self, Continue { to: None }))
    }

    fn labelled_stmt(&mut self) -> Result<ASTRef, String> {
        if accept!(self, TokenKind::Label, _) {
            let label = yank!(self, TokenKind::Label);
            let stmt = self.statement()?;
            let l = add_label(self.scope.clone(), &label, stmt.clone());

            if self.validate && !l.is_ok() {
                return Err(format!("'{}' label already defined", label));
            }

            Ok(new_node!(
                self,
                Label {
                    name: label,
                    stmt: stmt.clone()
                }
            ))
        } else if peek!(self, TokenKind::Case) {
            self.lexer.push_expr();
            self.advance();
            let expr = self.expr(0)?;
            self.lexer.pop_expr();
            expect!(self, TokenKind::Colon);
            let stmt = self.statement()?;
            let case_stmt = new_node!(
                self,
                Case {
                    expr: expr,
                    stmt: stmt,
                    idx: 0
                }
            );

            if let Some(cases) = self.cases.last_mut() {
                cases.push(case_stmt.clone());
            }
            Ok(case_stmt)
        } else {
            expect!(self, TokenKind::Default);
            expect!(self, TokenKind::Colon);
            let stmt = self.statement()?;
            let dflt_stmt = new_node!(self, Default { stmt: stmt });
            if let Some(cases) = self.cases.last_mut() {
                cases.push(dflt_stmt.clone());
            }
            Ok(dflt_stmt.clone())
        }
    }

    fn stmt_or_decl(&mut self) -> Result<ASTRef, String> {
        if self.is_type_spec() {
            self.declaration()
        } else {
            self.statement()
        }
    }

    fn block(&mut self) -> Result<ASTRef, String> {
        let mut body: Vec<ASTRef> = vec![];
        self.open_scope(ScopeKind::Block);

        expect!(self, TokenKind::LCurly);

        while !accept!(self, TokenKind::RCurly) {
            body.push(self.stmt_or_decl()?);
        }

        self.close_scope();

        return Ok(new_node!(self, Block { body: body }));
    }

    fn function_body(&mut self) -> Result<ASTRef, String> {
        let mut body: Vec<ASTRef> = vec![];

        expect!(self, TokenKind::LCurly);

        while !accept!(self, TokenKind::RCurly) {
            body.push(self.stmt_or_decl()?);
        }

        return Ok(new_node!(self, Block { body: body }));
    }

    fn if_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::If);
        expect!(self, TokenKind::LParen);
        let cond = self.expr(0)?;
        expect!(self, TokenKind::RParen);
        let then = self.statement()?;
        let otherwise = if accept!(self, TokenKind::Else) {
            Some(self.statement()?)
        } else {
            None
        };

        Ok(new_node!(
            self,
            If {
                cond: cond,
                then: then,
                otherwise: otherwise
            }
        ))
    }

    fn goto_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::GoTo);
        expect!(self, TokenKind::Identifier, _);
        let label = yank!(self, TokenKind::Identifier);
        expect!(self, TokenKind::Semicolon);

        Ok(new_node!(self, GoTo { label: label }))
    }

    fn return_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Return);
        let expr = self.expr(0)?;
        expect!(self, TokenKind::Semicolon);
        return Ok(new_node!(self, Return { expr: expr }));
    }

    fn expr_stmt(&mut self) -> Result<ASTRef, String> {
        let stmt = new_node!(
            self,
            ExprStmt {
                expr: self.expr(0)?
            }
        );
        expect!(self, TokenKind::Semicolon);
        return Ok(stmt);
    }

    fn conditional(
        &mut self,
        expr: ASTRef,
        min_prec: i32,
    ) -> Result<ASTRef, String> {
        let middle = self.expr(0)?;
        expect!(self, TokenKind::Colon);
        let right = self.expr(min_prec)?;

        Ok(new_node!(
            self,
            Conditional {
                left: expr.clone(),
                middle: middle,
                right: right,
            }
        ))
    }

    fn argument_list(&mut self) -> Result<Vec<ASTRef>, String> {
        let mut args: Vec<ASTRef> = vec![];

        if !accept!(self, TokenKind::RParen) {
            loop {
                args.push(self.expr(0)?);

                if accept!(self, TokenKind::RParen) {
                    break;
                } else {
                    expect!(self, TokenKind::Comma);
                }
            }
        }

        Ok(args)
    }

    fn call(&mut self, expr: ASTRef) -> Result<ASTRef, String> {
        let args = self.argument_list()?;

        Ok(new_node!(
            self,
            Call {
                expr: expr,
                args: args
            }
        ))
    }

    fn expr(&mut self, min_prec: i32) -> Result<ASTRef, String> {
        self.lexer.push_expr();
        let mut left = self.factor()?;
        let mut prec = precedence_of(&peek_any!(self));

        while self.peek_binop() && prec >= min_prec {
            if accept!(self, TokenKind::Assign) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.assignment(left, right)?;
            } else if accept!(self, TokenKind::PlusEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.plus_eq(left, right)?;
            } else if accept!(self, TokenKind::MinusEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.minus_eq(left, right)?;
            } else if accept!(self, TokenKind::MultEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.mult_eq(left, right)?;
            } else if accept!(self, TokenKind::DivEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.div_eq(left, right)?;
            } else if accept!(self, TokenKind::ModEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.mod_eq(left, right)?;
            } else if accept!(self, TokenKind::AndEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.and_eq(left, right)?;
            } else if accept!(self, TokenKind::OrEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.or_eq(left, right)?;
            } else if accept!(self, TokenKind::XorEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.xor_eq(left, right)?;
            } else if accept!(self, TokenKind::LShiftEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.lshift_eq(left, right)?;
            } else if accept!(self, TokenKind::RShiftEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.rshift_eq(left, right)?;
            } else if accept!(self, TokenKind::QMark) {
                left = self.conditional(left, prec)?;
            } else {
                left = self.binop(left, prec + 1)?;
                prec = precedence_of(&peek_any!(self));
            }
        }

        self.lexer.pop_expr();

        return Ok(left);
    }

    fn peek_binop(&self) -> bool {
        return match peek_any!(self) {
            TokenKind::Mult
            | TokenKind::Div
            | TokenKind::Mod
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::LShift
            | TokenKind::RShift
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Xor
            | TokenKind::LAnd
            | TokenKind::LOr
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::LThan
            | TokenKind::LThanEq
            | TokenKind::GThan
            | TokenKind::GThanEq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::MultEq
            | TokenKind::DivEq
            | TokenKind::ModEq
            | TokenKind::AndEq
            | TokenKind::OrEq
            | TokenKind::XorEq
            | TokenKind::LShiftEq
            | TokenKind::RShiftEq
            | TokenKind::Assign
            | TokenKind::QMark => true,
            _ => false,
        };
    }

    fn binop(&mut self, left: ASTRef, prec: i32) -> Result<ASTRef, String> {
        if accept!(self, TokenKind::Mult) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Multiply {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Div) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Divide {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Mod) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Modulo {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Plus) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Add {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Minus) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Subtract {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LShift) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LShift {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::RShift) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                RShift {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::And) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                And {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Or) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Or {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Xor) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Xor {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LAnd) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LogicAnd {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LOr) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LogicOr {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Eq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Equal {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::NotEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                NotEq {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LThan) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LessThan {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LThanEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LessOrEq {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::GThan) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                GreaterThan {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::GThanEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                GreaterOrEq {
                    left: left,
                    right: right
                }
            ));
        } else {
            return self.oops("malformed binary expression");
        }
    }

    fn assignment(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left,
                right: right
            }
        ));
    }

    fn plus_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Add {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn minus_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Subtract {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn mult_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Multiply {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn div_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Divide {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn mod_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Modulo {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn and_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    And {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn or_eq(&mut self, left: ASTRef, right: ASTRef) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Or {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn xor_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Xor {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn lshift_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    LShift {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn rshift_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    RShift {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn peek_postfix_op(&self) -> bool {
        match peek_any!(self) {
            TokenKind::Incr | TokenKind::Decr | TokenKind::LParen => true,
            _ => false,
        }
    }

    fn postfix(&mut self, mut expr: ASTRef) -> Result<ASTRef, String> {
        while self.peek_postfix_op() {
            if accept!(self, TokenKind::LParen) {
                expr = self.call(expr.clone())?;
            } else if accept!(self, TokenKind::Incr) {
                expr = new_node!(self, PostIncr { expr: expr.clone() });
            } else if accept!(self, TokenKind::Decr) {
                expr = new_node!(self, PostDecr { expr: expr.clone() });
            } else {
                return Err("invalid postfix expression".to_string());
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<ASTRef, String> {
        if peek!(self, TokenKind::ConstInt, _) {
            return self.const_int();
        } else if accept!(self, TokenKind::Tilde) {
            return Ok(new_node!(
                self,
                Complement {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Minus) {
            return Ok(new_node!(
                self,
                Negate {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Not) {
            return Ok(new_node!(
                self,
                Not {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Incr) {
            return Ok(new_node!(
                self,
                PreIncr {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Decr) {
            return Ok(new_node!(
                self,
                PreDecr {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::LParen) {
            let mut inner_expr = self.expr(0)?;
            expect!(self, TokenKind::RParen);

            if self.peek_postfix_op() {
                inner_expr = self.postfix(inner_expr.clone())?;
            }

            Ok(inner_expr)
        } else if accept!(self, TokenKind::Identifier, _) {
            let name = yank!(self, TokenKind::Identifier);
            let sym = find_sym(self.scope.clone(), &name, None);
            let mut expr = new_node!(
                self,
                Identifier {
                    name: name.clone(),
                    sym: if sym.is_some() {
                        Some(Rc::downgrade(&sym.unwrap()))
                    } else {
                        None
                    }
                }
            );

            if self.peek_postfix_op() {
                expr = self.postfix(expr.clone())?;
            }

            Ok(expr)
        } else {
            return self.oops("malformed expression");
        }
    }

    fn const_int(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::ConstInt, _);
        let value = yank!(self, TokenKind::ConstInt);
        return Ok(new_node!(self, ConstInt(value)));
    }

    fn oops(&self, why: &str) -> Result<ASTRef, String> {
        return Err(why.to_string());
    }

    fn eof(&self) -> bool {
        return matches!(self.next.as_ref().unwrap().kind, TokenKind::EOF);
    }

    fn advance(&mut self) {
        self.last = self.next.clone();
        self.next = self.lexer.lex();

        if self.next.as_ref().unwrap().kind == TokenKind::Bad {
            panic!("invalid token found");
        }
    }
}
