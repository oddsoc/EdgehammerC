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

use std::rc::Rc;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::scope;
use crate::types::*;
use scope::*;

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
                "expected {:?}, but got {:?}",
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
                "expected {:?}, but got {:?}",
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
        Ast::new($parser.next_node_id(), AstKind::$variant, None, $parser.scope.clone())
    };

    ($parser:expr, $variant:ident ( $($args:expr),* $(,)? )) => {
        Ast::new($parser.next_node_id(), AstKind::$variant( $($args),* ), None, $parser.scope.clone())
    };

    ($parser:expr, $variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Ast::new($parser.next_node_id(), AstKind::$variant { $($field: $value),*}, None, $parser.scope.clone())
    };
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    last: Option<Token>,
    next: Option<Token>,
    nr_nodes: usize,
    scope: ScopeRef,
    cases: Vec<Vec<AstRef>>,
    function: Option<SymRef>,
}

fn precedence_of(kind: &TokenKind) -> i32 {
    match kind {
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
    }
}

impl Parser {
    pub fn new(filename: &str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(filename),
            last: None,
            next: None,
            nr_nodes: 0,
            scope: crate::scope::new(),
            cases: vec![],
            function: None,
        };

        parser.advance();
        parser
    }

    fn next_node_id(&mut self) -> usize {
        let id = self.nr_nodes;
        self.nr_nodes += 1;
        id
    }

    fn open_scope(&mut self, kind: ScopeKind) {
        self.scope = scope::open(self.scope.clone(), kind);
    }

    fn close_scope(&mut self) {
        self.scope = scope::close(&self.scope);
    }

    pub fn parse(&mut self) -> Result<Vec<AstRef>, String> {
        self.program()
    }

    fn program(&mut self) -> Result<Vec<AstRef>, String> {
        let mut prog: Vec<AstRef> = vec![];

        while !self.eof() {
            prog.push(self.declaration()?);
        }

        Ok(prog)
    }

    fn declaration(&mut self) -> Result<AstRef, String> {
        let (ty_spec, storage_class) = self.decl_spec()?;
        expect!(self, TokenKind::Identifier, _);
        let name = yank!(self, TokenKind::Identifier);

        if peek!(self, TokenKind::LParen) {
            self.function(ty_spec, storage_class, name)
        } else {
            self.variable_decl(ty_spec, storage_class, name)
        }
    }

    fn is_storage_class(&mut self) -> bool {
        peek!(self, TokenKind::Static)
            || peek!(self, TokenKind::Extern)
            || peek!(self, TokenKind::Auto)
            || peek!(self, TokenKind::Register)
    }

    fn is_decl_spec(&mut self) -> bool {
        self.is_type_spec()
            || self.is_type_qualifier()
            || self.is_storage_class()
    }

    fn canonicalise_type_spec(
        &mut self,
        type_specs: &[Token],
    ) -> Result<AstRef, String> {
        let mut is_signed = false;
        let mut is_unsigned = false;
        let mut has_int = false;
        let mut long_count = 0;

        for spec in type_specs {
            match spec.kind {
                TokenKind::Signed => {
                    if is_unsigned || is_signed {
                        return Err("invalid type specifier".into());
                    }
                    is_signed = true;
                }
                TokenKind::Unsigned => {
                    if is_signed || is_unsigned {
                        return Err("invalid type specifier".into());
                    }
                    is_unsigned = true;
                }
                TokenKind::Int => {
                    if has_int {
                        return Err("invalid type specifier".into());
                    }
                    has_int = true;
                }
                TokenKind::Long => {
                    if long_count >= 2 {
                        return Err("invalid type specifier".into());
                    }
                    long_count += 1;
                }
                TokenKind::Void => {
                    if type_specs.len() > 1 {
                        return Err("invalid type specifier".into());
                    }
                    return Ok(new_node!(self, Void));
                }
                _ => unreachable!("unexpected token kind"),
            }
        }

        if !is_unsigned {
            is_signed = true;
        }

        let ty = match long_count {
            0 => int_type(is_signed),
            1 => long_type(is_signed),
            2 => long_long_type(is_signed),
            _ => unreachable!(),
        };

        let node = new_node!(self, Int);
        node.borrow_mut().ty = ty;

        Ok(node)
    }

    fn decl_spec(&mut self) -> Result<(AstRef, Option<StorageClass>), String> {
        let mut type_specs: Vec<Token> = vec![];
        let mut storage_classes: Vec<StorageClass> = vec![];

        loop {
            if self.is_type_spec() {
                type_specs.push(self.type_spec()?);
            } else if self.is_type_qualifier() {
                todo!();
            } else if self.is_storage_class() {
                storage_classes.push(self.storage_class()?);
            } else {
                break;
            }
        }

        if type_specs.len() < 1 {
            return Err("invalid type specifier".to_string());
        }

        let ty_spec = self.canonicalise_type_spec(&type_specs)?;

        if storage_classes.len() > 1 {
            return Err("invalid storage class".to_string());
        }

        if storage_classes.len() == 1 {
            Ok((ty_spec, Some(storage_classes[0])))
        } else {
            Ok((ty_spec, None))
        }
    }

    fn parameter_list(&mut self) -> Result<Vec<AstRef>, String> {
        let mut params: Vec<AstRef> = vec![];
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

    fn parameter(&mut self, idx: usize) -> Result<AstRef, String> {
        let (ty_spec, storage_class) = self.decl_spec()?;
        let ty = type_of(&ty_spec);

        if storage_class.is_some() {
            if let Some(StorageClass::Register) = storage_class {
            } else {
                return Err("invalid storage class".to_string());
            }
        }

        if peek!(self, TokenKind::RParen) {
            Ok(new_node!(
                self,
                Parameter {
                    name: None,
                    sym: None,
                    idx: idx,
                    type_spec: ty_spec
                }
            ))
        } else {
            expect!(self, TokenKind::Identifier, _);
            let name = yank!(self, TokenKind::Identifier);

            let sym = add_sym(
                self.scope.clone(),
                &name,
                SymKind::Parameter,
                ty.borrow().size,
                ty.borrow().alignment,
                None,
                Some(Definition::Concrete),
                None,
            )?;

            let param = new_node!(
                self,
                Parameter {
                    name: Some(name.clone()),
                    sym: Some(Rc::downgrade(&sym)),
                    idx: idx,
                    type_spec: ty_spec,
                }
            );

            sym.borrow_mut().node = Some(Rc::downgrade(&param));

            Ok(param)
        }
    }

    fn function(
        &mut self,
        ty_spec: AstRef,
        storage_class: Option<StorageClass>,
        name: String,
    ) -> Result<AstRef, String> {
        self.open_scope(ScopeKind::Function);
        let params = self.parameter_list()?;

        if accept!(self, TokenKind::Semicolon) {
            self.close_scope();

            let sym = add_sym(
                self.scope.clone(),
                &name,
                SymKind::Function,
                1,
                16,
                storage_class,
                None,
                None,
            )?;

            let signature = new_node!(
                self,
                Function {
                    name: name.clone(),
                    sym: Some(Rc::downgrade(&sym)),
                    params: params.clone(),
                    block: None,
                    type_spec: ty_spec.clone(),
                    scope: self.scope.clone(),
                }
            );

            sym.borrow_mut().node = Some(Rc::downgrade(&signature));

            Ok(signature)
        } else {
            let sym = add_sym(
                parent_of(&self.scope),
                &name,
                SymKind::Function,
                1,
                16,
                storage_class,
                Some(Definition::Concrete),
                None,
            )?;

            self.function = Some(sym.clone());

            let body = self.function_body()?;
            self.close_scope();

            if kind_of(&self.scope) != ScopeKind::File {
                return Err(
                    "function definition is not allowed here".to_string()
                );
            }

            let function = new_node!(
                self,
                Function {
                    name: name.clone(),
                    sym: Some(Rc::downgrade(&sym)),
                    params: params.clone(),
                    block: Some(body),
                    type_spec: ty_spec.clone(),
                    scope: self.scope.clone(),
                }
            );

            sym.borrow_mut().node = Some(Rc::downgrade(&function));

            self.function = None;

            Ok(function)
        }
    }

    fn determine_definition_type(
        &self,
        storage_class: Option<StorageClass>,
    ) -> Option<Definition> {
        if peek!(self, TokenKind::Assign) {
            Some(Definition::Concrete)
        } else if let Some(StorageClass::Static) = storage_class {
            Some(Definition::Tentative)
        } else if let Some(StorageClass::Extern) = storage_class {
            None
        } else if !scope::has_parent(&self.scope) {
            Some(Definition::Tentative)
        } else {
            None
        }
    }

    fn initializer(
        &mut self,
        storage_class: &Option<StorageClass>,
    ) -> Result<AstRef, String> {
        let mut is_static = if let Some(class) = storage_class {
            if matches!(class, StorageClass::Static) {
                true
            } else {
                false
            }
        } else {
            false
        };

        if !is_static && kind_of(&self.scope) == ScopeKind::File {
            is_static = true;
        }

        if is_static {
            Ok(new_node!(self, StaticInitializer(self.expr(0)?)))
        } else {
            Ok(new_node!(self, Initializer(self.expr(0)?)))
        }
    }

    fn variable_decl(
        &mut self,
        ty_spec: AstRef,
        storage_class: Option<StorageClass>,
        name: String,
    ) -> Result<AstRef, String> {
        let definition = self.determine_definition_type(storage_class);

        let ty = type_of(&ty_spec);

        let sym = add_sym(
            self.scope.clone(),
            &name,
            SymKind::Variable,
            ty.borrow().size,
            ty.borrow().alignment,
            storage_class,
            definition,
            None,
        )?;

        let init = if accept!(self, TokenKind::Assign) {
            Some(self.initializer(&storage_class)?)
        } else {
            None
        };

        expect!(self, TokenKind::Semicolon);

        let var = new_node!(
            self,
            Variable {
                name: name.clone(),
                sym: Some(Rc::downgrade(&sym)),
                type_spec: ty_spec.clone(),
                init: init,
            }
        );

        sym.borrow_mut().node = Some(Rc::downgrade(&var));

        Ok(var)
    }

    fn is_type_spec(&self) -> bool {
        match peek_any!(self) {
            TokenKind::Int | TokenKind::Long | TokenKind::Void => true,
            _ => false,
        }
    }

    fn is_type_qualifier(&self) -> bool {
        false
    }

    fn type_spec(&mut self) -> Result<Token, String> {
        if accept!(self, TokenKind::Int) {
            Ok(self.last.clone().unwrap())
        } else if accept!(self, TokenKind::Long) {
            Ok(self.last.clone().unwrap())
        } else if accept!(self, TokenKind::Signed) {
            Ok(self.last.clone().unwrap())
        } else if accept!(self, TokenKind::Unsigned) {
            Ok(self.last.clone().unwrap())
        } else if accept!(self, TokenKind::Void) {
            Ok(self.last.clone().unwrap())
        } else {
            Err("invalid type specifier".to_string())
        }
    }

    fn storage_class(&mut self) -> Result<StorageClass, String> {
        if accept!(self, TokenKind::Static) {
            Ok(StorageClass::Static)
        } else if accept!(self, TokenKind::Extern) {
            Ok(StorageClass::Extern)
        } else if accept!(self, TokenKind::Auto) {
            Ok(StorageClass::Auto)
        } else if accept!(self, TokenKind::Register) {
            Ok(StorageClass::Register)
        } else {
            Err("unknown storage class specifier".to_string())
        }
    }

    fn statement(&mut self) -> Result<AstRef, String> {
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

    fn while_stmt(&mut self) -> Result<AstRef, String> {
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

    fn do_while_stmt(&mut self) -> Result<AstRef, String> {
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

    fn for_init(&mut self) -> Result<AstRef, String> {
        if self.is_decl_spec() {
            let (ty_spec, storage_class) = self.decl_spec()?;
            expect!(self, TokenKind::Identifier, _);
            let name = yank!(self, TokenKind::Identifier);

            if storage_class.is_some() {
                return Err(format!(
                    "loop initial declaration of '{}' cannot have a storage class",
                    name
                ));
            }

            Ok(self.variable_decl(ty_spec, storage_class, name)?)
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

    fn for_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, TokenKind::For);
        expect!(self, TokenKind::LParen);

        self.open_scope(ScopeKind::Loop);

        let init: Option<AstRef> = if accept!(self, TokenKind::Semicolon) {
            None
        } else {
            Some(self.for_init()?)
        };

        let cond: Option<AstRef> = if accept!(self, TokenKind::Semicolon) {
            None
        } else {
            let c = Some(self.expr(0)?);
            expect!(self, TokenKind::Semicolon);
            c
        };

        let post: Option<AstRef> = if peek!(self, TokenKind::RParen) {
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

    fn switch_stmt(&mut self) -> Result<AstRef, String> {
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

    fn break_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, TokenKind::Break);
        expect!(self, TokenKind::Semicolon);

        Ok(new_node!(self, Break { to: None }))
    }

    fn continue_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, TokenKind::Continue);
        expect!(self, TokenKind::Semicolon);

        Ok(new_node!(self, Continue { to: None }))
    }

    fn labelled_stmt(&mut self) -> Result<AstRef, String> {
        if accept!(self, TokenKind::Label, _) {
            let label = yank!(self, TokenKind::Label);
            let stmt = self.statement()?;
            let l = add_label(self.scope.clone(), &label, stmt.clone());

            if l.is_err() {
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

    fn stmt_or_decl(&mut self) -> Result<AstRef, String> {
        if self.is_decl_spec() {
            self.declaration()
        } else {
            self.statement()
        }
    }

    fn block(&mut self) -> Result<AstRef, String> {
        let mut body: Vec<AstRef> = vec![];
        self.open_scope(ScopeKind::Block);

        expect!(self, TokenKind::LCurly);

        while !accept!(self, TokenKind::RCurly) {
            body.push(self.stmt_or_decl()?);
        }

        self.close_scope();

        Ok(new_node!(self, Block { body: body }))
    }

    fn function_body(&mut self) -> Result<AstRef, String> {
        let mut body: Vec<AstRef> = vec![];

        expect!(self, TokenKind::LCurly);

        while !accept!(self, TokenKind::RCurly) {
            body.push(self.stmt_or_decl()?);
        }

        Ok(new_node!(self, Block { body: body }))
    }

    fn if_stmt(&mut self) -> Result<AstRef, String> {
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

    fn goto_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, TokenKind::GoTo);
        expect!(self, TokenKind::Identifier, _);
        let label = yank!(self, TokenKind::Identifier);
        expect!(self, TokenKind::Semicolon);

        Ok(new_node!(self, GoTo { label: label }))
    }

    fn return_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, TokenKind::Return);
        let expr = self.expr(0)?;
        expect!(self, TokenKind::Semicolon);
        Ok(new_node!(
            self,
            Return {
                expr: expr,
                func: Rc::downgrade(&self.function.as_ref().unwrap()),
            }
        ))
    }

    fn expr_stmt(&mut self) -> Result<AstRef, String> {
        let stmt = new_node!(
            self,
            ExprStmt {
                expr: self.expr(0)?
            }
        );
        expect!(self, TokenKind::Semicolon);
        Ok(stmt)
    }

    fn conditional(
        &mut self,
        expr: AstRef,
        min_prec: i32,
    ) -> Result<AstRef, String> {
        let middle = self.expr(0)?;
        expect!(self, TokenKind::Colon);
        let right = self.expr(min_prec)?;

        Ok(new_node!(
            self,
            Ternary {
                left: expr.clone(),
                middle: middle,
                right: right,
            }
        ))
    }

    fn argument_list(&mut self) -> Result<Vec<AstRef>, String> {
        let mut args: Vec<AstRef> = vec![];

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

    fn call(&mut self, expr: AstRef) -> Result<AstRef, String> {
        let args = self.argument_list()?;

        Ok(new_node!(
            self,
            Call {
                expr: expr,
                args: args
            }
        ))
    }

    fn expr(&mut self, min_prec: i32) -> Result<AstRef, String> {
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

        Ok(left)
    }

    fn peek_binop(&self) -> bool {
        match peek_any!(self) {
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
        }
    }

    fn binop(&mut self, left: AstRef, prec: i32) -> Result<AstRef, String> {
        if accept!(self, TokenKind::Mult) {
            let right = self.expr(prec + 1)?;
            Ok(new_node!(
                self,
                Multiply {
                    left: left,
                    right: right
                }
            ))
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
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left,
                right: right
            }
        ))
    }

    fn pre_incr(&mut self, expr: AstRef) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: expr.clone(),
                right: new_node!(
                    self,
                    Add {
                        left: deep_clone(&expr),
                        right: new_node!(self, ConstInt(1))
                    }
                ),
            }
        ))
    }

    fn pre_decr(&mut self, expr: AstRef) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: expr.clone(),
                right: new_node!(
                    self,
                    Subtract {
                        left: deep_clone(&expr),
                        right: new_node!(self, ConstInt(1))
                    }
                ),
            }
        ))
    }

    fn plus_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Add {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn minus_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Subtract {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn mult_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Multiply {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn div_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Divide {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn mod_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Modulo {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn and_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    And {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn or_eq(&mut self, left: AstRef, right: AstRef) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Or {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn xor_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Xor {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn lshift_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    LShift {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn rshift_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    RShift {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn peek_postfix_op(&self) -> bool {
        match peek_any!(self) {
            TokenKind::Incr | TokenKind::Decr | TokenKind::LParen => true,
            _ => false,
        }
    }

    fn postfix(&mut self, mut expr: AstRef) -> Result<AstRef, String> {
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

    fn factor(&mut self) -> Result<AstRef, String> {
        if peek!(self, TokenKind::ConstInt, _) {
            self.const_int()
        } else if peek!(self, TokenKind::ConstLong, _) {
            self.const_long()
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
            let subexpr = self.factor()?;
            return self.pre_incr(subexpr);
        } else if accept!(self, TokenKind::Decr) {
            let subexpr = self.factor()?;
            return self.pre_decr(subexpr);
        } else if accept!(self, TokenKind::LParen) {
            if self.is_type_spec() || self.is_type_qualifier() {
                return self.cast_expr();
            }

            let mut inner_expr = self.expr(0)?;
            expect!(self, TokenKind::RParen);

            if self.peek_postfix_op() {
                inner_expr = self.postfix(inner_expr.clone())?;
            }

            Ok(inner_expr)
        } else if accept!(self, TokenKind::Identifier, _) {
            let name = yank!(self, TokenKind::Identifier);
            let sym = get_sym(self.scope.clone(), &name);

            let mut expr = new_node!(
                self,
                Identifier {
                    name: name.clone(),
                    sym: if let Some(s) = sym {
                        Some(Rc::downgrade(&s.clone()))
                    } else {
                        return Err(format!("'{}' undeclared", name));
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

    fn cast_expr(&mut self) -> Result<AstRef, String> {
        let (ty_spec, storage_class) = self.decl_spec()?;

        if storage_class.is_some() {
            return Err("a cast cannot have a storage class".into());
        }

        expect!(self, TokenKind::RParen);

        Ok(new_node!(
            self,
            Cast {
                type_spec: Some(ty_spec),
                expr: self.factor()?,
            }
        ))
    }

    fn const_int(&mut self) -> Result<AstRef, String> {
        expect!(self, TokenKind::ConstInt, _);
        let value = yank!(self, TokenKind::ConstInt);
        let node = new_node!(self, ConstInt(value as i32));
        node.borrow_mut().ty = int_type(true);

        Ok(node)
    }

    fn const_long(&mut self) -> Result<AstRef, String> {
        expect!(self, TokenKind::ConstLong, _);
        let value = yank!(self, TokenKind::ConstLong);
        let node = new_node!(self, ConstLong(value));
        node.borrow_mut().ty = long_type(true);

        Ok(node)
    }

    fn oops(&self, why: &str) -> Result<AstRef, String> {
        Err(why.to_string())
    }

    fn eof(&self) -> bool {
        matches!(self.next.as_ref().unwrap().kind, TokenKind::EOF)
    }

    fn advance(&mut self) {
        self.last = self.next.clone();
        self.next = self.lexer.lex();

        if self.next.as_ref().unwrap().kind == TokenKind::Bad {
            panic!("invalid token found");
        }
    }
}
