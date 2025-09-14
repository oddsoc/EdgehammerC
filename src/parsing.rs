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
use crate::lexing::{Token, TokenTag as Tag, Tokeniser};
use crate::scope;
use crate::types::*;
use scope::*;

macro_rules! accept {
    ($parser:expr, $tag:path) => {
        if let Some(token) = &$parser.tokens[1] {
            if matches!(token.tag, $tag) {
                $parser.advance()?;
                true
            } else {
                false
            }
        } else {
            false
        }
    };

    ($parser:expr, $tag:path, $_:tt) => {
        if let Some(token) = &$parser.tokens[1] {
            if matches!(token.tag, $tag(_)) {
                $parser.advance()?;
                true
            } else {
                false
            }
        } else {
            false
        }
    };
}

macro_rules! expect {
    ($parser:expr, $tag:path) => {
        if let Some(token) = &$parser.tokens[1] {
            if !matches!(token.tag, $tag) {
                let msg = format!(
                    "expected {:?} but got {:?}",
                    stringify!($tag),
                    token.tag
                );
                return Err(msg);
            } else {
                $parser.advance()?;
            }
        } else {
            let msg = format!(
                "expected {:?} but reached end of file",
                stringify!($tag)
            );
            return Err(msg);
        }
    };

    ($parser:expr, $tag:path, $_:tt) => {
        if let Some(token) = &$parser.tokens[1] {
            if !matches!(token.tag, $tag(_)) {
                let msg = format!(
                    "expected {:?} but got {:?}",
                    stringify!($tag),
                    token.tag
                );
                return Err(msg);
            } else {
                $parser.advance()?;
            }
        } else {
            let msg = format!(
                "expected {:?} but reached end of file",
                stringify!($tag)
            );
            return Err(msg);
        }
    };
}

macro_rules! peek {
    ($parser:expr, $tag:path) => {
        if let Some(token) = &$parser.tokens[1] {
            if matches!(token.tag, $tag) {
                true
            } else {
                false
            }
        } else {
            false
        }
    };

    ($parser:expr, $tag:path, $_:tt) => {
        if let Some(token) = &$parser.tokens[1] {
            if matches!(token.tag, $tag(_)) {
                true
            } else {
                false
            }
        } else {
            false
        }
    };
}

macro_rules! peek2 {
    ($parser:expr, $tag:path) => {
        if let Some(token) = &$parser.tokens[2] {
            if matches!(token.tag, $tag) {
                true
            } else {
                false
            }
        } else {
            false
        }
    };

    ($parser:expr, $tag:path, $_:tt) => {
        if let Some(token) = $parser.tokens[2] {
            if matches!(token.tag, $tag(_)) {
                true
            } else {
                false
            }
        } else {
            false
        }
    };
}

macro_rules! peek_tag {
    ($parser:expr) => {
        if let Some(token) = &$parser.tokens[1] {
            token.tag.clone()
        } else {
            Tag::End
        }
    };
}

macro_rules! yank {
    ($parser:expr, $tag:path) => {
        if let Some(token) = &$parser.tokens[0] {
            match &token.tag {
                $tag(data) => data.clone(),
                _ => panic!(
                    "expected {:?} but got {:?}",
                    stringify!($tag),
                    token.tag
                ),
            }
        } else {
            panic!("parser bug");
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
pub struct Parser<'buf> {
    buf: &'buf str,
    tokeniser: Tokeniser<'buf>,
    tokens: [Option<Token>; 3],
    nr_nodes: usize,
    scope: ScopeRef,
    cases: Vec<Vec<AstRef>>,
    function: Option<SymRef>,
}

fn precedence_of(tag: &Tag) -> i32 {
    match tag {
        Tag::Asterisk | Tag::ForwardSlash | Tag::Percent => 50,
        Tag::Plus | Tag::Minus => 45,
        Tag::LeftShift | Tag::RightShift => 40,
        Tag::Less | Tag::LessOrEq | Tag::Greater | Tag::GreaterOrEq => 35,
        Tag::Eq | Tag::NotEq => 30,
        Tag::Ampersand => 25,
        Tag::Caret => 20,
        Tag::Bar => 15,
        Tag::LAnd => 10,
        Tag::LOr => 5,
        Tag::Question => 3,
        Tag::Assign
        | Tag::PlusEq
        | Tag::MinusEq
        | Tag::MultEq
        | Tag::DivideEq
        | Tag::ModEq
        | Tag::OrEq
        | Tag::AndEq
        | Tag::InvEq => 1,
        _ => 0,
    }
}

impl<'buf> Parser<'buf> {
    pub fn new(buf: &'buf str) -> Result<Self, String> {
        let mut parser = Self {
            buf: buf,
            tokeniser: Tokeniser::new(buf),
            tokens: [None, None, None],
            nr_nodes: 0,
            scope: crate::scope::new(),
            cases: vec![],
            function: None,
        };

        parser.preload()?;

        Ok(parser)
    }

    fn next_node_id(&mut self) -> usize {
        let id = self.nr_nodes;
        self.nr_nodes += 1;
        id
    }

    fn open_scope(&mut self, tag: ScopeKind) {
        self.scope = scope::open(self.scope.clone(), tag);
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

    fn yank(&mut self) -> String {
        self.tokens[0].as_ref().unwrap().to_string(self.buf)
    }

    fn declaration(&mut self) -> Result<AstRef, String> {
        let (ty_spec, storage_class) = self.decl_spec()?;
        expect!(self, Tag::Identifier);
        let name = self.yank();

        if peek!(self, Tag::LeftParen) {
            self.function(ty_spec, storage_class, name)
        } else {
            self.variable_decl(ty_spec, storage_class, name)
        }
    }

    fn is_storage_class(&mut self) -> bool {
        peek!(self, Tag::Static)
            || peek!(self, Tag::Extern)
            || peek!(self, Tag::Auto)
            || peek!(self, Tag::Register)
    }

    fn is_decl_spec(&mut self) -> bool {
        self.is_type_spec()
            || self.is_type_qualifier()
            || self.is_storage_class()
    }

    fn normalise_type_spec(
        &mut self,
        type_specs: &[String],
    ) -> Result<AstRef, String> {
        let mut is_signed = false;
        let mut is_unsigned = false;
        let mut has_int = false;
        let mut long_count = 0;
        let mut has_double = false;

        for spec in type_specs {
            match spec.as_str() {
                "signed" => {
                    if is_unsigned || is_signed || has_double {
                        return Err("invalid type specifier".into());
                    }
                    is_signed = true;
                }
                "unsigned" => {
                    if is_signed || is_unsigned || has_double {
                        return Err("invalid type specifier".into());
                    }
                    is_unsigned = true;
                }
                "int" => {
                    if has_int || has_double {
                        return Err("invalid type specifier".into());
                    }
                    has_int = true;
                }
                "long" => {
                    if long_count >= 2 || has_double {
                        return Err("invalid type specifier".into());
                    }
                    long_count += 1;
                }
                "double" => {
                    if has_double {
                        return Err("invalid type specifier".into());
                    }
                    has_double = true;
                }
                "void" => {
                    if type_specs.len() > 1 {
                        return Err("invalid type specifier".into());
                    }
                    return Ok(new_node!(self, Void));
                }
                _ => unreachable!("unexpected token tag"),
            }
        }

        if has_double {
            if is_signed || is_unsigned || has_int {
                return Err("invalid type specifier".into());
            }

            let ty = if long_count == 1 {
                long_double_type()
            } else if long_count == 0 {
                double_type()
            } else {
                return Err("invalid type specifier".into());
            };

            let node = new_node!(self, Double);
            node.borrow_mut().ty = ty;
            return Ok(node);
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
        let mut type_specs: Vec<String> = vec![];
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

        let ty_spec = self.normalise_type_spec(&type_specs)?;

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
        expect!(self, Tag::LeftParen);

        if !accept!(self, Tag::RightParen) {
            loop {
                params.push(self.parameter(idx)?);

                if accept!(self, Tag::RightParen) {
                    break;
                } else {
                    expect!(self, Tag::Comma);
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

        if peek!(self, Tag::RightParen) {
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
            expect!(self, Tag::Identifier);
            let name = self.yank();

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

        if accept!(self, Tag::Semicolon) {
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
        if peek!(self, Tag::Assign) {
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

        let init = if accept!(self, Tag::Assign) {
            Some(self.initializer(&storage_class)?)
        } else {
            None
        };

        expect!(self, Tag::Semicolon);

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
        match peek_tag!(self) {
            Tag::Unsigned
            | Tag::Signed
            | Tag::Int
            | Tag::Long
            | Tag::Double
            | Tag::Void => true,
            _ => false,
        }
    }

    fn is_type_qualifier(&self) -> bool {
        false
    }

    fn type_spec(&mut self) -> Result<String, String> {
        if accept!(self, Tag::Int) {
            Ok("int".to_string())
        } else if accept!(self, Tag::Long) {
            Ok("long".to_string())
        } else if accept!(self, Tag::Signed) {
            Ok("signed".to_string())
        } else if accept!(self, Tag::Unsigned) {
            Ok("unsigned".to_string())
        } else if accept!(self, Tag::Double) {
            Ok("double".to_string())
        } else if accept!(self, Tag::Void) {
            Ok("void".to_string())
        } else {
            Err("invalid type specifier".to_string())
        }
    }

    fn storage_class(&mut self) -> Result<StorageClass, String> {
        if accept!(self, Tag::Static) {
            Ok(StorageClass::Static)
        } else if accept!(self, Tag::Extern) {
            Ok(StorageClass::Extern)
        } else if accept!(self, Tag::Auto) {
            Ok(StorageClass::Auto)
        } else if accept!(self, Tag::Register) {
            Ok(StorageClass::Register)
        } else {
            Err("unknown storage class specifier".to_string())
        }
    }

    fn statement(&mut self) -> Result<AstRef, String> {
        if peek!(self, Tag::LeftBrace) {
            self.block()
        } else if (peek!(self, Tag::Identifier) && peek2!(self, Tag::Colon))
            || peek!(self, Tag::Case)
            || peek!(self, Tag::Default)
        {
            self.labelled_stmt()
        } else if peek!(self, Tag::Return) {
            self.return_stmt()
        } else if peek!(self, Tag::If) {
            self.if_stmt()
        } else if peek!(self, Tag::While) {
            self.while_stmt()
        } else if peek!(self, Tag::Do) {
            self.do_while_stmt()
        } else if peek!(self, Tag::For) {
            self.for_stmt()
        } else if peek!(self, Tag::Switch) {
            self.switch_stmt()
        } else if peek!(self, Tag::GoTo) {
            self.goto_stmt()
        } else if peek!(self, Tag::Break) {
            self.break_stmt()
        } else if peek!(self, Tag::Continue) {
            self.continue_stmt()
        } else if accept!(self, Tag::Semicolon) {
            Ok(new_node!(self, EmptyStmt))
        } else {
            self.expr_stmt()
        }
    }

    fn while_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::While);
        expect!(self, Tag::LeftParen);
        let cond = self.expr(0)?;
        expect!(self, Tag::RightParen);
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
        expect!(self, Tag::Do);
        self.open_scope(ScopeKind::Loop);
        let body = self.statement()?;
        self.close_scope();
        expect!(self, Tag::While);
        expect!(self, Tag::LeftParen);
        let cond = self.expr(0)?;
        expect!(self, Tag::RightParen);
        expect!(self, Tag::Semicolon);

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
            expect!(self, Tag::Identifier);
            let name = self.yank();

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
            expect!(self, Tag::Semicolon);
            Ok(init)
        }
    }

    fn for_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::For);
        expect!(self, Tag::LeftParen);

        self.open_scope(ScopeKind::Loop);

        let init: Option<AstRef> = if accept!(self, Tag::Semicolon) {
            None
        } else {
            Some(self.for_init()?)
        };

        let cond: Option<AstRef> = if accept!(self, Tag::Semicolon) {
            None
        } else {
            let c = Some(self.expr(0)?);
            expect!(self, Tag::Semicolon);
            c
        };

        let post: Option<AstRef> = if peek!(self, Tag::RightParen) {
            None
        } else {
            Some(new_node!(
                self,
                ExprStmt {
                    expr: self.expr(0)?
                }
            ))
        };

        expect!(self, Tag::RightParen);

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
        expect!(self, Tag::Switch);
        expect!(self, Tag::LeftParen);
        let expr = self.expr(0)?;
        expect!(self, Tag::RightParen);

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
        expect!(self, Tag::Break);
        expect!(self, Tag::Semicolon);

        Ok(new_node!(self, Break { to: None }))
    }

    fn continue_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::Continue);
        expect!(self, Tag::Semicolon);

        Ok(new_node!(self, Continue { to: None }))
    }

    fn labelled_stmt(&mut self) -> Result<AstRef, String> {
        if accept!(self, Tag::Identifier) {
            let label = self.yank();
            expect!(self, Tag::Colon);
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
        } else if accept!(self, Tag::Case) {
            let expr = self.expr(0)?;
            expect!(self, Tag::Colon);
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
            expect!(self, Tag::Default);
            expect!(self, Tag::Colon);
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

        expect!(self, Tag::LeftBrace);

        while !accept!(self, Tag::RightBrace) {
            body.push(self.stmt_or_decl()?);
        }

        self.close_scope();

        Ok(new_node!(self, Block { body: body }))
    }

    fn function_body(&mut self) -> Result<AstRef, String> {
        let mut body: Vec<AstRef> = vec![];

        expect!(self, Tag::LeftBrace);

        while !accept!(self, Tag::RightBrace) {
            body.push(self.stmt_or_decl()?);
        }

        Ok(new_node!(self, Block { body: body }))
    }

    fn if_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::If);
        expect!(self, Tag::LeftParen);
        let cond = self.expr(0)?;
        expect!(self, Tag::RightParen);
        let then = self.statement()?;
        let otherwise = if accept!(self, Tag::Else) {
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
        expect!(self, Tag::GoTo);
        expect!(self, Tag::Identifier);
        let label = self.yank();
        expect!(self, Tag::Semicolon);

        Ok(new_node!(self, GoTo { label: label }))
    }

    fn return_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::Return);
        let expr = self.expr(0)?;
        expect!(self, Tag::Semicolon);
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
        expect!(self, Tag::Semicolon);
        Ok(stmt)
    }

    fn conditional(
        &mut self,
        expr: AstRef,
        min_prec: i32,
    ) -> Result<AstRef, String> {
        let middle = self.expr(0)?;
        expect!(self, Tag::Colon);
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

        if !accept!(self, Tag::RightParen) {
            loop {
                args.push(self.expr(0)?);

                if accept!(self, Tag::RightParen) {
                    break;
                } else {
                    expect!(self, Tag::Comma);
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
        let mut left = self.factor()?;
        let mut prec = precedence_of(&peek_tag!(self));

        while self.peek_binop() && prec >= min_prec {
            if accept!(self, Tag::Assign) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.assignment(left, right)?;
            } else if accept!(self, Tag::PlusEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.plus_eq(left, right)?;
            } else if accept!(self, Tag::MinusEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.minus_eq(left, right)?;
            } else if accept!(self, Tag::MultEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.mult_eq(left, right)?;
            } else if accept!(self, Tag::DivideEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.div_eq(left, right)?;
            } else if accept!(self, Tag::ModEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.mod_eq(left, right)?;
            } else if accept!(self, Tag::AndEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.and_eq(left, right)?;
            } else if accept!(self, Tag::OrEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.or_eq(left, right)?;
            } else if accept!(self, Tag::XorEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.xor_eq(left, right)?;
            } else if accept!(self, Tag::LeftShiftEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.lshift_eq(left, right)?;
            } else if accept!(self, Tag::RightShiftEq) {
                let right = self.expr(precedence_of(&peek_tag!(self)))?;
                left = self.rshift_eq(left, right)?;
            } else if accept!(self, Tag::Question) {
                left = self.conditional(left, prec)?;
            } else {
                left = self.binop(left, prec + 1)?;
                prec = precedence_of(&peek_tag!(self));
            }
        }

        Ok(left)
    }

    fn peek_binop(&self) -> bool {
        match peek_tag!(self) {
            Tag::Asterisk
            | Tag::ForwardSlash
            | Tag::Percent
            | Tag::Plus
            | Tag::Minus
            | Tag::LeftShift
            | Tag::RightShift
            | Tag::Ampersand
            | Tag::Bar
            | Tag::Caret
            | Tag::LAnd
            | Tag::LOr
            | Tag::Eq
            | Tag::NotEq
            | Tag::Less
            | Tag::LessOrEq
            | Tag::Greater
            | Tag::GreaterOrEq
            | Tag::PlusEq
            | Tag::MinusEq
            | Tag::MultEq
            | Tag::DivideEq
            | Tag::ModEq
            | Tag::AndEq
            | Tag::OrEq
            | Tag::XorEq
            | Tag::LeftShiftEq
            | Tag::RightShiftEq
            | Tag::Assign
            | Tag::Question => true,
            _ => false,
        }
    }

    fn binop(&mut self, left: AstRef, prec: i32) -> Result<AstRef, String> {
        if accept!(self, Tag::Asterisk) {
            let right = self.expr(prec + 1)?;
            Ok(new_node!(
                self,
                Multiply {
                    left: left,
                    right: right
                }
            ))
        } else if accept!(self, Tag::ForwardSlash) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Divide {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Percent) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Modulo {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Plus) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Add {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Minus) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Subtract {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::LeftShift) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LeftShift {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::RightShift) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                RightShift {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Ampersand) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                And {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Bar) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Or {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Caret) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Xor {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::LAnd) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LogicAnd {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::LOr) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LogicOr {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Eq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Equal {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::NotEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                NotEq {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Less) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Less {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::LessOrEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LessOrEq {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::Greater) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Greater {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, Tag::GreaterOrEq) {
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
                    LeftShift {
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
                    RightShift {
                        left: deep_clone(&left),
                        right: right.clone()
                    }
                ),
            }
        ))
    }

    fn peek_postfix_op(&self) -> bool {
        match peek_tag!(self) {
            Tag::Incr | Tag::Decr | Tag::LeftParen => true,
            _ => false,
        }
    }

    fn postfix(&mut self, mut expr: AstRef) -> Result<AstRef, String> {
        while self.peek_postfix_op() {
            if accept!(self, Tag::LeftParen) {
                expr = self.call(expr.clone())?;
            } else if accept!(self, Tag::Incr) {
                expr = new_node!(self, PostIncr { expr: expr.clone() });
            } else if accept!(self, Tag::Decr) {
                expr = new_node!(self, PostDecr { expr: expr.clone() });
            } else {
                return Err("invalid postfix expression".to_string());
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<AstRef, String> {
        if peek!(self, Tag::ConstInt, _) {
            self.const_int()
        } else if peek!(self, Tag::ConstUnsignedInt, _) {
            self.const_unsigned_int()
        } else if peek!(self, Tag::ConstLong, _) {
            self.const_long()
        } else if peek!(self, Tag::ConstLongLong, _) {
            self.const_long_long()
        } else if peek!(self, Tag::ConstUnsignedLong, _) {
            self.const_unsigned_long()
        } else if peek!(self, Tag::ConstUnsignedLongLong, _) {
            self.const_unsigned_long_long()
        } else if peek!(self, Tag::ConstDouble, _) {
            self.const_double()
        } else if accept!(self, Tag::Tilde) {
            return Ok(new_node!(
                self,
                Complement {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, Tag::Minus) {
            return Ok(new_node!(
                self,
                Negate {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, Tag::Bang) {
            return Ok(new_node!(
                self,
                Not {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, Tag::Incr) {
            let subexpr = self.factor()?;
            return self.pre_incr(subexpr);
        } else if accept!(self, Tag::Decr) {
            let subexpr = self.factor()?;
            return self.pre_decr(subexpr);
        } else if accept!(self, Tag::LeftParen) {
            if self.is_type_spec() || self.is_type_qualifier() {
                return self.cast_expr();
            }

            let mut inner_expr = self.expr(0)?;
            expect!(self, Tag::RightParen);

            if self.peek_postfix_op() {
                inner_expr = self.postfix(inner_expr.clone())?;
            }

            Ok(inner_expr)
        } else if accept!(self, Tag::Identifier) {
            let name = self.yank();
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

        expect!(self, Tag::RightParen);

        Ok(new_node!(
            self,
            Cast {
                type_spec: Some(ty_spec),
                expr: self.factor()?,
            }
        ))
    }

    fn const_int(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstInt, _);
        let value = yank!(self, Tag::ConstInt);
        let node = new_node!(self, ConstInt(value as i32));
        node.borrow_mut().ty = int_type(true);

        Ok(node)
    }

    fn const_unsigned_int(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstUnsignedInt, _);
        let value = yank!(self, Tag::ConstUnsignedInt);
        let node = new_node!(self, ConstUnsignedInt(value as u32));
        node.borrow_mut().ty = int_type(false);

        Ok(node)
    }

    fn const_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstLong, _);
        let value = yank!(self, Tag::ConstLong);
        let node = new_node!(self, ConstLong(value));
        node.borrow_mut().ty = long_type(true);

        Ok(node)
    }

    fn const_long_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstLongLong, _);
        let value = yank!(self, Tag::ConstLongLong);
        let node = new_node!(self, ConstLong(value));
        node.borrow_mut().ty = long_type(true);

        Ok(node)
    }

    fn const_unsigned_long_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstUnsignedLongLong, _);
        let value = yank!(self, Tag::ConstUnsignedLongLong);
        let node = new_node!(self, ConstUnsignedLong(value));
        node.borrow_mut().ty = long_type(false);

        Ok(node)
    }

    fn const_unsigned_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstUnsignedLong, _);
        let value = yank!(self, Tag::ConstUnsignedLong);
        let node = new_node!(self, ConstUnsignedLong(value));
        node.borrow_mut().ty = long_type(false);

        Ok(node)
    }

    fn const_double(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstDouble, _);
        let value = yank!(self, Tag::ConstDouble);
        let node = new_node!(self, ConstDouble(value));
        node.borrow_mut().ty = double_type();

        Ok(node)
    }

    fn oops(&self, why: &str) -> Result<AstRef, String> {
        Err(why.to_string())
    }

    fn eof(&self) -> bool {
        self.tokens[1].is_none()
    }

    fn preload(&mut self) -> Result<(), String> {
        for _ in 0..2 {
            for i in 0..2 {
                self.tokens[i] = self.tokens[i + 1].clone();
            }
            self.pull_token()?;
        }

        Ok(())
    }

    fn pull_token(&mut self) -> Result<(), String> {
        match self.tokeniser.next() {
            Some(res) => match res {
                Ok(token) => {
                    self.tokens[2] = Some(token);
                    Ok(())
                }
                Err(e) => Err(e),
            },
            None => {
                self.tokens[2] = None;
                Ok(())
            }
        }
    }

    fn advance(&mut self) -> Result<(), String> {
        if self.tokens[1].is_none() {
            panic!("attempt to read past end of file");
        }

        for i in 0..2 {
            self.tokens[i] = self.tokens[i + 1].clone();
        }

        /*
        println!(
            "{:?} \"{}\"",
            self.tokens[1],
            if let Some(token) = &self.tokens[1] {
                token.as_str(self.buf)
            } else {
                ""
            }
        );*/

        self.pull_token()
    }
}
