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
                #[cfg(feature = "tracing")]
                println!("  {}", token.as_str($parser.buf));
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
                #[cfg(feature = "tracing")]
                println!("  {}", token.as_str($parser.buf));
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
                    "expected {:?} but got {:?}, parser.rs:{}",
                    stringify!($tag),
                    token.tag,
                    line!()
                );
                return Err(msg);
            } else {
                #[cfg(feature = "tracing")]
                println!("  {}", token.as_str($parser.buf));
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
                #[cfg(feature = "tracing")]
                println!("  {}", token.as_str($parser.buf));
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

macro_rules! got {
    ($parser:expr, $tag:path) => {
        if let Some(token) = &$parser.tokens[0] {
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
        if let Some(token) = &$parser.tokens[0] {
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

#[derive(Debug, Clone)]
pub struct Parser<'buf> {
    buf: &'buf str,
    tokeniser: Tokeniser<'buf>,
    tokens: [Option<Token>; 3],
    nr_nodes: usize,
    scope: ScopeRef,
    cases: Vec<Vec<AstRef>>,
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
        | Tag::XorEq
        | Tag::AndEq
        | Tag::LeftShiftEq
        | Tag::RightShiftEq => 1,
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
        self.translation_unit()
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn translation_unit(&mut self) -> Result<Vec<AstRef>, String> {
        let mut prog: Vec<AstRef> = vec![];

        while !self.eof() {
            prog.extend(self.declaration()?);
        }

        Ok(prog)
    }

    fn yank(&mut self) -> String {
        self.tokens[0].as_ref().unwrap().to_string(self.buf)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn pointer(&mut self, type_spec: &AstRef) -> Result<AstRef, String> {
        let mut qualifiers: Vec<String> = Vec::new();

        expect!(self, Tag::Asterisk);

        while self.is_type_qualifier() {
            qualifiers.push(self.type_qualifier()?);
        }

        let mut pointer = self.pointer_to(type_spec, qualifiers)?;

        if peek!(self, Tag::Asterisk) {
            pointer = self.pointer(&pointer)?;
        }

        Ok(pointer)
    }

    fn pointer_to(
        &mut self,
        type_spec: &AstRef,
        qualifiers: Vec<String>,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            Pointer {
                base_type_spec: type_spec.clone(),
                qualifiers: qualifiers
            }
        ))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_suffix(
        &mut self,
        type_spec: &AstRef,
        storage_class: Option<StorageClass>,
        name: Option<String>,
    ) -> Result<AstRef, String> {
        if peek!(self, Tag::LeftParen) {
            self.function(type_spec.clone(), storage_class, name)
        } else if let Some(ident) = &name {
            if let AstKind::Function { name, sym, .. } =
                &mut type_spec.borrow_mut().kind
            {
                let s = add_sym(
                    self.scope.clone(),
                    &ident,
                    SymKind::Function,
                    1,
                    16,
                    storage_class,
                    None,
                    Some(type_spec.clone()),
                )?;

                *sym = Some(Rc::downgrade(&s));
                *name = Some(ident.clone());

                Ok(type_spec.clone())
            } else {
                self.variable(
                    type_spec.clone(),
                    storage_class,
                    ident.to_string(),
                )
            }
        } else {
            Ok(type_spec.clone())
        }
    }

    fn skip_declarator(&mut self) -> Result<(), String> {
        // Open a block scope to capture any declarations and ensure they are
        // unresolvable.
        self.open_scope(ScopeKind::Block);
        let tmp_type_spec = new_node!(self, Void);
        self.declarator(tmp_type_spec, None)?;
        self.close_scope();

        Ok(())
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn direct_declarator(
        &mut self,
        type_spec: AstRef,
        storage_class: Option<StorageClass>,
    ) -> Result<AstRef, String> {
        let mut inner_type_spec = type_spec;
        let mut name: Option<String> = None;

        if accept!(self, Tag::LeftParen) {
            let start = self.clone();

            self.skip_declarator()?;

            expect!(self, Tag::RightParen);

            inner_type_spec =
                self.type_suffix(&inner_type_spec, storage_class, None)?;

            let end = self.clone();

            // Now we wrap the type_suffix with the declarator we skipped
            // by backtracking and reparsing it.

            *self = start;

            let decl = self.declarator(inner_type_spec, storage_class)?;

            // And then return after jumping over the type_suffix since
            // we've already parsed that.

            *self = end;

            return Ok(decl);
        }

        if accept!(self, Tag::Identifier) {
            name = Some(self.yank());
        }

        inner_type_spec =
            self.type_suffix(&inner_type_spec, storage_class, name.clone())?;

        Ok(inner_type_spec)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn declarator(
        &mut self,
        mut type_spec: AstRef,
        storage_class: Option<StorageClass>,
    ) -> Result<AstRef, String> {
        if peek!(self, Tag::Asterisk) {
            type_spec = self.pointer(&type_spec)?;
        }

        self.direct_declarator(type_spec, storage_class)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn abstract_declarator(
        &mut self,
        type_spec: AstRef,
    ) -> Result<AstRef, String> {
        self.declarator(type_spec, None)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_name(&mut self) -> Result<AstRef, String> {
        let (type_spec, storage_class) = self.declaration_specifiers()?;

        if storage_class.is_some() {
            return Err("type names cannot have a storage class".into());
        }

        self.abstract_declarator(type_spec)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn init_declarator(
        &mut self,
        type_spec: AstRef,
        storage_class: Option<StorageClass>,
    ) -> Result<AstRef, String> {
        let decl = self.declarator(type_spec, storage_class)?;

        match &mut decl.borrow_mut().kind {
            AstKind::Function { block, .. } => {
                if peek!(self, Tag::LeftBrace) {
                    *block = Some(self.function_body()?);
                    self.close_scope();
                }
            }
            AstKind::Variable { init, .. } => {
                let initialiser = if accept!(self, Tag::Assign) {
                    Some(self.initialiser(&storage_class)?)
                } else {
                    None
                };

                if initialiser.is_some() {
                    *init = initialiser;
                }
            }
            _ => {}
        }

        Ok(decl)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn init_declarator_list(
        &mut self,
        type_spec: &AstRef,
        storage_class: Option<StorageClass>,
    ) -> Result<Vec<AstRef>, String> {
        let mut decls: Vec<AstRef> = Vec::new();

        loop {
            let decl =
                self.init_declarator(type_spec.clone(), storage_class)?;

            decls.push(decl);

            if got!(self, Tag::RightBrace) {
                if decls.len() > 1 {
                    return Err(
                        "a declarator list cannot contain a function definition".into());
                }
                return Ok(decls);
            }

            if !accept!(self, Tag::Comma) {
                break;
            }
        }

        expect!(self, Tag::Semicolon);

        Ok(decls)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn declaration(&mut self) -> Result<Vec<AstRef>, String> {
        let (type_spec, storage_class) = self.declaration_specifiers()?;
        let decls = self.init_declarator_list(&type_spec, storage_class)?;

        Ok(decls)
    }

    fn is_storage_class(&mut self) -> bool {
        peek!(self, Tag::Static)
            || peek!(self, Tag::Extern)
            || peek!(self, Tag::Auto)
            || peek!(self, Tag::Register)
    }

    fn is_declspec(&mut self) -> bool {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn declaration_specifiers(
        &mut self,
    ) -> Result<(AstRef, Option<StorageClass>), String> {
        let mut type_specs: Vec<String> = vec![];
        let mut storage_classes: Vec<StorageClass> = vec![];

        loop {
            if self.is_type_spec() {
                type_specs.push(self.type_specifier()?);
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

        let type_spec = self.normalise_type_spec(&type_specs)?;

        if storage_classes.len() > 1 {
            return Err("invalid storage class".to_string());
        }

        if storage_classes.len() == 1 {
            Ok((type_spec, Some(storage_classes[0])))
        } else {
            Ok((type_spec, None))
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn parameter_list(&mut self) -> Result<Vec<AstRef>, String> {
        let mut params: Vec<AstRef> = vec![];
        expect!(self, Tag::LeftParen);

        if !accept!(self, Tag::RightParen) {
            if accept!(self, Tag::Void) {
                expect!(self, Tag::RightParen);
            } else {
                loop {
                    params.push(self.parameter()?);

                    if accept!(self, Tag::RightParen) {
                        break;
                    } else {
                        expect!(self, Tag::Comma);
                    }
                }
            }
        }

        Ok(params)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn parameter(&mut self) -> Result<AstRef, String> {
        let (type_spec, storage_class) = self.declaration_specifiers()?;

        if storage_class.is_some() {
            if let Some(StorageClass::Register) = storage_class {
            } else {
                return Err("invalid storage class".to_string());
            }
        }

        self.declarator(type_spec, storage_class)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn function(
        &mut self,
        type_spec: AstRef,
        storage_class: Option<StorageClass>,
        name: Option<String>,
    ) -> Result<AstRef, String> {
        self.open_scope(ScopeKind::Function);
        let params = self.parameter_list()?;
        let mut sym: Option<SymRef> = None;

        if let Some(ident) = &name {
            sym = Some(add_sym(
                parent_of(&self.scope),
                &ident,
                SymKind::Function,
                1,
                16,
                storage_class,
                if peek!(self, Tag::LeftBrace) {
                    Some(Definition::Concrete)
                } else {
                    None
                },
                None,
            )?);
        }

        if !peek!(self, Tag::LeftBrace) {
            self.close_scope();
        }

        let decl = new_node!(
            self,
            Function {
                name: name.clone(),
                sym: if let Some(sym) = &sym {
                    Some(Rc::downgrade(sym))
                } else {
                    None
                },
                params: params.clone(),
                block: None,
                type_spec: type_spec.clone(),
            }
        );

        if let Some(sym) = &sym {
            sym.borrow_mut().node = Some(Rc::downgrade(&decl.clone()));
        }

        Ok(decl)
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn initialiser(
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn variable(
        &mut self,
        type_spec: AstRef,
        storage_class: Option<StorageClass>,
        name: String,
    ) -> Result<AstRef, String> {
        let definition = self.determine_definition_type(storage_class);

        Self::type_annotate(&type_spec)?;
        let ty = type_of(&type_spec);

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

        let var = new_node!(
            self,
            Variable {
                name: name.clone(),
                sym: Some(Rc::downgrade(&sym)),
                type_spec: type_spec.clone(),
                init: None,
            }
        );

        sym.borrow_mut().node = Some(Rc::downgrade(&var));

        Ok(var)
    }

    fn type_annotate(type_spec: &AstRef) -> Result<(), String> {
        let mut annotator = TypeAnnotator::new();
        annotator.run(&[type_spec.clone()])
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_qualifier(&mut self) -> Result<String, String> {
        if accept!(self, Tag::Const) {
            Ok("const".to_string())
        } else if accept!(self, Tag::Volatile) {
            Ok("volatile".to_string())
        } else if accept!(self, Tag::Restrict) {
            Ok("restrict".to_string())
        } else {
            Err("invalid type qualifier".to_string())
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_specifier(&mut self) -> Result<String, String> {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn for_init(&mut self) -> Result<AstRef, String> {
        if self.is_declspec() {
            let (type_spec, storage_class) = self.declaration_specifiers()?;

            if storage_class.is_some() {
                return Err(
                    "loop initial declaration of '{}' cannot have a storage class".into()
                );
            }

            let decl = self.init_declarator(type_spec, storage_class)?;

            match &decl.borrow().kind {
                AstKind::Variable { .. } => {}
                _ => {
                    return Err("loop initial declaration is invalid".into());
                }
            }

            expect!(self, Tag::Semicolon);
            Ok(decl)
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn break_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::Break);
        expect!(self, Tag::Semicolon);

        Ok(new_node!(self, Break { to: None }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn continue_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::Continue);
        expect!(self, Tag::Semicolon);

        Ok(new_node!(self, Continue { to: None }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn block(&mut self) -> Result<AstRef, String> {
        let mut body: Vec<AstRef> = vec![];
        self.open_scope(ScopeKind::Block);

        expect!(self, Tag::LeftBrace);

        while !accept!(self, Tag::RightBrace) {
            if self.is_declspec() {
                body.extend(self.declaration()?);
            } else {
                body.push(self.statement()?);
            }
        }

        self.close_scope();

        Ok(new_node!(self, Block { body: body }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn function_body(&mut self) -> Result<AstRef, String> {
        let mut body: Vec<AstRef> = vec![];

        expect!(self, Tag::LeftBrace);

        while !accept!(self, Tag::RightBrace) {
            if self.is_declspec() {
                body.extend(self.declaration()?);
            } else {
                body.push(self.statement()?);
            }
        }

        Ok(new_node!(self, Block { body: body }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn goto_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::GoTo);
        expect!(self, Tag::Identifier);
        let label = self.yank();
        expect!(self, Tag::Semicolon);

        Ok(new_node!(self, GoTo { label: label }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn return_stmt(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::Return);
        let expr = self.expr(0)?;
        expect!(self, Tag::Semicolon);
        Ok(new_node!(self, Return { expr: expr }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn expr(&mut self, min_prec: i32) -> Result<AstRef, String> {
        let mut left = self.factor()?;
        let mut prec: i32;

        while {
            prec = precedence_of(&peek_tag!(self));
            self.peek_binop() && prec >= min_prec
        } {
            if accept!(self, Tag::Assign) {
                let right = self.expr(prec)?;
                left = self.assignment(left, right)?;
            } else if accept!(self, Tag::PlusEq) {
                let right = self.expr(prec)?;
                left = self.plus_eq(left, right)?;
            } else if accept!(self, Tag::MinusEq) {
                let right = self.expr(prec)?;
                left = self.minus_eq(left, right)?;
            } else if accept!(self, Tag::MultEq) {
                let right = self.expr(prec)?;
                left = self.mult_eq(left, right)?;
            } else if accept!(self, Tag::DivideEq) {
                let right = self.expr(prec)?;
                left = self.div_eq(left, right)?;
            } else if accept!(self, Tag::ModEq) {
                let right = self.expr(prec)?;
                left = self.mod_eq(left, right)?;
            } else if accept!(self, Tag::AndEq) {
                let right = self.expr(prec)?;
                left = self.and_eq(left, right)?;
            } else if accept!(self, Tag::OrEq) {
                let right = self.expr(prec)?;
                left = self.or_eq(left, right)?;
            } else if accept!(self, Tag::XorEq) {
                let right = self.expr(prec)?;
                left = self.xor_eq(left, right)?;
            } else if accept!(self, Tag::LeftShiftEq) {
                let right = self.expr(prec)?;
                left = self.lshift_eq(left, right)?;
            } else if accept!(self, Tag::RightShiftEq) {
                let right = self.expr(prec)?;
                left = self.rshift_eq(left, right)?;
            } else if accept!(self, Tag::Question) {
                left = self.conditional(left, prec)?;
            } else {
                left = self.binop(left, precedence_of(&peek_tag!(self)) + 1)?;
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn addr_of(&mut self, expr: AstRef) -> Result<AstRef, String> {
        Ok(new_node!(self, AddrOf { expr: expr.clone() }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn deref(&mut self, expr: AstRef) -> Result<AstRef, String> {
        Ok(new_node!(self, Deref { expr: expr.clone() }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn plus_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn minus_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn mult_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn div_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn mod_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn and_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn or_eq(&mut self, left: AstRef, right: AstRef) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn xor_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn lshift_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn rshift_eq(
        &mut self,
        left: AstRef,
        right: AstRef,
    ) -> Result<AstRef, String> {
        Ok(new_node!(
            self,
            CompoundAssign {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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
        } else if accept!(self, Tag::Ampersand) {
            let subexpr = self.factor()?;
            return self.addr_of(subexpr);
        } else if accept!(self, Tag::Asterisk) {
            let subexpr = self.factor()?;
            return self.deref(subexpr);
        } else if accept!(self, Tag::Incr) {
            let subexpr = self.factor()?;
            return self.pre_incr(subexpr);
        } else if accept!(self, Tag::Decr) {
            let subexpr = self.factor()?;
            return self.pre_decr(subexpr);
        } else if accept!(self, Tag::LeftParen) {
            if self.is_declspec() {
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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn cast_expr(&mut self) -> Result<AstRef, String> {
        let type_spec = self.type_name()?;
        expect!(self, Tag::RightParen);

        Ok(new_node!(
            self,
            Cast {
                type_spec: Some(type_spec),
                expr: self.factor()?,
            }
        ))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_int(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstInt, _);
        let value = yank!(self, Tag::ConstInt);
        let node = new_node!(self, ConstInt(value as i32));
        node.borrow_mut().ty = int_type(true);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_unsigned_int(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstUnsignedInt, _);
        let value = yank!(self, Tag::ConstUnsignedInt);
        let node = new_node!(self, ConstUnsignedInt(value as u32));
        node.borrow_mut().ty = int_type(false);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstLong, _);
        let value = yank!(self, Tag::ConstLong);
        let node = new_node!(self, ConstLong(value));
        node.borrow_mut().ty = long_type(true);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_long_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstLongLong, _);
        let value = yank!(self, Tag::ConstLongLong);
        let node = new_node!(self, ConstLong(value));
        node.borrow_mut().ty = long_type(true);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_unsigned_long_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstUnsignedLongLong, _);
        let value = yank!(self, Tag::ConstUnsignedLongLong);
        let node = new_node!(self, ConstUnsignedLong(value));
        node.borrow_mut().ty = long_type(false);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_unsigned_long(&mut self) -> Result<AstRef, String> {
        expect!(self, Tag::ConstUnsignedLong, _);
        let value = yank!(self, Tag::ConstUnsignedLong);
        let node = new_node!(self, ConstUnsignedLong(value));
        node.borrow_mut().ty = long_type(false);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
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

        /*println!(
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
