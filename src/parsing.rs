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

use std::cell::Cell;

use crate::ast::*;
use crate::errors::{Error, ErrorClass::Parsing, ParsingError, error};
use crate::expr::*;
use crate::lexing::{Token, TokenTag as Tag, Tokeniser};
use crate::symtab::*;
use crate::types::*;

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
                return Err(error(Parsing(ParsingError::ExpectedButGot {
                    expected: stringify!($tag).to_string(),
                    got: format!("{:?}", token.tag),
                })));
            } else {
                #[cfg(feature = "tracing")]
                println!("  {}", token.as_str($parser.buf));
                $parser.advance()?;
            }
        } else {
            return Err(error(Parsing(ParsingError::ExpectedButReachedEof(
                stringify!($tag).to_string(),
            ))));
        }
    };

    ($parser:expr, $tag:path, $_:tt) => {
        if let Some(token) = &$parser.tokens[1] {
            if !matches!(token.tag, $tag(_)) {
                return Err(error(Parsing(ParsingError::ExpectedButGot {
                    expected: stringify!($tag).to_string(),
                    got: format!("{:?}", token.tag),
                })));
            } else {
                #[cfg(feature = "tracing")]
                println!("  {}", token.as_str($parser.buf));
                $parser.advance()?;
            }
        } else {
            return Err(error(Parsing(ParsingError::ExpectedButReachedEof(
                stringify!($tag).to_string(),
            ))));
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

pub struct Parser<'buf, 'sym> {
    buf: &'buf str,
    tokeniser: Tokeniser<'buf>,
    tokens: [Option<Token>; 3],
    fn_param: bool,
    backtracking: bool,
    scope: ScopeId,
    arena: AstArena,
    symtab: &'sym mut SymTab,
    cases: Vec<Vec<AstId>>,
}

#[derive(Clone)]
struct ParserSnapshot<'buf> {
    tokeniser: Tokeniser<'buf>,
    tokens: [Option<Token>; 3],
    fn_param: bool,
    backtracking: bool,
    scope: ScopeId,
    cases: Vec<Vec<AstId>>,
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

impl<'buf, 'sym> Parser<'buf, 'sym> {
    pub fn new(
        buf: &'buf str,
        symtab: &'sym mut SymTab,
    ) -> Result<Self, Error> {
        let mut parser = Self {
            buf: buf,
            tokeniser: Tokeniser::new(buf),
            tokens: [None, None, None],
            fn_param: false,
            backtracking: false,
            scope: ScopeId(0),
            arena: AstArena::new(),
            symtab,
            cases: vec![],
        };

        parser.preload()?;

        Ok(parser)
    }

    fn open_scope(&mut self, tag: ScopeKind) {
        let new_scope = self.symtab.open_scope(self.scope, tag);
        self.scope = new_scope;
    }

    fn close_scope(&mut self) {
        self.scope = self.symtab.close_scope(self.scope);
    }

    fn alloc(&mut self, kind: AstKind) -> AstId {
        self.arena.alloc(kind, None, self.scope)
    }

    pub fn parse(&mut self) -> Result<(AstArena, Vec<AstId>), Error> {
        let top = self.translation_unit()?;
        let arena = std::mem::replace(&mut self.arena, AstArena::new());
        Ok((arena, top))
    }

    fn save_state(&self) -> ParserSnapshot<'buf> {
        ParserSnapshot {
            tokeniser: self.tokeniser.clone(),
            tokens: self.tokens.clone(),
            fn_param: self.fn_param,
            backtracking: self.backtracking,
            scope: self.scope,
            cases: self.cases.clone(),
        }
    }

    fn get_state(&self) -> ParserSnapshot<'buf> {
        self.save_state()
    }

    fn restore_state(&mut self, state: &ParserSnapshot<'buf>) {
        self.tokeniser = state.tokeniser.clone();
        self.tokens = state.tokens.clone();
        self.fn_param = state.fn_param;
        self.backtracking = state.backtracking;
        self.scope = state.scope;
        self.cases = state.cases.clone();
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn translation_unit(&mut self) -> Result<Vec<AstId>, Error> {
        let mut prog: Vec<AstId> = vec![];

        while !self.eof() {
            prog.extend(self.declaration()?);
        }

        Ok(prog)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn pointer(&mut self, type_spec: AstId) -> Result<AstId, Error> {
        let mut qualifiers: Vec<String> = Vec::new();

        expect!(self, Tag::Asterisk);

        while self.is_type_qualifier() {
            qualifiers.push(self.type_qualifier()?);
        }

        let mut pointer = self.pointer_to(type_spec, qualifiers)?;

        if peek!(self, Tag::Asterisk) {
            pointer = self.pointer(pointer)?;
        }

        Ok(pointer)
    }

    fn pointer_to(
        &mut self,
        type_spec: AstId,
        qualifiers: Vec<String>,
    ) -> Result<AstId, Error> {
        Ok(self.alloc(AstKind::Pointer {
            base_type_spec: type_spec,
            qualifiers: qualifiers,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn array(&mut self, type_spec: AstId) -> Result<AstId, Error> {
        let mut dimensions = Vec::new();

        while accept!(self, Tag::LeftBracket) {
            let dimension = if accept!(self, Tag::RightBracket) {
                None
            } else {
                let dim = self.expr(0)?;
                expect!(self, Tag::RightBracket);
                Some(dim)
            };
            dimensions.push(dimension);
        }

        let mut elem_type_spec = type_spec;
        for dim in dimensions.into_iter().rev() {
            let len = if let Some(d) = dim {
                Self::array_dimension(&self.arena, d)?
            } else {
                0
            };

            elem_type_spec = self.alloc(AstKind::Array {
                type_spec: elem_type_spec,
                dimension: dim,
                len: Cell::new(len),
            });
        }

        Ok(elem_type_spec)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_suffix(
        &mut self,
        type_spec: AstId,
        storage_class: Option<StorageClass>,
        name: Option<Token>,
    ) -> Result<AstId, Error> {
        if peek!(self, Tag::LeftParen) {
            self.function(type_spec, storage_class, name)
        } else if peek!(self, Tag::LeftBracket) {
            let array = self.array(type_spec)?;

            if let Some(ident) = &name {
                self.variable(array, storage_class, ident.clone())
            } else {
                Ok(array)
            }
        } else if let Some(ident) = &name {
            if let AstKind::Function { name, sym, .. } =
                &mut self.arena[type_spec].kind
            {
                if !self.backtracking {
                    let s = self.symtab.declare_sym(
                        self.scope,
                        &ident.as_str(self.buf),
                        SymKind::Function,
                        storage_class,
                        None,
                        Some(type_spec),
                    )?;

                    *sym = Some(s);
                }

                *name = Some(ident.clone());

                Ok(type_spec)
            } else {
                self.variable(type_spec, storage_class, ident.clone())
            }
        } else {
            Ok(type_spec)
        }
    }

    fn skip_declarator(&mut self, allow_name: bool) -> Result<(), Error> {
        let scope = self.scope;
        self.open_scope(ScopeKind::Block);
        let tmp_type_spec = self.alloc(AstKind::Void);
        let backtracking = self.backtracking;
        self.backtracking = true;
        self.declarator(tmp_type_spec, None, allow_name)?;
        self.backtracking = backtracking;

        loop {
            if self.scope == scope {
                break;
            } else {
                self.close_scope();
            }
        }

        Ok(())
    }

    fn set_state(&mut self, state: &ParserSnapshot<'buf>) {
        let arena = std::mem::replace(&mut self.arena, AstArena::new());
        let scope = self.scope;
        self.restore_state(state);
        self.arena = arena;
        self.scope = scope;
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn direct_declarator(
        &mut self,
        type_spec: AstId,
        storage_class: Option<StorageClass>,
        allow_name: bool,
    ) -> Result<AstId, Error> {
        let mut inner_type_spec = type_spec;
        let mut name: Option<Token> = None;

        if accept!(self, Tag::LeftParen) {
            let start = self.get_state();
            self.skip_declarator(allow_name)?;

            expect!(self, Tag::RightParen);

            inner_type_spec =
                self.type_suffix(inner_type_spec, storage_class, None)?;

            let end = self.get_state();

            self.set_state(&start);

            let decl =
                self.declarator(inner_type_spec, storage_class, allow_name)?;

            self.set_state(&end);

            return Ok(decl);
        }

        if accept!(self, Tag::Identifier) {
            if !allow_name {
                return Err(error(Parsing(
                    ParsingError::AbstractDeclaratorCannotHaveIdentifier,
                )));
            }
            name = Some(self.tokens[0].as_ref().unwrap().clone());
        }

        inner_type_spec =
            self.type_suffix(inner_type_spec, storage_class, name.clone())?;

        Ok(inner_type_spec)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn declarator(
        &mut self,
        mut type_spec: AstId,
        storage_class: Option<StorageClass>,
        allow_name: bool,
    ) -> Result<AstId, Error> {
        if peek!(self, Tag::Asterisk) {
            type_spec = self.pointer(type_spec)?;
        }

        self.direct_declarator(type_spec, storage_class, allow_name)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn abstract_declarator(
        &mut self,
        type_spec: AstId,
    ) -> Result<AstId, Error> {
        self.declarator(type_spec, None, false)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_name(&mut self) -> Result<AstId, Error> {
        let (type_spec, storage_class) = self.declaration_specifiers()?;

        if storage_class.is_some() {
            return Err(error(Parsing(
                ParsingError::TypeNamesCannotHaveStorageClass,
            )));
        }

        self.abstract_declarator(type_spec)
    }

    fn array_dimension(arena: &AstArena, expr: AstId) -> Result<usize, Error> {
        if is_const_unsigned_int_expr(arena, expr) {
            return Ok(const_unsigned_int_value(arena, expr) as usize);
        }

        if is_const_int_expr(arena, expr) {
            let v = const_int_value(arena, expr);
            return if v < 0 {
                Err(error(Parsing(
                    ParsingError::ArrayDimensionCannotBeNegative(v),
                )))
            } else {
                Ok(v as usize)
            };
        }

        Err(error(Parsing(
            ParsingError::ArrayDimensionMustBeConstantIntegerExpression,
        )))
    }

    fn build_initialiser_for_array(
        &mut self,
        type_spec: AstId,
    ) -> Result<AstId, Error> {
        let dimension = match &self.arena[type_spec].kind {
            AstKind::Array {
                type_spec: _elem_type_spec,
                dimension,
                len: _,
            } => dimension.clone(),
            _ => unreachable!(),
        };

        let max_initialisers = if let Some(expr) = &dimension {
            Some(Self::array_dimension(&self.arena, *expr)?)
        } else {
            None
        };

        Ok(
            self.alloc(AstKind::CompoundInitialiser(CompoundInitialiser {
                type_spec: type_spec,
                initialisers: Vec::new(),
                max_initialisers: max_initialisers,
            })),
        )
    }

    fn build_initialiser_for(
        &mut self,
        type_spec: AstId,
    ) -> Result<AstId, Error> {
        match &self.arena[type_spec].kind {
            AstKind::Array { .. } => {
                self.build_initialiser_for_array(type_spec)
            }
            _ => Ok(self.alloc(AstKind::Initialiser {
                type_spec: type_spec,
                value: None,
            })),
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn init_declarator(
        &mut self,
        type_spec: AstId,
        storage_class: Option<StorageClass>,
    ) -> Result<AstId, Error> {
        let decl = self.declarator(type_spec, storage_class, true)?;

        let kind = self.arena[decl].kind.clone();

        match &kind {
            AstKind::Function { .. } => {
                let block = if peek!(self, Tag::LeftBrace) {
                    Some(self.function_body()?)
                } else {
                    None
                };
                if let AstKind::Function { block: b, .. } =
                    &mut self.arena[decl].kind
                {
                    *b = block;
                }
                self.close_scope();
            }
            AstKind::Variable {
                type_spec: ts, sym, ..
            } => {
                let ts = *ts;
                let definition = self.determine_definition_type(storage_class);
                let s = *sym;

                if let Some(s) = s {
                    self.symtab.update_sym(self.scope, s, definition)?;
                }

                let initialiser = self.build_initialiser_for(ts)?;

                if accept!(self, Tag::Assign) {
                    if matches!(
                        &self.arena[initialiser].kind,
                        AstKind::CompoundInitialiser(_)
                    ) {
                        self.array_initialiser_braced(initialiser, ts)?;
                    } else {
                        self.initialiser(initialiser, ts)?;
                    }
                    if let AstKind::Variable { init, .. } =
                        &mut self.arena[decl].kind
                    {
                        *init = Some(initialiser);
                    }
                }
            }
            _ => {}
        }

        Ok(decl)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn init_declarator_list(
        &mut self,
        type_spec: AstId,
        storage_class: Option<StorageClass>,
    ) -> Result<Vec<AstId>, Error> {
        let mut decls: Vec<AstId> = Vec::new();

        loop {
            let decl = self.init_declarator(type_spec, storage_class)?;

            decls.push(decl);

            if let AstKind::Function { block, .. } = &self.arena[decl].kind {
                if block.is_some() {
                    if decls.len() > 1 {
                        return Err(error(Parsing(
                            ParsingError::ADeclaratorListCannotContainFunctionDefinition,
                        )));
                    } else {
                        return Ok(decls);
                    }
                }
            }

            if !accept!(self, Tag::Comma) {
                break;
            }
        }

        expect!(self, Tag::Semicolon);

        Ok(decls)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn declaration(&mut self) -> Result<Vec<AstId>, Error> {
        let (type_spec, storage_class) = self.declaration_specifiers()?;
        let decls = self.init_declarator_list(type_spec, storage_class)?;

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
    ) -> Result<AstId, Error> {
        let mut is_signed = false;
        let mut is_unsigned = false;
        let mut has_int = false;
        let mut long_count = 0;
        let mut has_double = false;

        for spec in type_specs {
            match spec.as_str() {
                "signed" => {
                    if is_unsigned || is_signed || has_double {
                        return Err(error(Parsing(
                            ParsingError::InvalidTypeSpecifier,
                        )));
                    }
                    is_signed = true;
                }
                "unsigned" => {
                    if is_signed || is_unsigned || has_double {
                        return Err(error(Parsing(
                            ParsingError::InvalidTypeSpecifier,
                        )));
                    }
                    is_unsigned = true;
                }
                "int" => {
                    if has_int || has_double {
                        return Err(error(Parsing(
                            ParsingError::InvalidTypeSpecifier,
                        )));
                    }
                    has_int = true;
                }
                "long" => {
                    if long_count >= 2 || has_double {
                        return Err(error(Parsing(
                            ParsingError::InvalidTypeSpecifier,
                        )));
                    }
                    long_count += 1;
                }
                "double" => {
                    if has_double {
                        return Err(error(Parsing(
                            ParsingError::InvalidTypeSpecifier,
                        )));
                    }
                    has_double = true;
                }
                "void" => {
                    if type_specs.len() > 1 {
                        return Err(error(Parsing(
                            ParsingError::InvalidTypeSpecifier,
                        )));
                    }
                    return Ok(self.alloc(AstKind::Void));
                }
                _ => unreachable!("unexpected token tag"),
            }
        }

        if has_double {
            if is_signed || is_unsigned || has_int {
                return Err(error(Parsing(ParsingError::InvalidTypeSpecifier)));
            }

            let ty = if long_count == 1 {
                long_double_type()
            } else if long_count == 0 {
                double_type()
            } else {
                return Err(error(Parsing(ParsingError::InvalidTypeSpecifier)));
            };

            let node = self.alloc(AstKind::Double);
            self.arena[node].ty = ty;
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

        let node = self.alloc(AstKind::Int);
        self.arena[node].ty = ty;
        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn declaration_specifiers(
        &mut self,
    ) -> Result<(AstId, Option<StorageClass>), Error> {
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
            return Err(error(Parsing(ParsingError::InvalidTypeSpecifier)));
        }

        let type_spec = self.normalise_type_spec(&type_specs)?;

        if storage_classes.len() > 1 {
            return Err(error(Parsing(ParsingError::InvalidStorageClass)));
        }

        if storage_classes.len() == 1 {
            Ok((type_spec, Some(storage_classes[0])))
        } else {
            Ok((type_spec, None))
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn parameter_list(&mut self) -> Result<Vec<AstId>, Error> {
        let mut params: Vec<AstId> = vec![];
        expect!(self, Tag::LeftParen);

        self.fn_param = true;

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

        self.fn_param = false;

        Ok(params)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn parameter(&mut self) -> Result<AstId, Error> {
        let (type_spec, storage_class) = self.declaration_specifiers()?;

        if storage_class.is_some() {
            if let Some(StorageClass::Register) = storage_class {
            } else {
                return Err(error(Parsing(ParsingError::InvalidStorageClass)));
            }
        }

        self.declarator(type_spec, storage_class, true)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn function(
        &mut self,
        type_spec: AstId,
        storage_class: Option<StorageClass>,
        name: Option<Token>,
    ) -> Result<AstId, Error> {
        self.open_scope(ScopeKind::Function);
        let params = self.parameter_list()?;
        let mut sym: Option<SymId> = None;

        if let Some(ident) = &name
            && !self.backtracking
        {
            sym = Some(self.symtab.declare_sym(
                self.symtab.parent_of(self.scope),
                &ident.as_str(self.buf),
                SymKind::Function,
                storage_class,
                if peek!(self, Tag::LeftBrace) {
                    Some(Definition::Concrete)
                } else {
                    None
                },
                None,
            )?);
        }

        let decl = self.alloc(AstKind::Function {
            name: name.clone(),
            sym: sym,
            params: params.clone(),
            block: None,
            type_spec: type_spec,
        });

        if let Some(sym) = sym {
            self.symtab[sym].node = Some(decl);
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
        } else if !self.symtab.has_parent(self.scope) {
            Some(Definition::Tentative)
        } else {
            None
        }
    }

    fn count_array_initialiser_elems(
        &mut self,
        elem_type_spec: AstId,
    ) -> Result<usize, Error> {
        let state = self.save_state();

        let mut count = 0usize;
        let mut first = true;

        while !peek!(self, Tag::RightBrace) {
            if !first {
                if !accept!(self, Tag::Comma) {
                    break;
                }
                if peek!(self, Tag::RightBrace) {
                    break;
                }
            }
            first = false;

            self.skip_initialiser_of_type(elem_type_spec)?;

            count += 1;
        }

        self.restore_state(&state);

        Ok(count)
    }

    fn skip_initialiser_of_type(
        &mut self,
        type_spec: AstId,
    ) -> Result<(), Error> {
        let is_array =
            matches!(&self.arena[type_spec].kind, AstKind::Array { .. });

        if is_array {
            if peek!(self, Tag::LeftBrace) {
                self.skip_initialiser()?;
            } else {
                let dim;
                let elem_type;
                {
                    let kind = &self.arena[type_spec].kind;
                    if let AstKind::Array {
                        type_spec: elem_ts,
                        dimension,
                        len: _,
                    } = kind
                    {
                        elem_type = *elem_ts;
                        dim = dimension.as_ref().map(|e| *e);
                    } else {
                        unreachable!()
                    }
                }
                if let Some(dim_expr) = dim {
                    let dim_val = Self::array_dimension(&self.arena, dim_expr)?;
                    for i in 0..dim_val {
                        if i > 0 {
                            if !peek!(self, Tag::Comma) {
                                break;
                            }
                            accept!(self, Tag::Comma);
                        }
                        self.skip_initialiser_of_type(elem_type)?;
                    }
                }
            }
        } else {
            self.skip_initialiser()?;
        }

        Ok(())
    }

    fn skip_initialiser(&mut self) -> Result<(), Error> {
        if peek!(self, Tag::LeftBrace) {
            expect!(self, Tag::LeftBrace);
            let mut depth = 1;
            while depth > 0 && !self.eof() {
                if accept!(self, Tag::LeftBrace) {
                    depth += 1;
                } else if accept!(self, Tag::RightBrace) {
                    depth -= 1;
                } else {
                    self.advance()?;
                }
            }
        } else {
            while !peek!(self, Tag::Comma)
                && !peek!(self, Tag::RightBrace)
                && !peek!(self, Tag::Semicolon)
                && !self.eof()
            {
                self.advance()?;
            }
        }

        Ok(())
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn array_initialiser_braced(
        &mut self,
        initialiser: AstId,
        type_spec: AstId,
    ) -> Result<(), Error> {
        let mut ci: CompoundInitialiser = match &self.arena[initialiser].kind {
            AstKind::CompoundInitialiser(c) => c.clone(),
            _ => unreachable!(),
        };

        expect!(self, Tag::LeftBrace);

        let mut idx = 0usize;
        let mut first = true;

        if ci.max_initialisers.is_none() {
            let elem_type_spec = match &self.arena[type_spec].kind {
                AstKind::Array {
                    type_spec: elem_ts, ..
                } => *elem_ts,
                _ => return Ok(()),
            };

            let count = self.count_array_initialiser_elems(elem_type_spec)?;

            ci.max_initialisers = Some(count);
            if let AstKind::Array { len, .. } = &self.arena[type_spec].kind {
                len.set(count);
            }
        }

        while !peek!(self, Tag::RightBrace) {
            if !first {
                expect!(self, Tag::Comma);

                if peek!(self, Tag::RightBrace) {
                    break;
                }
            }
            first = false;

            let nr_elems = ci.max_initialisers.expect("unbounded initialiser");

            if idx < nr_elems {
                if idx < ci.initialisers.len() {
                    self.initialiser(ci.initialisers[idx], ci.type_spec)?;
                } else {
                    let elem_type_spec = match &self.arena[ci.type_spec].kind {
                        AstKind::Array {
                            type_spec: elem_type_spec,
                            ..
                        } => *elem_type_spec,
                        _ => {
                            unreachable!();
                        }
                    };

                    let elem_initialiser =
                        self.build_initialiser_for(elem_type_spec)?;
                    ci.initialisers.push(elem_initialiser);
                    self.initialiser(ci.initialisers[idx], elem_type_spec)?;
                }
            } else {
                return Err(error(Parsing(
                    ParsingError::ExcessElementsInArrayInitialiser,
                )));
            }

            idx += 1;
        }

        expect!(self, Tag::RightBrace);

        self.arena[initialiser].kind = AstKind::CompoundInitialiser(ci);

        Ok(())
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn array_initialiser(
        &mut self,
        initialiser: AstId,
        type_spec: AstId,
    ) -> Result<(), Error> {
        let mut ci: CompoundInitialiser = match &self.arena[initialiser].kind {
            AstKind::CompoundInitialiser(c) => c.clone(),
            _ => unreachable!(),
        };

        if ci.max_initialisers.is_none() {
            let elem_type_spec = match &self.arena[type_spec].kind {
                AstKind::Array {
                    type_spec: elem_ts, ..
                } => *elem_ts,
                _ => return Ok(()),
            };

            let count = self.count_array_initialiser_elems(elem_type_spec)?;

            ci.max_initialisers = Some(count);
            if let AstKind::Array { len, .. } = &self.arena[type_spec].kind {
                len.set(count);
            }
        }

        let max_elems = ci.max_initialisers.expect("unbounded initialiser");

        for idx in 0..max_elems {
            if idx > 0 {
                expect!(self, Tag::Comma);
            }

            if peek!(self, Tag::LeftBracket) || peek!(self, Tag::Dot) {
                self.arena[initialiser].kind = AstKind::CompoundInitialiser(ci);
                return Ok(());
            }

            if idx < ci.initialisers.len() {
                self.initialiser(ci.initialisers[idx], ci.type_spec)?;
            } else {
                let elem_type_spec = match &self.arena[ci.type_spec].kind {
                    AstKind::Array {
                        type_spec: elem_type_spec,
                        ..
                    } => *elem_type_spec,
                    _ => {
                        unreachable!();
                    }
                };
                let elem_initialiser =
                    self.build_initialiser_for(elem_type_spec)?;
                ci.initialisers.push(elem_initialiser);
                self.initialiser(ci.initialisers[idx], elem_type_spec)?;
            }
        }

        self.arena[initialiser].kind = AstKind::CompoundInitialiser(ci);

        Ok(())
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn expr_initialiser(
        &mut self,
        initialiser: AstId,
        _type_spec: AstId,
    ) -> Result<(), Error> {
        let expr = self.expr(0)?;

        match &mut self.arena[initialiser].kind {
            AstKind::Initialiser {
                type_spec: _,
                value,
            } => {
                *value = Some(expr);
            }
            _ => {
                todo!()
            }
        }

        Ok(())
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn initialiser(
        &mut self,
        initialiser: AstId,
        type_spec: AstId,
    ) -> Result<(), Error> {
        let is_array =
            matches!(&self.arena[type_spec].kind, AstKind::Array { .. });
        let is_compound = matches!(
            &self.arena[initialiser].kind,
            AstKind::CompoundInitialiser(_)
        );

        if is_array && is_compound {
            if peek!(self, Tag::LeftBrace) {
                self.array_initialiser_braced(initialiser, type_spec)
            } else {
                self.array_initialiser(initialiser, type_spec)
            }
        } else if !is_array {
            self.expr_initialiser(initialiser, type_spec)
        } else {
            unreachable!()
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn variable(
        &mut self,
        type_spec: AstId,
        storage_class: Option<StorageClass>,
        name: Token,
    ) -> Result<AstId, Error> {
        let sym = if !self.backtracking {
            Some(self.symtab.declare_sym(
                self.scope,
                &name.as_str(self.buf),
                SymKind::Variable,
                storage_class,
                None,
                None,
            )?)
        } else {
            None
        };

        let var = if self.fn_param {
            self.alloc(AstKind::Parameter {
                name: name.clone(),
                sym: sym,
                type_spec: type_spec,
            })
        } else {
            self.alloc(AstKind::Variable {
                name: name.clone(),
                sym: sym,
                type_spec: type_spec,
                init: None,
            })
        };

        if let Some(sym) = sym {
            self.symtab[sym].node = Some(var);
        }

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

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_qualifier(&mut self) -> Result<String, Error> {
        if accept!(self, Tag::Const) {
            Ok("const".to_string())
        } else if accept!(self, Tag::Volatile) {
            Ok("volatile".to_string())
        } else if accept!(self, Tag::Restrict) {
            Ok("restrict".to_string())
        } else {
            Err(error(Parsing(ParsingError::InvalidTypeQualifier)))
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn type_specifier(&mut self) -> Result<String, Error> {
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
            Err(error(Parsing(ParsingError::InvalidTypeSpecifier)))
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn storage_class(&mut self) -> Result<StorageClass, Error> {
        if accept!(self, Tag::Static) {
            Ok(StorageClass::Static)
        } else if accept!(self, Tag::Extern) {
            Ok(StorageClass::Extern)
        } else if accept!(self, Tag::Auto) {
            Ok(StorageClass::Auto)
        } else if accept!(self, Tag::Register) {
            Ok(StorageClass::Register)
        } else {
            Err(error(Parsing(ParsingError::UnknownStorageClassSpecifier)))
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn statement(&mut self) -> Result<AstId, Error> {
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
            Ok(self.alloc(AstKind::EmptyStmt))
        } else {
            self.expr_stmt()
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn while_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::While);
        expect!(self, Tag::LeftParen);
        let cond = self.expr(0)?;
        expect!(self, Tag::RightParen);
        self.open_scope(ScopeKind::Loop);
        let body = self.statement()?;
        self.close_scope();

        Ok(self.alloc(AstKind::While {
            cond: cond,
            body: body,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn do_while_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::Do);
        self.open_scope(ScopeKind::Loop);
        let body = self.statement()?;
        self.close_scope();
        expect!(self, Tag::While);
        expect!(self, Tag::LeftParen);
        let cond = self.expr(0)?;
        expect!(self, Tag::RightParen);
        expect!(self, Tag::Semicolon);

        Ok(self.alloc(AstKind::DoWhile {
            cond: cond,
            body: body,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn for_init(&mut self) -> Result<AstId, Error> {
        if self.is_declspec() {
            let (type_spec, storage_class) = self.declaration_specifiers()?;

            if storage_class.is_some() {
                return Err(error(Parsing(
                    ParsingError::LoopInitialDeclarationIsInvalid,
                )));
            }

            let decl = self.init_declarator(type_spec, storage_class)?;

            match &self.arena[decl].kind {
                AstKind::Variable { .. } => {}
                _ => {
                    return Err(error(Parsing(
                        ParsingError::LoopInitialDeclarationIsInvalid,
                    )));
                }
            }

            expect!(self, Tag::Semicolon);
            Ok(decl)
        } else {
            let init_expr = self.expr(0)?;
            let init = self.alloc(AstKind::ExprStmt { expr: init_expr });
            expect!(self, Tag::Semicolon);
            Ok(init)
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn for_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::For);
        expect!(self, Tag::LeftParen);

        self.open_scope(ScopeKind::Loop);

        let init: Option<AstId> = if accept!(self, Tag::Semicolon) {
            None
        } else {
            Some(self.for_init()?)
        };

        let cond: Option<AstId> = if accept!(self, Tag::Semicolon) {
            None
        } else {
            let c = Some(self.expr(0)?);
            expect!(self, Tag::Semicolon);
            c
        };

        let post: Option<AstId> = if peek!(self, Tag::RightParen) {
            None
        } else {
            let post_expr = self.expr(0)?;
            Some(self.alloc(AstKind::ExprStmt { expr: post_expr }))
        };

        expect!(self, Tag::RightParen);

        let body = self.statement()?;

        self.close_scope();

        Ok(self.alloc(AstKind::For {
            init: init,
            cond: cond,
            post: post,
            body: body,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn switch_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::Switch);
        expect!(self, Tag::LeftParen);
        let expr = self.expr(0)?;
        expect!(self, Tag::RightParen);

        self.cases.push(vec![]);
        self.open_scope(ScopeKind::Switch);
        let stmt = self.statement()?;
        self.close_scope();

        let cases = self.cases.pop().unwrap();

        Ok(self.alloc(AstKind::Switch {
            cond: expr,
            body: stmt,
            cases: cases,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn break_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::Break);
        expect!(self, Tag::Semicolon);

        Ok(self.alloc(AstKind::Break { to: None }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn continue_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::Continue);
        expect!(self, Tag::Semicolon);

        Ok(self.alloc(AstKind::Continue { to: None }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn labelled_stmt(&mut self) -> Result<AstId, Error> {
        if accept!(self, Tag::Identifier) {
            let label = self.tokens[0].as_ref().unwrap().clone();
            expect!(self, Tag::Colon);
            let stmt = self.statement()?;
            let l = self.symtab.add_label(
                self.scope,
                &label.as_str(self.buf),
                stmt,
            );

            if l.is_err() {
                return Err(error(Parsing(ParsingError::LabelAlreadyDefined(
                    label.to_string(self.buf),
                ))));
            }

            Ok(self.alloc(AstKind::Label {
                name: label,
                stmt: stmt,
            }))
        } else if accept!(self, Tag::Case) {
            let expr = self.expr(0)?;
            expect!(self, Tag::Colon);
            let stmt = self.statement()?;
            let case_stmt = self.alloc(AstKind::Case {
                expr: expr,
                stmt: stmt,
                idx: 0,
            });

            if let Some(cases) = self.cases.last_mut() {
                cases.push(case_stmt);
            }
            Ok(case_stmt)
        } else {
            expect!(self, Tag::Default);
            expect!(self, Tag::Colon);
            let stmt = self.statement()?;
            let dflt_stmt = self.alloc(AstKind::Default { stmt: stmt });
            if let Some(cases) = self.cases.last_mut() {
                cases.push(dflt_stmt);
            }
            Ok(dflt_stmt)
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn block(&mut self) -> Result<AstId, Error> {
        let mut body: Vec<AstId> = vec![];
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

        Ok(self.alloc(AstKind::Block { body: body }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn function_body(&mut self) -> Result<AstId, Error> {
        let mut body: Vec<AstId> = vec![];

        expect!(self, Tag::LeftBrace);

        while !accept!(self, Tag::RightBrace) {
            if self.is_declspec() {
                body.extend(self.declaration()?);
            } else {
                body.push(self.statement()?);
            }
        }

        Ok(self.alloc(AstKind::Block { body: body }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn if_stmt(&mut self) -> Result<AstId, Error> {
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

        Ok(self.alloc(AstKind::If {
            cond: cond,
            then: then,
            otherwise: otherwise,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn goto_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::GoTo);
        expect!(self, Tag::Identifier);
        let label = self.tokens[0].as_ref().unwrap().clone();
        expect!(self, Tag::Semicolon);

        Ok(self.alloc(AstKind::GoTo { label: label }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn return_stmt(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::Return);
        let expr = self.expr(0)?;
        expect!(self, Tag::Semicolon);
        Ok(self.alloc(AstKind::Return { expr: expr }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn expr_stmt(&mut self) -> Result<AstId, Error> {
        let expr = self.expr(0)?;
        let stmt = self.alloc(AstKind::ExprStmt { expr: expr });
        expect!(self, Tag::Semicolon);
        Ok(stmt)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn conditional(
        &mut self,
        expr: AstId,
        min_prec: i32,
    ) -> Result<AstId, Error> {
        let middle = self.expr(0)?;
        expect!(self, Tag::Colon);
        let right = self.expr(min_prec)?;

        Ok(self.alloc(AstKind::Ternary {
            left: expr,
            middle: middle,
            right: right,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn argument_list(&mut self) -> Result<Vec<AstId>, Error> {
        let mut args: Vec<AstId> = vec![];

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
    fn call(&mut self, expr: AstId) -> Result<AstId, Error> {
        let args = self.argument_list()?;

        Ok(self.alloc(AstKind::Call {
            expr: expr,
            args: args,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn subscript(&mut self, expr: AstId) -> Result<AstId, Error> {
        expect!(self, Tag::LeftBracket);

        let idx_expr = self.expr(0)?;

        expect!(self, Tag::RightBracket);

        Ok(self.alloc(AstKind::Subscript {
            left: expr,
            right: idx_expr,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn expr(&mut self, min_prec: i32) -> Result<AstId, Error> {
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
    fn binop(&mut self, left: AstId, prec: i32) -> Result<AstId, Error> {
        if accept!(self, Tag::Asterisk) {
            let right = self.expr(prec + 1)?;
            Ok(self.alloc(AstKind::Multiply {
                left: left,
                right: right,
            }))
        } else if accept!(self, Tag::ForwardSlash) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Divide {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Percent) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Modulo {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Plus) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Add {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Minus) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Subtract {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::LeftShift) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::LeftShift {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::RightShift) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::RightShift {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Ampersand) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::And {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Bar) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Or {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Caret) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Xor {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::LAnd) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::LogicAnd {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::LOr) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::LogicOr {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Eq) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Equal {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::NotEq) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::NotEq {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Less) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Less {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::LessOrEq) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::LessOrEq {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::Greater) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::Greater {
                left: left,
                right: right,
            }));
        } else if accept!(self, Tag::GreaterOrEq) {
            let right = self.expr(prec + 1)?;
            return Ok(self.alloc(AstKind::GreaterOrEq {
                left: left,
                right: right,
            }));
        } else {
            return Err(error(Parsing(
                ParsingError::MalformedBinaryExpression,
            )));
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn assignment(
        &mut self,
        left: AstId,
        right: AstId,
    ) -> Result<AstId, Error> {
        Ok(self.alloc(AstKind::Assign {
            left: left,
            right: right,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn addr_of(&mut self, expr: AstId) -> Result<AstId, Error> {
        Ok(self.alloc(AstKind::AddrOf { expr: expr }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn deref(&mut self, expr: AstId) -> Result<AstId, Error> {
        Ok(self.alloc(AstKind::Deref { expr: expr }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn pre_incr(&mut self, expr: AstId) -> Result<AstId, Error> {
        Ok(self.alloc(AstKind::PreIncr { expr: expr }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn pre_decr(&mut self, expr: AstId) -> Result<AstId, Error> {
        Ok(self.alloc(AstKind::PreDecr { expr: expr }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn plus_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::Add {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn minus_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::Subtract {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn mult_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::Multiply {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn div_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::Divide {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn mod_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::Modulo {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn and_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::And {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn or_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::Or {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn xor_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::Xor {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn lshift_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::LeftShift {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn rshift_eq(&mut self, left: AstId, right: AstId) -> Result<AstId, Error> {
        let cloned = self.arena.alloc_clone(left);
        let inner = self.alloc(AstKind::RightShift {
            left: cloned,
            right: right,
        });
        Ok(self.alloc(AstKind::CompoundAssign {
            left: left,
            right: inner,
        }))
    }

    fn peek_postfix_op(&self) -> bool {
        match peek_tag!(self) {
            Tag::Incr | Tag::Decr | Tag::LeftParen | Tag::LeftBracket => true,
            _ => false,
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn postfix(&mut self, mut expr: AstId) -> Result<AstId, Error> {
        while self.peek_postfix_op() {
            if accept!(self, Tag::LeftParen) {
                expr = self.call(expr)?;
            } else if peek!(self, Tag::LeftBracket) {
                expr = self.subscript(expr)?;
            } else if accept!(self, Tag::Incr) {
                expr = self.alloc(AstKind::PostIncr { expr: expr });
            } else if accept!(self, Tag::Decr) {
                expr = self.alloc(AstKind::PostDecr { expr: expr });
            } else {
                return Err(error(Parsing(
                    ParsingError::InvalidPostfixExpression,
                )));
            }
        }

        Ok(expr)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn factor(&mut self) -> Result<AstId, Error> {
        let mut expr = if peek!(self, Tag::ConstInt, _) {
            self.const_int()?
        } else if peek!(self, Tag::ConstUnsignedInt, _) {
            self.const_unsigned_int()?
        } else if peek!(self, Tag::ConstLong, _) {
            self.const_long()?
        } else if peek!(self, Tag::ConstLongLong, _) {
            self.const_long_long()?
        } else if peek!(self, Tag::ConstUnsignedLong, _) {
            self.const_unsigned_long()?
        } else if peek!(self, Tag::ConstUnsignedLongLong, _) {
            self.const_unsigned_long_long()?
        } else if peek!(self, Tag::ConstDouble, _) {
            self.const_double()?
        } else if accept!(self, Tag::Tilde) {
            let subexpr = self.factor()?;
            return Ok(self.alloc(AstKind::Complement { expr: subexpr }));
        } else if accept!(self, Tag::Minus) {
            let subexpr = self.factor()?;
            return Ok(self.alloc(AstKind::Negate { expr: subexpr }));
        } else if accept!(self, Tag::Bang) {
            let subexpr = self.factor()?;
            return Ok(self.alloc(AstKind::Not { expr: subexpr }));
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

            let inner_expr = self.expr(0)?;
            expect!(self, Tag::RightParen);
            inner_expr
        } else if accept!(self, Tag::Identifier) {
            let name = self.tokens[0].as_ref().unwrap().clone();
            let sym = self.symtab.get_sym(self.scope, &name.as_str(self.buf));

            self.alloc(AstKind::Identifier {
                name: name.clone(),
                sym: if let Some(s) = sym {
                    Some(s)
                } else {
                    return Err(error(Parsing(
                        ParsingError::UndeclaredIdentifier(
                            name.to_string(self.buf),
                        ),
                    )));
                },
            })
        } else {
            return Err(error(Parsing(ParsingError::MalformedExpression)));
        };

        while self.peek_postfix_op() {
            expr = self.postfix(expr)?;
        }

        Ok(expr)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn cast_expr(&mut self) -> Result<AstId, Error> {
        let type_spec = self.type_name()?;
        expect!(self, Tag::RightParen);

        let expr = self.factor()?;

        Ok(self.alloc(AstKind::Cast {
            type_spec: Some(type_spec),
            expr: expr,
        }))
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_int(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::ConstInt, _);
        let value = yank!(self, Tag::ConstInt);
        let node = self.alloc(AstKind::ConstInt(value as i32));
        self.arena[node].ty = int_type(true);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_unsigned_int(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::ConstUnsignedInt, _);
        let value = yank!(self, Tag::ConstUnsignedInt);
        let node = self.alloc(AstKind::ConstUnsignedInt(value as u32));
        self.arena[node].ty = int_type(false);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_long(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::ConstLong, _);
        let value = yank!(self, Tag::ConstLong);
        let node = self.alloc(AstKind::ConstLong(value));
        self.arena[node].ty = long_type(true);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_long_long(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::ConstLongLong, _);
        let value = yank!(self, Tag::ConstLongLong);
        let node = self.alloc(AstKind::ConstLong(value));
        self.arena[node].ty = long_type(true);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_unsigned_long_long(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::ConstUnsignedLongLong, _);
        let value = yank!(self, Tag::ConstUnsignedLongLong);
        let node = self.alloc(AstKind::ConstUnsignedLong(value));
        self.arena[node].ty = long_type(false);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_unsigned_long(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::ConstUnsignedLong, _);
        let value = yank!(self, Tag::ConstUnsignedLong);
        let node = self.alloc(AstKind::ConstUnsignedLong(value));
        self.arena[node].ty = long_type(false);

        Ok(node)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(skip_all))]
    fn const_double(&mut self) -> Result<AstId, Error> {
        expect!(self, Tag::ConstDouble, _);
        let value = yank!(self, Tag::ConstDouble);
        let node = self.alloc(AstKind::ConstDouble(value));
        self.arena[node].ty = double_type();

        Ok(node)
    }

    fn eof(&self) -> bool {
        self.tokens[1].is_none()
    }

    fn preload(&mut self) -> Result<(), Error> {
        for _ in 0..2 {
            for i in 0..2 {
                self.tokens[i] = self.tokens[i + 1].clone();
            }
            self.pull_token()?;
        }

        Ok(())
    }

    fn pull_token(&mut self) -> Result<(), Error> {
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

    fn advance(&mut self) -> Result<(), Error> {
        if self.tokens[1].is_none() {
            panic!("attempt to read past end of file");
        }

        for i in 0..2 {
            self.tokens[i] = self.tokens[i + 1].clone();
        }

        self.pull_token()
    }
}
