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
use std::fmt;

use crate::errors::{Error, ErrorClass, error, error_at};
use crate::symtab::*;
use crate::tokenising::Token;
use crate::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AstId(pub usize);

#[derive(Clone)]
pub struct Ast {
    pub ty: TypeRef,
    pub kind: AstKind,
    pub scope: ScopeId,
}

impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ast")
            .field("ty", &self.ty)
            .field("kind", &self.kind)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct CompoundInitialiser {
    pub type_spec: AstId,
    pub initialisers: Vec<AstId>,
    pub max_initialisers: Option<usize>,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum AstKind {
    Initialiser {
        type_spec: AstId,
        value: Option<AstId>,
    },
    CompoundInitialiser(CompoundInitialiser),
    ConstInt(i32),
    ConstUnsignedInt(u32),
    ConstLong(i64),
    ConstUnsignedLong(u64),
    ConstDouble(f64),
    Void,
    Int,
    Double,
    Pointer {
        base_type_spec: AstId,
        qualifiers: Vec<String>,
    },
    Function {
        name: Option<Token>,
        sym: Option<SymId>,
        params: Vec<AstId>,
        block: Option<AstId>,
        type_spec: AstId,
    },
    Block {
        body: Vec<AstId>,
    },
    Variable {
        name: Token,
        sym: Option<SymId>,
        type_spec: AstId,
        init: Option<AstId>,
    },
    Parameter {
        name: Token,
        sym: Option<SymId>,
        type_spec: AstId,
    },
    Array {
        type_spec: AstId,
        dimension: Option<AstId>,
        len: Cell<usize>,
    },
    Identifier {
        name: Token,
        sym: Option<SymId>,
    },
    Return {
        expr: AstId,
    },
    If {
        cond: AstId,
        then: AstId,
        otherwise: Option<AstId>,
    },
    Break {
        to: Option<AstId>,
    },
    Continue {
        to: Option<AstId>,
    },
    While {
        cond: AstId,
        body: AstId,
    },
    DoWhile {
        cond: AstId,
        body: AstId,
    },
    For {
        init: Option<AstId>,
        cond: Option<AstId>,
        post: Option<AstId>,
        body: AstId,
    },
    Switch {
        cond: AstId,
        body: AstId,
        cases: Vec<AstId>,
    },
    ExprStmt {
        expr: AstId,
    },
    GoTo {
        label: Token,
    },
    Label {
        name: Token,
        stmt: AstId,
    },
    Case {
        expr: AstId,
        stmt: AstId,
        idx: usize,
    },
    Default {
        stmt: AstId,
    },
    EmptyStmt,
    Ternary {
        left: AstId,
        middle: AstId,
        right: AstId,
    },
    Complement {
        expr: AstId,
    },
    Negate {
        expr: AstId,
    },
    Not {
        expr: AstId,
    },
    PreIncr {
        expr: AstId,
    },
    PreDecr {
        expr: AstId,
    },
    PostIncr {
        expr: AstId,
    },
    PostDecr {
        expr: AstId,
    },
    Add {
        left: AstId,
        right: AstId,
    },
    Subtract {
        left: AstId,
        right: AstId,
    },
    Multiply {
        left: AstId,
        right: AstId,
    },
    Divide {
        left: AstId,
        right: AstId,
    },
    Modulo {
        left: AstId,
        right: AstId,
    },
    LeftShift {
        left: AstId,
        right: AstId,
    },
    RightShift {
        left: AstId,
        right: AstId,
    },
    And {
        left: AstId,
        right: AstId,
    },
    Or {
        left: AstId,
        right: AstId,
    },
    Xor {
        left: AstId,
        right: AstId,
    },
    LogicAnd {
        left: AstId,
        right: AstId,
    },
    LogicOr {
        left: AstId,
        right: AstId,
    },
    Equal {
        left: AstId,
        right: AstId,
    },
    NotEq {
        left: AstId,
        right: AstId,
    },
    Less {
        left: AstId,
        right: AstId,
    },
    LessOrEq {
        left: AstId,
        right: AstId,
    },
    Greater {
        left: AstId,
        right: AstId,
    },
    GreaterOrEq {
        left: AstId,
        right: AstId,
    },
    Assign {
        left: AstId,
        right: AstId,
    },
    CompoundAssign {
        left: AstId,
        right: AstId,
    },
    Call {
        expr: AstId,
        args: Vec<AstId>,
    },
    Subscript {
        left: AstId,
        right: AstId,
    },
    Cast {
        type_spec: Option<AstId>,
        expr: AstId,
    },
    AddrOf {
        expr: AstId,
    },
    Deref {
        expr: AstId,
    },
}

#[derive(Debug, Clone, Default)]
pub struct AstArena {
    pub arena: Vec<Ast>,
    pub source: String,
}

#[derive(Debug, Clone)]
pub struct AstStage {
    pub arena: AstArena,
    pub root: Vec<AstId>,
    pub symtab: SymTab,
}

impl AstArena {
    pub fn new() -> Self {
        AstArena {
            arena: Vec::new(),
            source: String::new(),
        }
    }

    pub fn token_str(&self, token: &Token) -> &str {
        &self.source[token.loc.start..token.loc.end]
    }

    pub fn alloc(
        &mut self,
        kind: AstKind,
        ty: Option<TypeRef>,
        scope: ScopeId,
    ) -> AstId {
        let id = AstId(self.arena.len());
        self.arena.push(Ast {
            ty: ty.unwrap_or_else(undefined_type),
            kind,
            scope,
        });
        id
    }

    pub fn alloc_clone(&mut self, node: AstId) -> AstId {
        let id = AstId(self.arena.len());
        self.arena.push(self.arena[node.0].clone());
        id
    }
}

impl std::ops::Index<AstId> for AstArena {
    type Output = Ast;

    fn index(&self, id: AstId) -> &Ast {
        &self.arena[id.0]
    }
}

impl std::ops::IndexMut<AstId> for AstArena {
    fn index_mut(&mut self, id: AstId) -> &mut Ast {
        &mut self.arena[id.0]
    }
}

impl AstArena {
    pub fn key_token(&self, id: AstId) -> Option<Token> {
        let kind = &self[id].kind;
        match kind {
            AstKind::Function { name, .. } => *name,
            AstKind::Variable { name, .. } => Some(*name),
            AstKind::Parameter { name, .. } => Some(*name),
            AstKind::Identifier { name, .. } => Some(*name),
            AstKind::GoTo { label } => Some(*label),
            AstKind::Label { name, .. } => Some(*name),
            AstKind::Return { expr } => self.key_token(*expr),
            AstKind::If { cond, .. } => self.key_token(*cond),
            AstKind::While { cond, .. } => self.key_token(*cond),
            AstKind::DoWhile { cond, .. } => self.key_token(*cond),
            AstKind::For { init, cond, .. } => init
                .and_then(|i| self.key_token(i))
                .or_else(|| cond.and_then(|c| self.key_token(c))),
            AstKind::Switch { cond, .. } => self.key_token(*cond),
            AstKind::ExprStmt { expr } => self.key_token(*expr),
            AstKind::Case { expr, .. } => self.key_token(*expr),
            AstKind::Default { stmt } => self.key_token(*stmt),
            AstKind::Block { body } => {
                body.first().and_then(|&id| self.key_token(id))
            }
            AstKind::Ternary { left, .. } => self.key_token(*left),
            AstKind::Assign { left, .. } => self.key_token(*left),
            AstKind::CompoundAssign { left, .. } => self.key_token(*left),
            AstKind::LogicAnd { left, .. } => self.key_token(*left),
            AstKind::LogicOr { left, .. } => self.key_token(*left),
            AstKind::Equal { left, .. } => self.key_token(*left),
            AstKind::NotEq { left, .. } => self.key_token(*left),
            AstKind::Less { left, .. } => self.key_token(*left),
            AstKind::LessOrEq { left, .. } => self.key_token(*left),
            AstKind::Greater { left, .. } => self.key_token(*left),
            AstKind::GreaterOrEq { left, .. } => self.key_token(*left),
            AstKind::Add { left, .. } => self.key_token(*left),
            AstKind::Subtract { left, .. } => self.key_token(*left),
            AstKind::Multiply { left, .. } => self.key_token(*left),
            AstKind::Divide { left, .. } => self.key_token(*left),
            AstKind::Modulo { left, .. } => self.key_token(*left),
            AstKind::And { left, .. } => self.key_token(*left),
            AstKind::Or { left, .. } => self.key_token(*left),
            AstKind::Xor { left, .. } => self.key_token(*left),
            AstKind::LeftShift { left, .. } => self.key_token(*left),
            AstKind::RightShift { left, .. } => self.key_token(*left),
            AstKind::Subscript { left, .. } => self.key_token(*left),
            AstKind::Call { expr, .. } => self.key_token(*expr),
            AstKind::Cast { expr, .. } => self.key_token(*expr),
            AstKind::Complement { expr } => self.key_token(*expr),
            AstKind::Negate { expr } => self.key_token(*expr),
            AstKind::Not { expr } => self.key_token(*expr),
            AstKind::AddrOf { expr } => self.key_token(*expr),
            AstKind::Deref { expr } => self.key_token(*expr),
            AstKind::PreIncr { expr } => self.key_token(*expr),
            AstKind::PreDecr { expr } => self.key_token(*expr),
            AstKind::PostIncr { expr } => self.key_token(*expr),
            AstKind::PostDecr { expr } => self.key_token(*expr),
            AstKind::Initialiser { value, .. } => {
                value.and_then(|v| self.key_token(v))
            }
            AstKind::CompoundInitialiser(inner) => {
                self.key_token(inner.type_spec)
            }
            AstKind::Array { type_spec, .. } => self.key_token(*type_spec),
            AstKind::Pointer { base_type_spec, .. } => {
                self.key_token(*base_type_spec)
            }
            _ => None,
        }
    }

    pub fn node_error(&self, id: AstId, class: ErrorClass) -> Error {
        match self.key_token(id) {
            Some(token) => error_at(token.loc, class),
            None => error(class),
        }
    }
}
