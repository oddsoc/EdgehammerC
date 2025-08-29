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

use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::scope::*;
use crate::types::*;

pub type AstRef = Rc<RefCell<Ast>>;
pub type AstWeakRef = Weak<RefCell<Ast>>;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Ast {
    pub id: usize,
    pub ty: TypeRef,
    pub kind: AstKind,
    pub scope: ScopeRef,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum AstKind {
    StaticInitializer(AstRef),
    Initializer(AstRef),
    ConstInt(i32),
    ConstUnsignedInt(u32),
    ConstLong(i64),
    ConstUnsignedLong(u64),
    Void,
    Int,
    Function {
        name: String,
        sym: Option<SymWeakRef>,
        params: Vec<AstRef>,
        block: Option<AstRef>,
        type_spec: AstRef,
        scope: ScopeRef,
    },
    Parameter {
        name: Option<String>,
        sym: Option<SymWeakRef>,
        idx: usize,
        type_spec: AstRef,
    },
    Block {
        body: Vec<AstRef>,
    },
    Variable {
        name: String,
        sym: Option<SymWeakRef>,
        type_spec: AstRef,
        init: Option<AstRef>,
    },
    Identifier {
        name: String,
        sym: Option<SymWeakRef>,
    },
    Return {
        expr: AstRef,
        func: SymWeakRef,
    },
    If {
        cond: AstRef,
        then: AstRef,
        otherwise: Option<AstRef>,
    },
    Break {
        to: Option<AstRef>,
    },
    Continue {
        to: Option<AstRef>,
    },
    While {
        cond: AstRef,
        body: AstRef,
    },
    DoWhile {
        cond: AstRef,
        body: AstRef,
    },
    For {
        init: Option<AstRef>,
        cond: Option<AstRef>,
        post: Option<AstRef>,
        body: AstRef,
    },
    Switch {
        cond: AstRef,
        body: AstRef,
        cases: Vec<AstRef>,
    },
    ExprStmt {
        expr: AstRef,
    },
    GoTo {
        label: String,
    },
    Label {
        name: String,
        stmt: AstRef,
    },
    Case {
        expr: AstRef,
        stmt: AstRef,
        idx: usize,
    },
    Default {
        stmt: AstRef,
    },
    EmptyStmt,
    Ternary {
        left: AstRef,
        middle: AstRef,
        right: AstRef,
    },
    Complement {
        expr: AstRef,
    },
    Negate {
        expr: AstRef,
    },
    Not {
        expr: AstRef,
    },
    PostIncr {
        expr: AstRef,
    },
    PostDecr {
        expr: AstRef,
    },
    Add {
        left: AstRef,
        right: AstRef,
    },
    Subtract {
        left: AstRef,
        right: AstRef,
    },
    Multiply {
        left: AstRef,
        right: AstRef,
    },
    Divide {
        left: AstRef,
        right: AstRef,
    },
    Modulo {
        left: AstRef,
        right: AstRef,
    },
    LShift {
        left: AstRef,
        right: AstRef,
    },
    RShift {
        left: AstRef,
        right: AstRef,
    },
    And {
        left: AstRef,
        right: AstRef,
    },
    Or {
        left: AstRef,
        right: AstRef,
    },
    Xor {
        left: AstRef,
        right: AstRef,
    },
    LogicAnd {
        left: AstRef,
        right: AstRef,
    },
    LogicOr {
        left: AstRef,
        right: AstRef,
    },
    Equal {
        left: AstRef,
        right: AstRef,
    },
    NotEq {
        left: AstRef,
        right: AstRef,
    },
    LessThan {
        left: AstRef,
        right: AstRef,
    },
    LessOrEq {
        left: AstRef,
        right: AstRef,
    },
    GreaterThan {
        left: AstRef,
        right: AstRef,
    },
    GreaterOrEq {
        left: AstRef,
        right: AstRef,
    },
    Assign {
        left: AstRef,
        right: AstRef,
    },
    Call {
        expr: AstRef,
        args: Vec<AstRef>,
    },
    Cast {
        type_spec: Option<AstRef>,
        expr: AstRef,
    },
}

impl Ast {
    pub fn new(
        id: usize,
        kind: AstKind,
        ty: Option<TypeRef>,
        scope: ScopeRef,
    ) -> AstRef {
        Rc::new(RefCell::new(Ast {
            id,
            ty: if let Some(t) = ty {
                t.clone()
            } else {
                undefined_type()
            },
            kind,
            scope,
        }))
    }
}

pub fn deep_clone(node: &AstRef) -> AstRef {
    Rc::new(RefCell::new(node.borrow().clone()))
}

pub fn replace(node: &AstRef, with: &AstRef) {
    if !Rc::ptr_eq(node, with) {
        *node.borrow_mut() = with.borrow().clone();
    }
}
