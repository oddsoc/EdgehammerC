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

use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::scope::*;
use crate::types::*;

pub type ASTRef = Rc<RefCell<AST>>;

#[derive(Debug)]
#[allow(dead_code)]
pub struct AST {
    pub id: usize,
    pub ty: TypeRef,
    pub kind: ASTKind,
    pub scope: ScopeRef,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum ASTKind {
    ConstInt(i64),
    Void,
    Int,
    Function {
        name: String,
        params: Vec<ASTRef>,
        block: Option<ASTRef>,
        ty: ASTRef,
        scope: ScopeRef,
    },
    Parameter {
        name: Option<String>,
        idx: usize,
        ty: ASTRef,
    },
    Block {
        body: Vec<ASTRef>,
    },
    Variable {
        name: String,
        ty: ASTRef,
        init: Option<ASTRef>,
    },
    Identifier {
        name: String,
        sym: Option<Weak<RefCell<Symbol>>>,
    },
    Return {
        expr: ASTRef,
    },
    If {
        cond: ASTRef,
        then: ASTRef,
        otherwise: Option<ASTRef>,
    },
    Break {
        to: Option<ASTRef>,
    },
    Continue {
        to: Option<ASTRef>,
    },
    While {
        cond: ASTRef,
        body: ASTRef,
    },
    DoWhile {
        cond: ASTRef,
        body: ASTRef,
    },
    For {
        init: Option<ASTRef>,
        cond: Option<ASTRef>,
        post: Option<ASTRef>,
        body: ASTRef,
    },
    Switch {
        cond: ASTRef,
        body: ASTRef,
        cases: Vec<ASTRef>,
    },
    ExprStmt {
        expr: ASTRef,
    },
    GoTo {
        label: String,
    },
    Label {
        name: String,
        stmt: ASTRef,
    },
    Case {
        expr: ASTRef,
        stmt: ASTRef,
        idx: usize,
    },
    Default {
        stmt: ASTRef,
    },
    EmptyStmt,
    Conditional {
        left: ASTRef,
        middle: ASTRef,
        right: ASTRef,
    },
    Complement {
        expr: ASTRef,
    },
    Negate {
        expr: ASTRef,
    },
    Not {
        expr: ASTRef,
    },
    PreIncr {
        expr: ASTRef,
    },
    PreDecr {
        expr: ASTRef,
    },
    PostIncr {
        expr: ASTRef,
    },
    PostDecr {
        expr: ASTRef,
    },
    Add {
        left: ASTRef,
        right: ASTRef,
    },
    Subtract {
        left: ASTRef,
        right: ASTRef,
    },
    Multiply {
        left: ASTRef,
        right: ASTRef,
    },
    Divide {
        left: ASTRef,
        right: ASTRef,
    },
    Modulo {
        left: ASTRef,
        right: ASTRef,
    },
    LShift {
        left: ASTRef,
        right: ASTRef,
    },
    RShift {
        left: ASTRef,
        right: ASTRef,
    },
    And {
        left: ASTRef,
        right: ASTRef,
    },
    Or {
        left: ASTRef,
        right: ASTRef,
    },
    Xor {
        left: ASTRef,
        right: ASTRef,
    },
    LogicAnd {
        left: ASTRef,
        right: ASTRef,
    },
    LogicOr {
        left: ASTRef,
        right: ASTRef,
    },
    Equal {
        left: ASTRef,
        right: ASTRef,
    },
    NotEq {
        left: ASTRef,
        right: ASTRef,
    },
    LessThan {
        left: ASTRef,
        right: ASTRef,
    },
    LessOrEq {
        left: ASTRef,
        right: ASTRef,
    },
    GreaterThan {
        left: ASTRef,
        right: ASTRef,
    },
    GreaterOrEq {
        left: ASTRef,
        right: ASTRef,
    },
    Assign {
        left: ASTRef,
        right: ASTRef,
    },
    Call {
        expr: ASTRef,
        args: Vec<ASTRef>,
    },
}

impl AST {
    pub fn new(
        id: usize,
        kind: ASTKind,
        ty: Option<TypeRef>,
        scope: ScopeRef,
    ) -> ASTRef {
        Rc::new(RefCell::new(AST {
            id: id,
            ty: if ty.is_some() {
                ty.unwrap().clone()
            } else {
                undefined_type()
            },
            kind: kind,
            scope: scope,
        }))
    }

    pub fn preceeds(&self, other: &AST) -> bool {
        self.id < other.id
    }

    pub fn resolve(&self) -> Option<SymRef> {
        if let ASTKind::Identifier { sym, .. } = &self.kind {
            if let Some(sym_ref) = sym {
                if let Some(sym_rc) = sym_ref.upgrade() {
                    return Some(sym_rc);
                }
            }
        }

        None
    }

    pub fn resolve_node(&self) -> Option<ASTRef> {
        if let Some(sym) = self.resolve() {
            if let Some(node) = &sym.borrow().node {
                return node.upgrade();
            }
        }

        None
    }
}

/*
impl Drop for AST {
    fn drop(&mut self) {
        println!("Dropping AST: {:?}", self);
    }
}
*/
