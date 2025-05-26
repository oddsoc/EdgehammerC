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

use crate::scope::ScopeRef;
use std::rc::Rc;

#[derive(Debug)]
pub struct AST {
    pub id: usize,
    pub kind: ASTKind,
    pub scope: ScopeRef,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum ASTKind {
    ConstInt(i64),
    Int,
    Function {
        name: String,
        params: Vec<Rc<AST>>,
        block: Option<Rc<AST>>,
        rtype: Rc<AST>,
        scope: ScopeRef,
    },
    Block {
        body: Vec<Rc<AST>>,
    },
    Variable {
        name: String,
        typ: Rc<AST>,
        init: Option<Rc<AST>>,
    },
    Identifier {
        name: String,
        sym: Option<(usize, Rc<AST>)>,
    },
    Return {
        expr: Rc<AST>,
    },
    If {
        cond: Rc<AST>,
        then: Rc<AST>,
        otherwise: Option<Rc<AST>>,
    },
    Break {
        to: Option<Rc<AST>>,
    },
    Continue {
        to: Option<Rc<AST>>,
    },
    While {
        cond: Rc<AST>,
        body: Rc<AST>,
    },
    DoWhile {
        cond: Rc<AST>,
        body: Rc<AST>,
    },
    For {
        init: Option<Rc<AST>>,
        cond: Option<Rc<AST>>,
        post: Option<Rc<AST>>,
        body: Rc<AST>,
    },
    Switch {
        cond: Rc<AST>,
        body: Rc<AST>,
        cases: Vec<Rc<AST>>,
    },
    ExprStmt {
        expr: Rc<AST>,
    },
    GoTo {
        label: String,
    },
    Label {
        name: String,
        stmt: Rc<AST>,
    },
    Case {
        expr: Rc<AST>,
        stmt: Rc<AST>,
        idx: usize,
    },
    Default {
        stmt: Rc<AST>,
    },
    EmptyStmt,
    Conditional {
        left: Rc<AST>,
        middle: Rc<AST>,
        right: Rc<AST>,
    },
    Complement {
        expr: Rc<AST>,
    },
    Negate {
        expr: Rc<AST>,
    },
    Not {
        expr: Rc<AST>,
    },
    PreIncr {
        expr: Rc<AST>,
    },
    PreDecr {
        expr: Rc<AST>,
    },
    PostIncr {
        expr: Rc<AST>,
    },
    PostDecr {
        expr: Rc<AST>,
    },
    Add {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Subtract {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Multiply {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Divide {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Modulo {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    LShift {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    RShift {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    And {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Or {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Xor {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    LogicAnd {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    LogicOr {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Equal {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    NotEq {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    LessThan {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    LessOrEq {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    GreaterThan {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    GreaterOrEq {
        left: Rc<AST>,
        right: Rc<AST>,
    },
    Assign {
        left: Rc<AST>,
        right: Rc<AST>,
    },
}
