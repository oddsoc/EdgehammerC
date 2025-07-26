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
use std::rc::Rc;

use crate::ast::*;
use crate::scope::*;

pub type TypeRef = Rc<RefCell<Type>>;

const IS_SIGNED: u8 = 1;
const IS_SCALAR: u8 = 1 << 1;

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub basetype: Option<TypeRef>,
    pub alignment: usize,
    pub size: usize,
    pub flags: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Undefined,
    Void,
    Int,
    Function {
        param_tys: Vec<TypeRef>,
        return_ty: TypeRef,
    },
}

pub fn undefined_type() -> TypeRef {
    Rc::new(RefCell::new(Type {
        kind: TypeKind::Undefined,
        basetype: None,
        alignment: 0,
        size: 0,
        flags: 0,
    }))
}

pub fn int_type() -> TypeRef {
    Rc::new(RefCell::new(Type {
        kind: TypeKind::Int,
        basetype: None,
        alignment: 4,
        size: 4,
        flags: IS_SIGNED | IS_SCALAR,
    }))
}

pub fn is_signed(ty: &TypeRef) -> bool {
    ty.borrow().flags & IS_SIGNED != 0
}

pub fn void_type() -> TypeRef {
    Rc::new(RefCell::new(Type {
        kind: TypeKind::Void,
        basetype: None,
        alignment: 1,
        size: 1,
        flags: 0,
    }))
}

fn is_compatible(ty0: &TypeRef, ty1: &TypeRef) -> bool {
    *ty0.borrow() == *ty1.borrow()
}

fn is_int_type(ty: &TypeRef) -> bool {
    match ty.borrow().kind {
        TypeKind::Int => true,
        _ => false,
    }
}

fn is_scalar_type(ty: &TypeRef) -> bool {
    ty.borrow().flags & IS_SCALAR != 0
}

pub struct Annotator;

impl Annotator {
    pub fn new() -> Annotator {
        Annotator {}
    }

    fn annotate_decls(
        &self,
        decls: &Vec<SymRef>,
        ty: TypeRef,
    ) -> Result<TypeRef, String> {
        for decl in decls {
            if let Some(node) = sym_as_node(decl.clone()) {
                let decl_ty = self.annotate(&node)?;

                if !is_compatible(&ty, &decl_ty) {
                    return Err("incompatible types".to_string());
                }
            }
        }

        Ok(ty.clone())
    }

    pub fn annotate(&self, ast: &AstRef) -> Result<TypeRef, String> {
        let node = ast.as_ref().borrow();

        if !matches!(node.ty.borrow().kind, TypeKind::Undefined) {
            return Ok(node.ty.clone());
        }

        match &node.kind {
            AstKind::Function {
                name,
                sym: _,
                params,
                block,
                type_spec: rty_spec,
                ..
            } => {
                let mut param_tys: Vec<TypeRef> = vec![];

                for param in params {
                    param_tys.push(self.annotate(param)?);
                }

                let has_void = param_tys
                    .iter()
                    .any(|p| matches!(p.borrow().kind, TypeKind::Void));

                if has_void && param_tys.len() > 1 {
                    return Err("void must be the only parameter".to_string());
                } else if has_void {
                    param_tys.clear()
                }

                let return_ty = self.annotate(rty_spec)?;

                let ty = Rc::new(RefCell::new(Type {
                    kind: TypeKind::Function {
                        param_tys,
                        return_ty: return_ty.clone(),
                    },
                    basetype: None,
                    alignment: 8,
                    size: 1,
                    flags: 0,
                }));

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                if let Some(body) = block {
                    self.annotate(body)?;
                }

                if let Some(decls) =
                    get_all_named_sym_decls(node.scope.clone(), name)
                {
                    self.annotate_decls(&decls, ty.clone())?;
                }

                Ok(ty)
            }

            AstKind::Parameter {
                name: _,
                sym: _,
                idx: _,
                type_spec,
            } => {
                let ty = self.annotate(type_spec)?;
                *node.ty.borrow_mut() = (*ty.borrow()).clone();
                Ok(ty)
            }

            AstKind::Block { body } => {
                for stmt in body {
                    self.annotate(stmt)?;
                }

                Ok(undefined_type())
            }

            AstKind::Variable {
                type_spec, init, ..
            } => {
                let ty = self.annotate(type_spec)?;

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                if let Some(init) = init {
                    self.annotate(init)?;
                }

                Ok(ty)
            }

            AstKind::Return { expr } => {
                self.annotate(expr)?;
                Ok(undefined_type())
            }

            AstKind::If {
                cond,
                then,
                otherwise,
            } => {
                if !is_scalar_type(&self.annotate(cond)?) {
                    return Err("expected scalar type".to_string());
                }

                self.annotate(then)?;
                if let Some(otherwise) = otherwise {
                    self.annotate(otherwise)?;
                }
                Ok(undefined_type())
            }
            AstKind::DoWhile { cond, body } => {
                if !is_scalar_type(&self.annotate(cond)?) {
                    return Err("expected scalar type".to_string());
                }

                self.annotate(body)?;
                Ok(undefined_type())
            }
            AstKind::While { cond, body } => {
                if !is_scalar_type(&self.annotate(cond)?) {
                    return Err("expected scalar type".to_string());
                }

                self.annotate(body)?;
                Ok(undefined_type())
            }
            AstKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init) = init {
                    self.annotate(init)?;
                }

                if let Some(cond) = cond {
                    if !is_scalar_type(&self.annotate(cond)?) {
                        return Err("expected scalar type".to_string());
                    }
                }

                if let Some(post) = post {
                    self.annotate(post)?;
                }

                self.annotate(body)?;

                Ok(undefined_type())
            }

            AstKind::ExprStmt { expr } => {
                let _ = self.annotate(expr)?;
                Ok(undefined_type())
            }
            AstKind::GoTo { .. } => Ok(undefined_type()),

            AstKind::Label { stmt, .. } => {
                self.annotate(stmt)?;
                Ok(undefined_type())
            }

            AstKind::Case { expr, stmt, idx: _ } => {
                if !is_int_type(&self.annotate(expr)?) {
                    return Err(
                        "switch expression must be an integer".to_string()
                    );
                }
                self.annotate(stmt)?;
                Ok(undefined_type())
            }

            AstKind::Default { stmt } => {
                self.annotate(stmt)?;
                Ok(undefined_type())
            }

            AstKind::Switch {
                cond,
                body,
                cases: _,
            } => {
                if !is_int_type(&self.annotate(cond)?) {
                    return Err(
                        "switch expression must be an integer".to_string()
                    );
                }

                self.annotate(body)?;
                Ok(undefined_type())
            }

            AstKind::Void => {
                let ty = void_type();
                *node.ty.borrow_mut() = (*ty.borrow()).clone();
                Ok(ty)
            }

            AstKind::Int => {
                let ty = int_type();
                *node.ty.borrow_mut() = (*ty.borrow()).clone();
                Ok(ty)
            }

            AstKind::Identifier { .. } => {
                if let Some(sym) = resolve(ast) {
                    if let Some(identified) = sym_as_node(sym.clone()) {
                        let ty = self.annotate(&identified)?;
                        *node.ty.borrow_mut() = (*ty.borrow()).clone();
                        Ok(ty)
                    } else {
                        Ok(undefined_type())
                    }
                } else {
                    Ok(undefined_type())
                }
            }

            AstKind::Assign { left, right }
            | AstKind::Add { left, right }
            | AstKind::Subtract { left, right }
            | AstKind::Multiply { left, right }
            | AstKind::Divide { left, right }
            | AstKind::Modulo { left, right }
            | AstKind::LShift { left, right }
            | AstKind::RShift { left, right }
            | AstKind::And { left, right }
            | AstKind::Or { left, right }
            | AstKind::Xor { left, right }
            | AstKind::Equal { left, right }
            | AstKind::NotEq { left, right }
            | AstKind::LessThan { left, right }
            | AstKind::LessOrEq { left, right }
            | AstKind::GreaterThan { left, right }
            | AstKind::GreaterOrEq { left, right }
            | AstKind::LogicAnd { left, right }
            | AstKind::LogicOr { left, right } => {
                if !is_scalar_type(&self.annotate(right)?) {
                    return Err("expected scalar type".to_string());
                }

                let ty = self.annotate(left)?;

                if !is_scalar_type(&ty) {
                    return Err("expected scalar type".to_string());
                }

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }

            AstKind::Ternary {
                left,
                middle,
                right,
            } => {
                let left_ty = self.annotate(left)?;

                if !is_scalar_type(&left_ty) {
                    return Err("expected a scalar type".to_string());
                }

                let middle_ty = self.annotate(middle)?;

                if !is_scalar_type(&middle_ty) {
                    return Err("expected a scalar type".to_string());
                }

                let right_ty = self.annotate(right)?;

                if !is_scalar_type(&right_ty) {
                    return Err("expected a scalar type".to_string());
                }

                if !is_compatible(&middle_ty, &right_ty) {
                    return Err("incompatible types".to_string());
                }

                *node.ty.borrow_mut() = (*middle_ty.borrow()).clone();

                Ok(middle_ty)
            }

            AstKind::Negate { expr: inner }
            | AstKind::Complement { expr: inner }
            | AstKind::Not { expr: inner } => {
                let ty = self.annotate(inner)?;

                if !is_scalar_type(&ty) {
                    return Err("expected scalar type".to_string());
                }

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }

            AstKind::PostIncr { expr: inner }
            | AstKind::PostDecr { expr: inner } => {
                let ty = self.annotate(inner)?;

                if !is_scalar_type(&ty) {
                    return Err("expected scalar type".to_string());
                }

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }

            AstKind::Call { expr: callee, args } => {
                let mut arg_tys: Vec<TypeRef> = vec![];

                for arg in args {
                    arg_tys.push(self.annotate(arg)?);
                }

                let ty = self.annotate(callee)?;
                if let TypeKind::Function {
                    param_tys,
                    return_ty,
                } = &ty.clone().borrow().kind
                {
                    if args.len() > param_tys.len() {
                        return Err("Too many arguments".to_string());
                    } else if args.len() < param_tys.len() {
                        return Err("Too few arguments".to_string());
                    } else {
                        let call_ty = Rc::new(RefCell::new(Type {
                            kind: TypeKind::Function {
                                param_tys: arg_tys,
                                return_ty: return_ty.clone(),
                            },
                            basetype: None,
                            alignment: 8,
                            size: 1,
                            flags: 0,
                        }));

                        if !is_compatible(&ty, &call_ty) {
                            return Err("Argument mismatch".to_string());
                        }

                        *node.ty.borrow_mut() = (*return_ty.borrow()).clone();

                        return Ok(return_ty.clone());
                    }
                }

                Ok(undefined_type())
            }

            AstKind::ConstInt(_) => {
                let ty = int_type();
                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }

            _ => {
                let ty = undefined_type();
                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }
        }
    }

    pub fn run(&self, ast: &Vec<AstRef>) -> Result<(), String> {
        for node in ast {
            self.annotate(node)?;
        }

        Ok(())
    }
}
