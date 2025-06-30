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

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub struct Type {
    pub kind: TypeKind,
    pub basetype: Option<TypeRef>,
    pub alignment: usize,
    pub size: usize,
    pub signed: bool,
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
        signed: false,
    }))
}

pub fn int_type() -> TypeRef {
    Rc::new(RefCell::new(Type {
        kind: TypeKind::Int,
        basetype: None,
        alignment: 4,
        size: 4,
        signed: true,
    }))
}

pub fn void_type() -> TypeRef {
    Rc::new(RefCell::new(Type {
        kind: TypeKind::Void,
        basetype: None,
        alignment: 1,
        size: 1,
        signed: false,
    }))
}

fn is_compatible(ty0: &TypeRef, ty1: &TypeRef) -> bool {
    if *ty0.borrow() == *ty1.borrow() {
        true
    } else {
        false
    }
}

fn is_int_type(ty: &TypeRef) -> bool {
    match ty.borrow().kind {
        TypeKind::Int => true,
        _ => false,
    }
}

fn is_scalar_type(ty: &TypeRef) -> bool {
    is_int_type(ty)
}

pub struct Annotator;

impl Annotator {
    pub fn new() -> Annotator {
        Annotator {}
    }

    pub fn annotate(&self, ast: &ASTRef) -> Result<TypeRef, String> {
        let node = ast.as_ref().borrow();

        if !matches!(node.ty.borrow().kind, TypeKind::Undefined) {
            return Ok(node.ty.clone());
        }

        match &node.kind {
            ASTKind::Function {
                name,
                params,
                block,
                ty: rty,
                ..
            } => {
                let mut param_tys: Vec<TypeRef> = vec![];

                for param in params {
                    param_tys.push(self.annotate(&param)?);
                }

                let has_void = param_tys
                    .iter()
                    .any(|p| matches!(p.borrow().kind, TypeKind::Void));

                if has_void && param_tys.len() > 1 {
                    return Err("void must be the only parameter".to_string());
                } else if has_void {
                    param_tys.clear()
                }

                let return_ty = self.annotate(&rty)?;

                let ty = Rc::new(RefCell::new(Type {
                    kind: TypeKind::Function {
                        param_tys,
                        return_ty: return_ty.clone(),
                    },
                    basetype: None,
                    alignment: 8,
                    size: 1,
                    signed: false,
                }));

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                let sc = global_scope(node.scope.clone());
                let extern_sym = get_sym(sc.clone(), &name, None);

                if let Some(extern_node) = extern_sym
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .node
                    .as_ref()
                    .unwrap()
                    .upgrade()
                {
                    if extern_node.borrow().id != node.id {
                        let extern_ty = self.annotate(&extern_node)?;

                        if !is_compatible(&ty, &extern_ty) {
                            return Err("incompatible types".to_string());
                        }

                        if let ASTKind::Function { block: b, .. } =
                            &extern_node.borrow().kind
                        {
                            if b.is_some() && block.is_some() {
                                return Err(format!(
                                    "multiple definitions of {}",
                                    name
                                ));
                            }
                        }
                    }
                }

                if let Some(body) = block {
                    self.annotate(&body)?;
                }

                Ok(ty)
            }

            ASTKind::Parameter {
                name: _,
                idx: _,
                ty: type_spec,
            } => {
                let ty = self.annotate(&type_spec)?;
                *node.ty.borrow_mut() = (*ty.borrow()).clone();
                Ok(ty)
            }

            ASTKind::Block { body } => {
                for stmt in body {
                    self.annotate(&stmt)?;
                }

                Ok(undefined_type())
            }

            ASTKind::Variable {
                ty: var_ty, init, ..
            } => {
                let ty = self.annotate(&var_ty)?;

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                if let Some(init) = init {
                    self.annotate(&init)?;
                }

                Ok(ty)
            }

            ASTKind::Return { expr } => {
                self.annotate(&expr)?;
                Ok(undefined_type())
            }

            ASTKind::If {
                cond,
                then,
                otherwise,
            } => {
                if !is_scalar_type(&self.annotate(&cond)?) {
                    return Err("expected scalar type".to_string());
                }

                self.annotate(&then)?;
                if let Some(otherwise) = otherwise {
                    self.annotate(&otherwise)?;
                }
                Ok(undefined_type())
            }
            ASTKind::DoWhile { cond, body } => {
                if !is_scalar_type(&self.annotate(&cond)?) {
                    return Err("expected scalar type".to_string());
                }

                self.annotate(&body)?;
                Ok(undefined_type())
            }
            ASTKind::While { cond, body } => {
                if !is_scalar_type(&self.annotate(&cond)?) {
                    return Err("expected scalar type".to_string());
                }

                self.annotate(&body)?;
                Ok(undefined_type())
            }
            ASTKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init) = init {
                    self.annotate(&init)?;
                }

                if let Some(cond) = cond {
                    if !is_scalar_type(&self.annotate(&cond)?) {
                        return Err("expected scalar type".to_string());
                    }
                }

                if let Some(post) = post {
                    self.annotate(&post)?;
                }

                self.annotate(&body)?;

                Ok(undefined_type())
            }

            ASTKind::ExprStmt { expr } => {
                let _ = self.annotate(&expr)?;
                Ok(undefined_type())
            }
            ASTKind::GoTo { .. } => Ok(undefined_type()),

            ASTKind::Label { stmt, .. } => {
                self.annotate(&stmt)?;
                Ok(undefined_type())
            }

            ASTKind::Case { expr, stmt, idx: _ } => {
                if !is_int_type(&self.annotate(&expr)?) {
                    return Err(
                        "switch expression must be an integer".to_string()
                    );
                }
                self.annotate(&stmt)?;
                Ok(undefined_type())
            }

            ASTKind::Default { stmt } => {
                self.annotate(&stmt)?;
                Ok(undefined_type())
            }

            ASTKind::Switch {
                cond,
                body,
                cases: _,
            } => {
                if !is_int_type(&self.annotate(&cond)?) {
                    return Err(
                        "switch expression must be an integer".to_string()
                    );
                }

                self.annotate(&body)?;
                Ok(undefined_type())
            }

            ASTKind::Void => {
                let ty = void_type();
                *node.ty.borrow_mut() = (*ty.borrow()).clone();
                Ok(ty)
            }

            ASTKind::Int => {
                let ty = int_type();
                *node.ty.borrow_mut() = (*ty.borrow()).clone();
                Ok(ty)
            }

            ASTKind::Identifier { .. } => {
                if let Some(identified) = node.resolve_node() {
                    let ty = self.annotate(&identified)?;
                    *node.ty.borrow_mut() = (*ty.borrow()).clone();
                    Ok(ty)
                } else {
                    Ok(undefined_type())
                }
            }

            ASTKind::Assign { left, right }
            | ASTKind::Add { left, right }
            | ASTKind::Subtract { left, right }
            | ASTKind::Multiply { left, right }
            | ASTKind::Divide { left, right }
            | ASTKind::Modulo { left, right }
            | ASTKind::LShift { left, right }
            | ASTKind::RShift { left, right }
            | ASTKind::And { left, right }
            | ASTKind::Or { left, right }
            | ASTKind::Xor { left, right }
            | ASTKind::Equal { left, right }
            | ASTKind::NotEq { left, right }
            | ASTKind::LessThan { left, right }
            | ASTKind::LessOrEq { left, right }
            | ASTKind::GreaterThan { left, right }
            | ASTKind::GreaterOrEq { left, right }
            | ASTKind::LogicAnd { left, right }
            | ASTKind::LogicOr { left, right } => {
                if !is_scalar_type(&self.annotate(&right)?) {
                    return Err("expected scalar type".to_string());
                }

                let ty = self.annotate(&left)?;

                if !is_scalar_type(&ty) {
                    return Err("expected scalar type".to_string());
                }

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }

            ASTKind::Conditional {
                left,
                middle,
                right,
            } => {
                let left_ty = self.annotate(&left)?;

                if !is_scalar_type(&left_ty) {
                    return Err("expected a scalar type".to_string());
                }

                let middle_ty = self.annotate(&middle)?;

                if !is_scalar_type(&middle_ty) {
                    return Err("expected a scalar type".to_string());
                }

                let right_ty = self.annotate(&right)?;

                if !is_scalar_type(&right_ty) {
                    return Err("expected a scalar type".to_string());
                }

                if !is_compatible(&middle_ty, &right_ty) {
                    return Err("incompatible types".to_string());
                }

                *node.ty.borrow_mut() = (*middle_ty.borrow()).clone();

                Ok(middle_ty)
            }

            ASTKind::Negate { expr: inner }
            | ASTKind::Complement { expr: inner }
            | ASTKind::Not { expr: inner } => {
                let ty = self.annotate(&inner)?;

                if !is_scalar_type(&ty) {
                    return Err("expected scalar type".to_string());
                }

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }

            ASTKind::PreIncr { expr: inner }
            | ASTKind::PostIncr { expr: inner }
            | ASTKind::PreDecr { expr: inner }
            | ASTKind::PostDecr { expr: inner } => {
                let ty = self.annotate(&inner)?;

                if !is_scalar_type(&ty) {
                    return Err("expected scalar type".to_string());
                }

                *node.ty.borrow_mut() = (*ty.borrow()).clone();

                Ok(ty)
            }

            ASTKind::Call { expr: callee, args } => {
                let mut arg_tys: Vec<TypeRef> = vec![];

                for arg in args {
                    arg_tys.push(self.annotate(&arg)?);
                }

                let ty = self.annotate(&callee)?;
                match &ty.clone().borrow().kind {
                    TypeKind::Function {
                        param_tys,
                        return_ty,
                    } => {
                        if args.len() > param_tys.len() {
                            return Err(format!("Too many arguments"));
                        } else if args.len() < param_tys.len() {
                            return Err(format!("Too few arguments"));
                        } else {
                            let call_ty = Rc::new(RefCell::new(Type {
                                kind: TypeKind::Function {
                                    param_tys: arg_tys,
                                    return_ty: return_ty.clone(),
                                },
                                basetype: None,
                                alignment: 8,
                                size: 1,
                                signed: false,
                            }));

                            if !is_compatible(&ty, &call_ty) {
                                return Err(format!("Argument mismatch"));
                            }

                            *node.ty.borrow_mut() =
                                (*return_ty.borrow()).clone();

                            return Ok(return_ty.clone());
                        }
                    }
                    _ => {}
                }

                Ok(undefined_type())
            }

            ASTKind::ConstInt(_) => {
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

    pub fn run(&self, ast: &Vec<ASTRef>) -> Result<(), String> {
        for node in ast {
            self.annotate(node)?;
        }

        Ok(())
    }
}
