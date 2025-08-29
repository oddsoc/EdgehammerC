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
use std::collections::{HashMap, HashSet};
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
    Long,
    LongLong,
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

pub fn int_type(is_signed: bool) -> TypeRef {
    let flags = if is_signed {
        IS_SIGNED | IS_SCALAR
    } else {
        IS_SCALAR
    };

    Rc::new(RefCell::new(Type {
        kind: TypeKind::Int,
        basetype: None,
        alignment: 4,
        size: 4,
        flags: flags,
    }))
}

pub fn long_type(is_signed: bool) -> TypeRef {
    let flags = if is_signed {
        IS_SIGNED | IS_SCALAR
    } else {
        IS_SCALAR
    };

    Rc::new(RefCell::new(Type {
        kind: TypeKind::Long,
        basetype: None,
        alignment: 8,
        size: 8,
        flags: flags,
    }))
}

pub fn long_long_type(is_signed: bool) -> TypeRef {
    let flags = if is_signed {
        IS_SIGNED | IS_SCALAR
    } else {
        IS_SCALAR
    };

    Rc::new(RefCell::new(Type {
        kind: TypeKind::LongLong,
        basetype: None,
        alignment: 8,
        size: 8,
        flags: flags,
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

pub fn is_compatible(ty0: &TypeRef, ty1: &TypeRef) -> bool {
    (is_scalar_type(ty0) && is_scalar_type(ty1)) || is_match(ty0, ty1)
}

pub fn is_match(ty0: &TypeRef, ty1: &TypeRef) -> bool {
    *ty0.borrow() == *ty1.borrow()
}

pub fn is_int_type(ty: &TypeRef) -> bool {
    match ty.borrow().kind {
        TypeKind::Int | TypeKind::Long | TypeKind::LongLong => true,
        _ => false,
    }
}

fn is_scalar_type(ty: &TypeRef) -> bool {
    ty.borrow().flags & IS_SCALAR != 0
}

fn get_common_type(ty0: &TypeRef, ty1: &TypeRef) -> TypeRef {
    if *ty0.borrow() == *ty1.borrow() {
        return ty0.clone();
    }

    if ty0.borrow().size == ty1.borrow().size {
        if is_signed(ty0) {
            return ty1.clone();
        } else {
            return ty0.clone();
        }
    }

    if ty0.borrow().size > ty1.borrow().size {
        return ty0.clone();
    } else {
        return ty1.clone();
    }
}

pub fn type_of(node: &AstRef) -> TypeRef {
    node.borrow().ty.clone()
}

pub fn has_type(node: &AstRef) -> bool {
    !matches!(type_of(node).borrow().kind, TypeKind::Undefined)
}

fn cast_to(expr: &AstRef, ty: &TypeRef) -> AstRef {
    if *type_of(expr).borrow() == *ty.borrow() {
        return Rc::clone(expr);
    }

    if let AstKind::Cast { .. } = &expr.borrow().kind {
        return Rc::clone(expr);
    }

    let scope = scope_of(expr);

    let subexpr = Rc::new(RefCell::new(expr.borrow().clone()));
    let cast = Rc::new(RefCell::new(Ast {
        id: 0,
        ty: ty.clone(),
        kind: AstKind::Cast {
            type_spec: None,
            expr: subexpr,
        },
        scope: scope.clone(),
    }));

    cast
}

pub struct Annotator {
    file_decls: HashSet<String>,
    decl_types: HashMap<usize, TypeRef>,
}

impl Annotator {
    pub fn new() -> Annotator {
        Annotator {
            file_decls: HashSet::new(),
            decl_types: HashMap::new(),
        }
    }

    fn check_decls(&self, decls: &Vec<SymRef>) -> Result<(), String> {
        let mut prev_ty: Option<TypeRef> = None;

        for decl in decls {
            if let Some(node) = sym_as_node(decl.clone()) {
                let decl_ty = node.borrow().ty.clone();

                if let Some(ty) = &prev_ty {
                    if !is_match(&ty, &decl_ty) {
                        return Err("mismatching types".to_string());
                    }
                } else {
                    prev_ty = Some(decl_ty.clone());
                }
            }
        }

        Ok(())
    }

    fn annotate(&mut self, ast: &AstRef) -> Result<(AstRef, TypeRef), String> {
        if has_type(ast) {
            return Ok((ast.clone(), type_of(ast)));
        }

        let mut node_ty = type_of(ast);

        match &ast.borrow().kind {
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
                    let (_, par_ty) = self.annotate(param)?;
                    param_tys.push(par_ty);
                }

                let has_void = param_tys
                    .iter()
                    .any(|p| matches!(p.borrow().kind, TypeKind::Void));

                if has_void && param_tys.len() > 1 {
                    return Err("void must be the only parameter".to_string());
                } else if has_void {
                    param_tys.clear()
                }

                let (_, return_ty) = self.annotate(rty_spec)?;

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

                node_ty = ty.clone();

                self.file_decls.insert(name.clone());
                self.decl_types.insert(ast.borrow().id, node_ty.clone());

                if let Some(body) = block {
                    self.annotate(body)?;
                }
            }

            AstKind::Parameter {
                name: _,
                sym: _,
                idx: _,
                type_spec,
            } => {
                let (_, ty) = self.annotate(type_spec)?;
                node_ty = ty.clone();
                self.decl_types.insert(ast.borrow().id, node_ty.clone());
            }

            AstKind::Block { body } => {
                for stmt in body {
                    self.annotate(stmt)?;
                }
            }

            AstKind::Variable {
                type_spec,
                init,
                name,
                ..
            } => {
                let (_, ty) = self.annotate(type_spec)?;

                node_ty = ty.clone();

                self.decl_types.insert(ast.borrow().id, node_ty.clone());
                self.file_decls.insert(name.clone());

                if let Some(init) = init {
                    self.annotate(init)?;
                }
            }

            AstKind::Return { expr, func } => {
                let (e, expr_ty) = self.annotate(expr)?;
                node_ty = undefined_type();

                if let Some(sym) = func.upgrade() {
                    if let Some(f) = sym_as_node(sym) {
                        let id = f.borrow().id;
                        if let Some(t) = self.decl_types.get(&id) {
                            if let TypeKind::Function { return_ty, .. } =
                                &t.borrow().kind
                            {
                                node_ty = return_ty.clone();
                            }
                        }
                    }
                }

                if !is_scalar_type(&expr_ty) {
                    return Err("expected scalar type".to_string());
                }

                if !is_scalar_type(&node_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(expr, &cast_to(&e, &node_ty));
            }

            AstKind::If {
                cond,
                then,
                otherwise,
            } => {
                let (new_cond, cond_ty) = self.annotate(cond)?;

                if !is_scalar_type(&cond_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(cond, &new_cond);

                let (new_then, _) = self.annotate(then)?;
                replace(then, &new_then);

                if let Some(otherwise) = otherwise {
                    let (new_otherwise, _) = self.annotate(otherwise)?;
                    replace(otherwise, &new_otherwise);
                }
            }

            AstKind::DoWhile { cond, body } => {
                let (new_cond, cond_ty) = self.annotate(cond)?;

                if !is_scalar_type(&cond_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(cond, &new_cond);

                self.annotate(body)?;
            }

            AstKind::While { cond, body } => {
                let (new_cond, cond_ty) = self.annotate(cond)?;

                if !is_scalar_type(&cond_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(cond, &new_cond);

                self.annotate(body)?;
            }

            AstKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init) = init {
                    let (new_init, _) = self.annotate(init)?;
                    replace(init, &new_init);
                }

                if let Some(c) = cond {
                    let (new_cond, cond_ty) = self.annotate(c)?;

                    if !is_scalar_type(&cond_ty) {
                        return Err("expected scalar type".to_string());
                    }

                    replace(c, &new_cond);
                }

                if let Some(post) = post {
                    self.annotate(post)?;
                }

                self.annotate(body)?;
            }

            AstKind::ExprStmt { expr } => {
                let _ = self.annotate(expr)?;
            }
            AstKind::GoTo { .. } => {}

            AstKind::Label { stmt, .. } => {
                self.annotate(stmt)?;
            }

            AstKind::Case { expr, stmt, idx: _ } => {
                let (new_expr, expr_ty) = self.annotate(expr)?;

                if !is_int_type(&expr_ty) {
                    return Err(
                        "switch expression must be an integer".to_string()
                    );
                }

                replace(expr, &new_expr);

                self.annotate(stmt)?;
            }

            AstKind::Default { stmt } => {
                self.annotate(stmt)?;
            }

            AstKind::Switch { cond, body, cases } => {
                let (new_cond, cond_ty) = self.annotate(cond)?;

                if !is_int_type(&cond_ty) {
                    return Err(
                        "switch expression must be an integer".to_string()
                    );
                }

                replace(cond, &new_cond);

                for case in cases {
                    if let AstKind::Case { expr, .. } = &case.borrow().kind {
                        let e = cast_to(&expr, &cond_ty);
                        replace(expr, &e);
                    }
                }

                self.annotate(body)?;
            }

            AstKind::Void => {
                let ty = void_type();
                node_ty = ty.clone();
            }

            AstKind::Int => {
                let ty = int_type(true);
                node_ty = ty.clone();
            }

            AstKind::Identifier { .. } => {
                if let Some(sym) = resolve(&ast.clone()) {
                    if let Some(sym_node) = sym_as_node(sym.clone()) {
                        let id = sym_node.borrow().id;
                        if let Some(ty) = self.decl_types.get(&id) {
                            node_ty = ty.clone();
                        }
                    }
                }
            }

            AstKind::LogicAnd { left, right }
            | AstKind::LogicOr { left, right } => {
                let (lhs, lhs_ty) = self.annotate(left)?;
                let (rhs, rhs_ty) = self.annotate(right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(left, &lhs);
                replace(right, &rhs);

                let ty = int_type(true);

                node_ty = ty.clone();
            }

            AstKind::Assign { left, right } => {
                let (_, lhs_ty) = self.annotate(left)?;
                let (rhs, rhs_ty) = self.annotate(right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(right, &cast_to(&rhs, &lhs_ty));

                node_ty = lhs_ty.clone();
            }

            AstKind::Add { left, right }
            | AstKind::Subtract { left, right }
            | AstKind::Multiply { left, right }
            | AstKind::Divide { left, right }
            | AstKind::Modulo { left, right }
            | AstKind::And { left, right }
            | AstKind::Or { left, right }
            | AstKind::Xor { left, right } => {
                let (lhs, lhs_ty) = self.annotate(left)?;
                let (rhs, rhs_ty) = self.annotate(right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                let common_ty = get_common_type(&lhs_ty, &rhs_ty);

                replace(left, &cast_to(&lhs, &common_ty));
                replace(right, &cast_to(&rhs, &common_ty));

                node_ty = common_ty.clone();
            }

            AstKind::LShift { left, right }
            | AstKind::RShift { left, right } => {
                let (lhs, lhs_ty) = self.annotate(left)?;
                let (rhs, rhs_ty) = self.annotate(right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(left, &lhs);
                replace(right, &rhs);

                node_ty = lhs_ty.clone();
            }

            AstKind::Equal { left, right }
            | AstKind::NotEq { left, right }
            | AstKind::LessThan { left, right }
            | AstKind::LessOrEq { left, right }
            | AstKind::GreaterThan { left, right }
            | AstKind::GreaterOrEq { left, right } => {
                let (lhs, lhs_ty) = self.annotate(left)?;
                let (rhs, rhs_ty) = self.annotate(right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err("expected scalar type".to_string());
                }

                let common_ty = get_common_type(&lhs_ty, &rhs_ty);

                replace(left, &cast_to(&lhs, &common_ty));
                replace(right, &cast_to(&rhs, &common_ty));

                let ty = int_type(true);

                node_ty = ty.clone();
            }

            AstKind::Ternary {
                left,
                middle,
                right,
            } => {
                let (lhs, lhs_ty) = self.annotate(left)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err("expected a scalar type".to_string());
                }

                replace(left, &lhs);

                let (mhs, mhs_ty) = self.annotate(middle)?;

                if !is_scalar_type(&mhs_ty) {
                    return Err("expected a scalar type".to_string());
                }

                let (rhs, rhs_ty) = self.annotate(right)?;

                if !is_scalar_type(&rhs_ty) {
                    return Err("expected a scalar type".to_string());
                }

                if !is_compatible(&mhs_ty, &rhs_ty) {
                    return Err("incompatible types".to_string());
                }

                let common_ty = get_common_type(&mhs_ty, &rhs_ty);

                replace(middle, &cast_to(&mhs, &common_ty));
                replace(right, &cast_to(&rhs, &common_ty));

                node_ty = common_ty.clone();
            }

            AstKind::Not { expr: inner } => {
                let (new_inner, inner_ty) = self.annotate(inner)?;

                if !is_scalar_type(&inner_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(inner, &new_inner);

                let ty = int_type(true);

                node_ty = ty.clone();
            }

            AstKind::Negate { expr: inner }
            | AstKind::Complement { expr: inner } => {
                let (new_inner, inner_ty) = self.annotate(inner)?;

                if !is_scalar_type(&inner_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(inner, &new_inner);

                node_ty = inner_ty.clone();
            }

            AstKind::PostIncr { expr: inner }
            | AstKind::PostDecr { expr: inner } => {
                let (new_inner, inner_ty) = self.annotate(inner)?;

                if !is_scalar_type(&inner_ty) {
                    return Err("expected scalar type".to_string());
                }

                replace(inner, &new_inner);

                node_ty = inner_ty.clone();
            }

            AstKind::Cast {
                type_spec,
                expr: inner,
            } => {
                self.annotate(inner)?;

                if let Some(ty_spec) = type_spec {
                    let (new_type_spec, ty) = self.annotate(ty_spec)?;

                    replace(ty_spec, &new_type_spec);
                    node_ty = ty.clone();
                }
            }

            AstKind::Call { expr: callee, args } => {
                let (new_callee, call_ty) = self.annotate(callee)?;
                replace(callee, &new_callee);

                if let TypeKind::Function {
                    param_tys,
                    return_ty,
                } = &call_ty.clone().borrow().kind
                {
                    if args.len() > param_tys.len() {
                        return Err("too many arguments".to_string());
                    } else if args.len() < param_tys.len() {
                        return Err("too few arguments".to_string());
                    } else {
                        for (arg, param_ty) in args.iter().zip(param_tys.iter())
                        {
                            self.annotate(arg)?;
                            let new_arg = cast_to(arg, param_ty);
                            replace(&arg, &new_arg);
                        }
                        node_ty = return_ty.clone();
                    }
                }
            }

            AstKind::ConstInt(_) => {
                let ty = int_type(true);
                node_ty = ty.clone();
            }

            AstKind::ConstLong(_) => {
                let ty = long_type(true);
                node_ty = ty.clone();
            }

            AstKind::ConstUnsignedInt(_) => {
                let ty = int_type(false);
                node_ty = ty.clone();
            }

            AstKind::ConstUnsignedLong(_) => {
                let ty = long_type(false);
                node_ty = ty.clone();
            }

            AstKind::StaticInitializer(c_expr) => {
                let (e, ty) = self.annotate(c_expr)?;
                replace(c_expr, &e);
                node_ty = ty.clone();
            }

            AstKind::Initializer(expr) => {
                let (e, ty) = self.annotate(expr)?;
                replace(expr, &e);
                node_ty = ty.clone();
            }

            _ => {
                let ty = undefined_type();
                node_ty = ty.clone();
            }
        }

        ast.borrow_mut().ty = node_ty.clone();

        Ok((ast.clone(), node_ty.clone()))
    }

    pub fn run(&mut self, ast: &Vec<AstRef>) -> Result<(), String> {
        if ast.len() == 0 {
            return Ok(());
        }

        let scope = scope_of(&ast[0]);

        for node in ast {
            self.annotate(node)?;
        }

        for name in &self.file_decls {
            if let Some(decls) = get_all_named_sym_decls(scope.clone(), name) {
                self.check_decls(&decls)?;
            }
        }

        Ok(())
    }
}
