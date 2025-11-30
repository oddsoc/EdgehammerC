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
use std::collections::HashSet;
use std::rc::Rc;

use crate::ast::*;
use crate::errors::{
    Error, ErrorClass::TypeChecking, TypeCheckingError, error,
};
use crate::expr::{is_lvalue, is_null_pointer_const_expr};
use crate::symtab::*;

pub type TypeRef = Rc<RefCell<Type>>;

const IS_SIGNED: u8 = 1;
const IS_SCALAR: u8 = 1 << 1;
const IS_ARITHMETIC: u8 = 1 << 2;

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
    Pointer,
    Void,
    Int,
    Long,
    LongLong,
    Double,
    LongDouble,
    Array(usize),
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
        IS_SIGNED | IS_SCALAR | IS_ARITHMETIC
    } else {
        IS_SCALAR | IS_ARITHMETIC
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
        IS_SIGNED | IS_SCALAR | IS_ARITHMETIC
    } else {
        IS_SCALAR | IS_ARITHMETIC
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
        IS_SIGNED | IS_SCALAR | IS_ARITHMETIC
    } else {
        IS_SCALAR | IS_ARITHMETIC
    };

    Rc::new(RefCell::new(Type {
        kind: TypeKind::LongLong,
        basetype: None,
        alignment: 8,
        size: 8,
        flags: flags,
    }))
}

pub fn is_undefined_type(ty: &TypeRef) -> bool {
    matches!(ty.borrow().kind, TypeKind::Undefined)
}

pub fn is_signed(ty: &TypeRef) -> bool {
    ty.borrow().flags & IS_SIGNED != 0
}

pub fn size_of(ty: &TypeRef) -> usize {
    ty.borrow().size
}

pub fn alignment_of(ty: &TypeRef) -> usize {
    ty.borrow().alignment
}

pub fn double_type() -> TypeRef {
    let flags = IS_SCALAR | IS_ARITHMETIC;

    Rc::new(RefCell::new(Type {
        kind: TypeKind::Double,
        basetype: None,
        alignment: 8,
        size: 8,
        flags: flags,
    }))
}

pub fn long_double_type() -> TypeRef {
    let flags = IS_SCALAR | IS_ARITHMETIC;

    Rc::new(RefCell::new(Type {
        kind: TypeKind::LongDouble,
        basetype: None,
        alignment: 16,
        size: 16,
        flags: flags,
    }))
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

pub fn pointer_type(basetype: &TypeRef) -> TypeRef {
    Rc::new(RefCell::new(Type {
        kind: TypeKind::Pointer,
        basetype: Some(basetype.clone()),
        alignment: 8,
        size: 8,
        flags: IS_SCALAR,
    }))
}

pub fn array_type(basetype: &TypeRef, len: usize) -> TypeRef {
    Rc::new(RefCell::new(Type {
        kind: TypeKind::Array(len),
        basetype: Some(basetype.clone()),
        alignment: alignment_of(basetype),
        size: size_of(basetype) * len,
        flags: 0,
    }))
}

pub fn base_type(ty: &TypeRef) -> Option<TypeRef> {
    ty.borrow().basetype.clone()
}

pub fn base_type_v2(ty: &TypeRef) -> TypeRef {
    if let Some(base_ty) = ty.borrow().basetype.clone() {
        base_ty
    } else {
        ty.clone()
    }
}

pub fn innermost_base_type(ty: &TypeRef) -> TypeRef {
    let mut ty_iter = base_type(ty);

    while let Some(base_ty) = &mut ty_iter {
        if base_ty.borrow().basetype.is_some() {
            ty_iter = base_type(base_ty);
        } else {
            break;
        }
    }

    if let Some(base_ty) = ty_iter {
        base_ty
    } else {
        ty.clone()
    }
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

pub fn is_double_type(ty: &TypeRef) -> bool {
    match ty.borrow().kind {
        TypeKind::Double => true,
        _ => false,
    }
}

pub fn is_long_double_type(ty: &TypeRef) -> bool {
    match ty.borrow().kind {
        TypeKind::LongDouble => true,
        _ => false,
    }
}

#[allow(unused)]
pub fn is_pointer_type(ty: &TypeRef) -> bool {
    matches!(ty.borrow().kind, TypeKind::Pointer)
}

pub fn is_array_type(ty: &TypeRef) -> bool {
    matches!(ty.borrow().kind, TypeKind::Array(_))
}

pub fn is_function_type(ty: &TypeRef) -> bool {
    matches!(ty.borrow().kind, TypeKind::Function { .. })
}

pub fn is_scalar_type(ty: &TypeRef) -> bool {
    ty.borrow().flags & IS_SCALAR != 0
}

pub fn is_arithmetic_type(ty: &TypeRef) -> bool {
    ty.borrow().flags & IS_ARITHMETIC != 0
}

fn int_type_rank(ty: &TypeRef) -> usize {
    ty.borrow().size - if is_signed(ty) { 1 } else { 0 }
}

fn convert_by_assignment(
    arena: &mut AstArena,
    id: AstId,
    ty: &TypeRef,
) -> Result<AstId, Error> {
    if *type_of(arena, id).borrow() == *ty.borrow() {
        Ok(id)
    } else if is_arithmetic_type(&type_of(arena, id)) && is_arithmetic_type(ty)
    {
        Ok(cast_to(arena, id, ty))
    } else if is_null_pointer_const_expr(arena, id) && is_pointer_type(ty) {
        Ok(cast_to(arena, id, ty))
    } else {
        Err(error(TypeChecking(
            TypeCheckingError::CannotConvertTypeForAssign,
        )))
    }
}

fn get_common_pointer_type(
    arena: &AstArena,
    e0: AstId,
    e1: AstId,
) -> Result<TypeRef, Error> {
    let ty0 = type_of(arena, e0);
    let ty1 = type_of(arena, e1);

    if *ty0.borrow() == *ty1.borrow() {
        Ok(ty0.clone())
    } else if is_null_pointer_const_expr(arena, e0) {
        Ok(ty1.clone())
    } else if is_null_pointer_const_expr(arena, e1) {
        Ok(ty0.clone())
    } else {
        Err(error(TypeChecking(
            TypeCheckingError::IncompatibleExprTypes,
        )))
    }
}

fn get_common_type(ty0: &TypeRef, ty1: &TypeRef) -> TypeRef {
    if *ty0.borrow() == *ty1.borrow() {
        return ty0.clone();
    }

    if is_long_double_type(ty0) || is_long_double_type(ty1) {
        return long_double_type();
    }

    if is_double_type(ty0) || is_double_type(ty1) {
        return double_type();
    }

    let rank0 = int_type_rank(ty0);
    let rank1 = int_type_rank(ty1);

    if rank0 != rank1 {
        return if rank0 > rank1 {
            ty0.clone()
        } else {
            ty1.clone()
        };
    }

    let signed0 = is_signed(ty0);
    let signed1 = is_signed(ty1);

    match (signed0, signed1) {
        (true, true) | (false, false) => ty0.clone(),
        (true, false) => ty1.clone(),
        (false, true) => ty0.clone(),
    }
}

pub fn type_of(arena: &AstArena, id: AstId) -> TypeRef {
    arena[id].ty.clone()
}

pub fn has_type(arena: &AstArena, id: AstId) -> bool {
    !is_undefined_type(&arena[id].ty)
}

fn cast_to(arena: &mut AstArena, expr: AstId, ty: &TypeRef) -> AstId {
    if *type_of(arena, expr).borrow() == *ty.borrow() {
        return expr;
    }

    if let AstKind::Cast { .. } = &arena[expr].kind {
        return expr;
    }

    let scope = scope_of(arena, expr);

    let subexpr_id = arena.alloc_clone(expr);
    arena.alloc(
        AstKind::Cast {
            type_spec: None,
            expr: subexpr_id,
        },
        Some(ty.clone()),
        scope,
    )
}

pub struct TypeAnnotator {
    decls: HashSet<String>,
    current_fn: Option<SymId>,
}

impl TypeAnnotator {
    pub fn new() -> TypeAnnotator {
        TypeAnnotator {
            decls: HashSet::new(),
            current_fn: None,
        }
    }

    fn check_decls(
        &self,
        decls: &Vec<SymId>,
        arena: &AstArena,
        symtab: &SymTab,
    ) -> Result<(), Error> {
        let mut prev_ty: Option<TypeRef> = None;

        for &decl in decls {
            if let Some(node) = sym_as_node(symtab, decl) {
                let decl_ty = arena[node].ty.clone();

                if let Some(ty) = &prev_ty {
                    if !is_match(&ty, &decl_ty) {
                        return Err(error(TypeChecking(
                            TypeCheckingError::TypeMismatch,
                        )));
                    }
                } else {
                    prev_ty = Some(decl_ty.clone());
                }
            }
        }

        Ok(())
    }

    fn annotate(
        &mut self,
        arena: &mut AstArena,
        symtab: &SymTab,
        id: AstId,
    ) -> Result<(AstId, TypeRef), Error> {
        if has_type(arena, id) {
            return Ok((id, type_of(arena, id)));
        }

        let mut node_ty = type_of(arena, id);
        let kind = arena[id].kind.clone();

        match &kind {
            AstKind::Function {
                name,
                sym,
                params,
                block,
                type_spec: rty_spec,
                ..
            } => {
                if let AstKind::Array { .. } = &arena[*rty_spec].kind {
                    return Err(error(TypeChecking(
                        TypeCheckingError::CannotReturnArray,
                    )));
                }

                let mut param_tys: Vec<TypeRef> = vec![];

                for &param in params {
                    let (_, par_ty) = self.annotate(arena, symtab, param)?;
                    param_tys.push(par_ty);
                }

                let has_void = param_tys
                    .iter()
                    .any(|p| matches!(p.borrow().kind, TypeKind::Void));

                if has_void && param_tys.len() > 1 {
                    return Err(error(TypeChecking(
                        TypeCheckingError::SurplusVoidParam,
                    )));
                } else if has_void {
                    param_tys.clear()
                }

                let (_, return_ty) = self.annotate(arena, symtab, *rty_spec)?;

                if let TypeKind::Function { .. } = &return_ty.borrow().kind {
                    return Err(error(TypeChecking(
                        TypeCheckingError::CannotReturnFunction,
                    )));
                }

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
                arena[id].ty = node_ty.clone();

                if let Some(name) = name {
                    self.decls.insert(arena.token_str(name).to_string());
                }

                if let Some(body) = block {
                    self.current_fn = *sym;
                    self.annotate(arena, symtab, *body)?;

                    self.current_fn = None;
                }
            }

            AstKind::Block { body } => {
                for &stmt in body {
                    self.annotate(arena, symtab, stmt)?;
                }
            }

            AstKind::Variable {
                type_spec,
                init,
                name,
                ..
            } => {
                let (_, ty) = self.annotate(arena, symtab, *type_spec)?;

                node_ty = ty.clone();
                arena[id].ty = node_ty.clone();

                self.decls.insert(arena.token_str(name).to_string());

                if let Some(init) = init {
                    let (_new_init, _init_ty) =
                        self.annotate(arena, symtab, *init)?;
                }
            }

            AstKind::Parameter {
                type_spec, name, ..
            } => {
                let (_, ty) =
                    self.annotate_and_convert(arena, symtab, *type_spec)?;

                self.decls.insert(arena.token_str(name).to_string());

                node_ty = ty.clone();
            }

            AstKind::Return { expr } => {
                let (e, _expr_ty) =
                    self.annotate_and_convert(arena, symtab, *expr)?;
                let mut ty = undefined_type();

                if let Some(sym) = self.current_fn {
                    if let Some(f) = sym_as_node(symtab, sym) {
                        if let TypeKind::Function { return_ty, .. } =
                            &type_of(arena, f).borrow().kind
                        {
                            ty = return_ty.clone();
                        }
                    }
                }

                let new_expr = convert_by_assignment(arena, e, &ty)?;
                let _cast_new_expr = cast_to(arena, new_expr, &ty);
                arena[*expr] = arena[_cast_new_expr].clone();

                node_ty = ty;
            }

            AstKind::If {
                cond,
                then,
                otherwise,
            } => {
                let (new_cond, cond_ty) =
                    self.annotate_and_convert(arena, symtab, *cond)?;

                if !is_scalar_type(&cond_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                arena[*cond] = arena[new_cond].clone();

                let (new_then, _) = self.annotate(arena, symtab, *then)?;
                arena[*then] = arena[new_then].clone();

                if let Some(otherwise) = otherwise {
                    let (new_otherwise, _) =
                        self.annotate(arena, symtab, *otherwise)?;
                    arena[*otherwise] = arena[new_otherwise].clone();
                }
            }

            AstKind::DoWhile { cond, body } => {
                let (new_cond, cond_ty) =
                    self.annotate_and_convert(arena, symtab, *cond)?;

                if !is_scalar_type(&cond_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                arena[*cond] = arena[new_cond].clone();

                self.annotate(arena, symtab, *body)?;
            }

            AstKind::While { cond, body } => {
                let (new_cond, cond_ty) =
                    self.annotate_and_convert(arena, symtab, *cond)?;

                if !is_scalar_type(&cond_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                arena[*cond] = arena[new_cond].clone();

                self.annotate(arena, symtab, *body)?;
            }

            AstKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init) = init {
                    let (new_init, _) = self.annotate(arena, symtab, *init)?;
                    arena[*init] = arena[new_init].clone();
                }

                if let Some(c) = cond {
                    let (new_cond, cond_ty) =
                        self.annotate_and_convert(arena, symtab, *c)?;

                    if !is_scalar_type(&cond_ty) {
                        return Err(error(TypeChecking(
                            TypeCheckingError::ExpectedScalarType,
                        )));
                    }

                    arena[*c] = arena[new_cond].clone();
                }

                if let Some(post) = post {
                    self.annotate(arena, symtab, *post)?;
                }

                self.annotate(arena, symtab, *body)?;
            }

            AstKind::ExprStmt { expr } => {
                let _ = self.annotate_and_convert(arena, symtab, *expr)?;
            }
            AstKind::GoTo { .. } => {}

            AstKind::Label { stmt, .. } => {
                self.annotate(arena, symtab, *stmt)?;
            }

            AstKind::Case { expr, stmt, .. } => {
                let (new_expr, expr_ty) =
                    self.annotate_and_convert(arena, symtab, *expr)?;

                if !is_int_type(&expr_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::NonIntegerSwitchExprType,
                    )));
                }

                arena[*expr] = arena[new_expr].clone();

                self.annotate(arena, symtab, *stmt)?;
            }

            AstKind::Default { stmt } => {
                self.annotate(arena, symtab, *stmt)?;
            }

            AstKind::Switch { cond, body, cases } => {
                let (new_cond, cond_ty) =
                    self.annotate_and_convert(arena, symtab, *cond)?;

                if !is_int_type(&cond_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::NonIntegerSwitchExprType,
                    )));
                }

                arena[*cond] = arena[new_cond].clone();

                for &case_id in cases {
                    let case_kind = arena[case_id].kind.clone();
                    if let AstKind::Case { expr, .. } = &case_kind {
                        let e = cast_to(arena, *expr, &cond_ty);
                        arena[*expr] = arena[e].clone();
                    }
                }

                self.annotate(arena, symtab, *body)?;
            }

            AstKind::Void => {
                let ty = void_type();
                node_ty = ty.clone();
            }

            AstKind::Int => {
                let ty = int_type(true);
                node_ty = ty.clone();
            }

            AstKind::Pointer {
                base_type_spec,
                qualifiers: _,
            } => {
                let (new_base_type_spec, base_ty) =
                    self.annotate(arena, symtab, *base_type_spec)?;
                arena[*base_type_spec] = arena[new_base_type_spec].clone();
                node_ty = pointer_type(&base_ty);
            }

            AstKind::Identifier { .. } => {
                if let Some(sym) = resolve(symtab, arena, &id) {
                    if let Some(sym_node) = sym_as_node(symtab, sym) {
                        let (_, ty) = self.annotate(arena, symtab, sym_node)?;
                        node_ty = ty.clone();
                    }
                }
            }

            AstKind::LogicAnd { left, right }
            | AstKind::LogicOr { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                arena[*left] = arena[lhs].clone();
                arena[*right] = arena[rhs].clone();

                let ty = int_type(false);

                node_ty = ty.clone();
            }

            AstKind::CompoundAssign { left, right }
            | AstKind::Assign { left, right } => {
                let (new_left, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if is_array_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::CannotAssignToArrayType,
                    )));
                }

                if !is_lvalue(arena, new_left) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::CannotAssignToNonLvalue,
                    )));
                }

                if !is_scalar_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                let new_rhs = convert_by_assignment(arena, rhs, &lhs_ty)?;
                arena[*right] = arena[new_rhs].clone();

                node_ty = lhs_ty.clone();
            }

            AstKind::Subscript { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if is_pointer_type(&lhs_ty) && is_int_type(&rhs_ty) {
                    let _cast_rhs = cast_to(arena, rhs, &long_type(true));
                    arena[*right] = arena[_cast_rhs].clone();
                    node_ty = base_type(&lhs_ty).expect("expected a pointer");
                } else if is_int_type(&lhs_ty) && is_pointer_type(&rhs_ty) {
                    let int_cast = cast_to(arena, lhs, &long_type(true));
                    arena[*left] = arena[rhs].clone();
                    arena[*right] = arena[int_cast].clone();
                    node_ty = base_type(&rhs_ty).expect("expected a pointer");
                } else {
                    return Err(error(TypeChecking(
                        TypeCheckingError::InvalidSubscriptOperands,
                    )));
                }
            }

            AstKind::Add { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                if is_arithmetic_type(&lhs_ty) && is_arithmetic_type(&rhs_ty) {
                    let common_ty = get_common_type(&lhs_ty, &rhs_ty);

                    let lhs = cast_to(arena, lhs, &common_ty);
                    arena[*left] = arena[lhs].clone();
                    let rhs = cast_to(arena, rhs, &common_ty);
                    arena[*right] = arena[rhs].clone();

                    node_ty = common_ty.clone();
                } else if is_pointer_type(&lhs_ty) && is_int_type(&rhs_ty) {
                    let new_rhs_ty = long_type(true);
                    let rhs = cast_to(arena, rhs, &new_rhs_ty);
                    arena[*right] = arena[rhs].clone();
                    node_ty = lhs_ty.clone();
                } else if is_int_type(&lhs_ty) && is_pointer_type(&rhs_ty) {
                    let int_cast = cast_to(arena, lhs, &long_type(true));
                    arena[*left] = arena[rhs].clone();
                    arena[*right] = arena[int_cast].clone();
                    node_ty = rhs_ty.clone();
                } else {
                    return Err(error(TypeChecking(
                        TypeCheckingError::InvalidAddOperands,
                    )));
                }
            }

            AstKind::Subtract { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                if !is_scalar_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                if is_arithmetic_type(&lhs_ty) && is_arithmetic_type(&rhs_ty) {
                    let common_ty = get_common_type(&lhs_ty, &rhs_ty);

                    let lhs = cast_to(arena, lhs, &common_ty);
                    arena[*left] = arena[lhs].clone();
                    let rhs = cast_to(arena, rhs, &common_ty);
                    arena[*right] = arena[rhs].clone();

                    node_ty = common_ty.clone();
                } else if is_pointer_type(&lhs_ty) && is_int_type(&rhs_ty) {
                    let new_rhs_ty = long_type(true);
                    let rhs = cast_to(arena, rhs, &new_rhs_ty);
                    arena[*right] = arena[rhs].clone();
                    node_ty = lhs_ty.clone();
                } else if is_pointer_type(&lhs_ty) && is_pointer_type(&rhs_ty) {
                    if !is_match(&lhs_ty, &rhs_ty) {
                        return Err(error(TypeChecking(
                            TypeCheckingError::SubtractingDifferingPointers,
                        )));
                    }
                    node_ty = long_type(true);
                } else {
                    return Err(error(TypeChecking(
                        TypeCheckingError::InvalidSubtractOperands,
                    )));
                }
            }

            AstKind::Multiply { left, right }
            | AstKind::Divide { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if !is_arithmetic_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedArithmeticType,
                    )));
                }

                if !is_arithmetic_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedArithmeticType,
                    )));
                }

                let common_ty = get_common_type(&lhs_ty, &rhs_ty);

                let lhs = cast_to(arena, lhs, &common_ty);
                arena[*left] = arena[lhs].clone();
                let rhs = cast_to(arena, rhs, &common_ty);
                arena[*right] = arena[rhs].clone();

                node_ty = common_ty.clone();
            }

            AstKind::Modulo { left, right }
            | AstKind::And { left, right }
            | AstKind::Or { left, right }
            | AstKind::Xor { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if !is_int_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedIntegerType,
                    )));
                }

                if !is_int_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedIntegerType,
                    )));
                }

                let common_ty = get_common_type(&lhs_ty, &rhs_ty);

                let lhs = cast_to(arena, lhs, &common_ty);
                arena[*left] = arena[lhs].clone();
                let rhs = cast_to(arena, rhs, &common_ty);
                arena[*right] = arena[rhs].clone();

                node_ty = common_ty.clone();
            }

            AstKind::LeftShift { left, right }
            | AstKind::RightShift { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if !is_int_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedIntegerType,
                    )));
                }

                if !is_int_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedIntegerType,
                    )));
                }

                arena[*left] = arena[lhs].clone();
                arena[*right] = arena[rhs].clone();

                node_ty = lhs_ty.clone();
            }

            AstKind::Equal { left, right }
            | AstKind::NotEq { left, right }
            | AstKind::Less { left, right }
            | AstKind::LessOrEq { left, right }
            | AstKind::Greater { left, right }
            | AstKind::GreaterOrEq { left, right } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;
                let (rhs, rhs_ty) =
                    self.annotate_and_convert(arena, symtab, *right)?;

                if !is_scalar_type(&lhs_ty) || !is_scalar_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                let is_equality = matches!(
                    arena[id].kind,
                    AstKind::Equal { .. } | AstKind::NotEq { .. }
                );

                let common_ty = if is_pointer_type(&lhs_ty)
                    || is_pointer_type(&rhs_ty)
                {
                    if is_pointer_type(&lhs_ty) && !is_pointer_type(&rhs_ty) {
                        if !is_equality
                            || !is_null_pointer_const_expr(arena, *right)
                        {
                            return Err(error(TypeChecking(
                                TypeCheckingError::ComparePointerNonZeroInteger,
                            )));
                        }
                        lhs_ty.clone()
                    } else if !is_pointer_type(&lhs_ty)
                        && is_pointer_type(&rhs_ty)
                    {
                        if !is_equality
                            || !is_null_pointer_const_expr(arena, *left)
                        {
                            return Err(error(TypeChecking(
                                TypeCheckingError::ComparePointerNonZeroInteger,
                            )));
                        }
                        rhs_ty.clone()
                    } else {
                        get_common_pointer_type(arena, lhs, rhs)?
                    }
                } else {
                    get_common_type(&lhs_ty, &rhs_ty)
                };

                let lhs = cast_to(arena, lhs, &common_ty);
                arena[*left] = arena[lhs].clone();
                let rhs = cast_to(arena, rhs, &common_ty);
                arena[*right] = arena[rhs].clone();

                node_ty = int_type(false);
            }

            AstKind::Ternary {
                left,
                middle,
                right,
            } => {
                let (lhs, lhs_ty) =
                    self.annotate_and_convert(arena, symtab, *left)?;

                if !is_scalar_type(&lhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                arena[*left] = arena[lhs].clone();

                let (mhs, mhs_ty) = self.annotate(arena, symtab, *middle)?;

                if !is_scalar_type(&mhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                let (rhs, rhs_ty) = self.annotate(arena, symtab, *right)?;

                if !is_scalar_type(&rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                if !is_compatible(&mhs_ty, &rhs_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::IncompatibleTypes,
                    )));
                }

                let common_ty =
                    if is_pointer_type(&mhs_ty) || is_pointer_type(&rhs_ty) {
                        get_common_pointer_type(arena, mhs, rhs)?
                    } else {
                        get_common_type(&mhs_ty, &rhs_ty)
                    };

                let mhs = cast_to(arena, mhs, &common_ty);
                arena[*middle] = arena[mhs].clone();
                let rhs = cast_to(arena, rhs, &common_ty);
                arena[*right] = arena[rhs].clone();

                node_ty = common_ty.clone();
            }

            AstKind::Not { expr: inner } => {
                let (new_inner, inner_ty) =
                    self.annotate_and_convert(arena, symtab, *inner)?;

                if !is_scalar_type(&inner_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                arena[*inner] = arena[new_inner].clone();

                node_ty = int_type(true);
            }

            AstKind::Complement { expr: inner } => {
                let (new_inner, inner_ty) =
                    self.annotate_and_convert(arena, symtab, *inner)?;

                if !is_int_type(&inner_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedIntegerType,
                    )));
                }

                arena[*inner] = arena[new_inner].clone();

                node_ty = inner_ty.clone();
            }

            AstKind::AddrOf { expr: inner } => {
                let (new_inner, inner_ty) =
                    self.annotate(arena, symtab, *inner)?;

                arena[*inner] = arena[new_inner].clone();

                if !is_lvalue(arena, *inner) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::DereferencingRvalue,
                    )));
                }

                node_ty = pointer_type(&inner_ty.clone());
            }

            AstKind::Deref { expr: inner } => {
                let (new_inner, inner_ty) =
                    self.annotate_and_convert(arena, symtab, *inner)?;

                arena[*inner] = arena[new_inner].clone();

                let ty = base_type(&inner_ty).expect("expected a pointer");
                node_ty = ty.clone();
            }

            AstKind::Negate { expr: inner } => {
                let (new_inner, inner_ty) =
                    self.annotate_and_convert(arena, symtab, *inner)?;

                if !is_arithmetic_type(&inner_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedScalarType,
                    )));
                }

                arena[*inner] = arena[new_inner].clone();

                node_ty = inner_ty.clone();
            }

            AstKind::PreIncr { expr: inner }
            | AstKind::PreDecr { expr: inner }
            | AstKind::PostIncr { expr: inner }
            | AstKind::PostDecr { expr: inner } => {
                let (new_inner, inner_ty) =
                    self.annotate_and_convert(arena, symtab, *inner)?;

                if !is_arithmetic_type(&inner_ty) && !is_pointer_type(&inner_ty)
                {
                    return Err(error(TypeChecking(
                        TypeCheckingError::ExpectedArithmeticType,
                    )));
                }

                arena[*inner] = arena[new_inner].clone();

                node_ty = inner_ty.clone();
            }

            AstKind::Cast {
                type_spec,
                expr: inner,
            } => {
                let (new_inner, inner_ty) =
                    self.annotate_and_convert(arena, symtab, *inner)?;

                if let Some(ty_spec) = type_spec {
                    let (_, ty) = self.annotate(arena, symtab, *ty_spec)?;

                    if is_pointer_type(&inner_ty) && is_double_type(&ty) {
                        return Err(error(TypeChecking(
                            TypeCheckingError::CannotCastDoubleToPointer,
                        )));
                    }

                    if is_pointer_type(&ty) && is_double_type(&inner_ty) {
                        return Err(error(TypeChecking(
                            TypeCheckingError::CannotCastPointerToDouble,
                        )));
                    }

                    let new_inner = cast_to(arena, new_inner, &ty);
                    arena[*inner] = arena[new_inner].clone();
                    node_ty = ty.clone();
                }
            }

            AstKind::Call { expr: callee, args } => {
                let (new_callee, call_ty) =
                    self.annotate_and_convert(arena, symtab, *callee)?;
                arena[*callee] = arena[new_callee].clone();

                if let TypeKind::Function {
                    param_tys,
                    return_ty,
                } = &call_ty.clone().borrow().kind
                {
                    if args.len() > param_tys.len() {
                        return Err(error(TypeChecking(
                            TypeCheckingError::TooManyArguments,
                        )));
                    } else if args.len() < param_tys.len() {
                        return Err(error(TypeChecking(
                            TypeCheckingError::TooFewArguments,
                        )));
                    } else {
                        for (arg, param_ty) in args.iter().zip(param_tys.iter())
                        {
                            self.annotate_and_convert(arena, symtab, *arg)?;

                            let new_arg =
                                convert_by_assignment(arena, *arg, param_ty)?;
                            arena[*arg] = arena[new_arg].clone();
                        }
                        node_ty = return_ty.clone();
                    }
                }
            }

            AstKind::Array {
                type_spec,
                dimension: _,
                len,
            } => {
                let (new_type_spec, base_ty) =
                    self.annotate(arena, symtab, *type_spec)?;

                if is_function_type(&base_ty) {
                    return Err(error(TypeChecking(
                        TypeCheckingError::FunctionArray,
                    )));
                }

                arena[*type_spec] = arena[new_type_spec].clone();

                node_ty = array_type(&base_ty, len.get());
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

            AstKind::Initialiser { type_spec, value } => {
                let (_, type_spec_ty) =
                    self.annotate(arena, symtab, *type_spec)?;
                if let Some(expr) = value {
                    let (e, _ty) =
                        self.annotate_and_convert(arena, symtab, *expr)?;
                    let cast_expr =
                        convert_by_assignment(arena, e, &type_spec_ty)?;
                    arena[*expr] = arena[cast_expr].clone();
                }
                node_ty = type_spec_ty.clone();
            }

            AstKind::CompoundInitialiser(initialiser) => {
                let (_, type_spec_ty) =
                    self.annotate(arena, symtab, initialiser.type_spec)?;
                node_ty = type_spec_ty.clone();

                if initialiser.initialisers.len() == 0 {
                    return Err(error(TypeChecking(
                        TypeCheckingError::EmptyInitialiserList,
                    )));
                }

                let base_ty = if is_array_type(&type_spec_ty)
                    || is_pointer_type(&type_spec_ty)
                {
                    base_type(&type_spec_ty).ok_or(error(TypeChecking(
                        TypeCheckingError::EmptyInitialiserList,
                    )))?
                } else {
                    unreachable!();
                };

                for expr in &initialiser.initialisers {
                    let (e, ty) = self.annotate(arena, symtab, *expr)?;

                    if !is_compatible(&base_ty, &ty) {
                        return Err(error(TypeChecking(
                            TypeCheckingError::IncompatibleElementInArrayInitialiser,
                        )));
                    }

                    let cast_expr = cast_to(arena, e, &base_ty);
                    arena[*expr] = arena[cast_expr].clone();
                }
            }

            _ => {
                let ty = undefined_type();
                node_ty = ty.clone();
            }
        }

        arena[id].ty = node_ty.clone();

        Ok((id, node_ty.clone()))
    }

    fn type_convert(&mut self, ty: &TypeRef) -> Result<TypeRef, Error> {
        let node_ty = if is_array_type(ty) {
            let base_ty = base_type(ty).expect("array without basetype");
            pointer_type(&base_ty)
        } else {
            ty.clone()
        };

        Ok(node_ty.clone())
    }

    fn annotate_and_convert(
        &mut self,
        arena: &mut AstArena,
        symtab: &SymTab,
        id: AstId,
    ) -> Result<(AstId, TypeRef), Error> {
        let (node, ty) = self.annotate(arena, symtab, id)?;
        let conv_ty = self.type_convert(&ty)?;

        if is_array_type(&ty) {
            let inner = arena.alloc_clone(node);
            let scope = scope_of(arena, node);
            let addr_of = arena.alloc(
                AstKind::AddrOf { expr: inner },
                Some(conv_ty.clone()),
                scope,
            );
            arena[id] = arena[addr_of].clone();
            Ok((addr_of, conv_ty))
        } else {
            let _cast_node = cast_to(arena, node, &conv_ty);
            arena[id] = arena[_cast_node].clone();
            Ok((node, conv_ty.clone()))
        }
    }

    pub fn run(&mut self, stage: &mut AstStage) -> Result<(), Error> {
        if stage.root.is_empty() {
            return Ok(());
        }

        let scope = stage
            .symtab
            .upto(scope_of(&stage.arena, stage.root[0]), ScopeKind::File)
            .unwrap();

        for &node in &stage.root {
            self.annotate(&mut stage.arena, &stage.symtab, node)?;
        }

        for name in &self.decls {
            if let Some(decls) =
                stage.symtab.get_all_named_sym_decls(scope, name)
            {
                self.check_decls(&decls, &mut stage.arena, &stage.symtab)?;
            }
        }

        Ok(())
    }
}
