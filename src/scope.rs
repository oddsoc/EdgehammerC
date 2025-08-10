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

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::ast::*;
use crate::types::*;

pub type ScopeRef = Rc<RefCell<Scope>>;
pub type SymRef = Rc<RefCell<Symbol>>;
pub type SymWeakRef = Weak<RefCell<Symbol>>;
pub type SymTab = HashMap<String, Vec<SymRef>>;
pub type Labels = HashMap<String, AstWeakRef>;

#[derive(Debug)]
pub struct Scope {
    next_id: Rc<Cell<usize>>,
    pub id: usize,
    parent: Option<ScopeRef>,
    kind: ScopeKind,
    pub symbols: SymTab,
    externs: Rc<RefCell<SymTab>>,
    labels: Labels,
    offset: usize,
    pub size: usize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ScopeKind {
    File,
    Function,
    Block,
    Loop,
    Switch,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymKind {
    Function,
    Parameter,
    Variable,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Symbol {
    pub at_scope: usize,
    pub kind: SymKind,
    pub name: String,
    pub node: Option<AstWeakRef>,
    pub pos: usize,
    pub size: usize,
    pub alignment: usize,
    pub linkage: Option<Linkage>,
    pub storage_class: Option<StorageClass>,
    pub definition: Option<Definition>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Tentative,
    Concrete,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Linkage {
    Internal,
    External,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
    Auto,
    Register,
}

pub trait SymTabOps {
    fn decls<'a>(&'a self, name: &'a str) -> Option<&'a Vec<SymRef>>;
    fn decl(&self, name: &str) -> Option<SymRef>;
    fn def(&self, name: &str) -> Option<SymRef>;
    fn defs(&self) -> Vec<SymRef>;
    fn has_concrete_def(&self, name: &str) -> bool;
    #[allow(dead_code)]
    fn has_extern_decl(&self, name: &str) -> bool;
    fn add_sym(&mut self, name: &str, sym: SymRef);
}

impl SymTabOps for SymTab {
    fn decls<'a>(&'a self, name: &'a str) -> Option<&'a Vec<SymRef>> {
        self.get(name)
    }

    fn decl(&self, name: &str) -> Option<SymRef> {
        if let Some(syms) = self.get(name) {
            if !syms.is_empty() {
                Some(syms[0].clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn def(&self, name: &str) -> Option<SymRef> {
        self.get(name).and_then(|syms| {
            syms.iter()
                .find(|s| {
                    matches!(s.borrow().definition, Some(Definition::Concrete))
                })
                .cloned()
                .or_else(|| {
                    syms.iter()
                        .find(|s| {
                            matches!(
                                s.borrow().definition,
                                Some(Definition::Tentative)
                            )
                        })
                        .cloned()
                })
        })
    }

    fn defs(&self) -> Vec<SymRef> {
        self.iter()
            .filter_map(|(_name, syms)| {
                syms.iter()
                    .find(|s| {
                        matches!(
                            s.borrow().definition,
                            Some(Definition::Concrete)
                        )
                    })
                    .cloned()
                    .or_else(|| {
                        syms.iter()
                            .find(|s| {
                                matches!(
                                    s.borrow().definition,
                                    Some(Definition::Tentative)
                                )
                            })
                            .cloned()
                    })
            })
            .collect()
    }

    fn has_concrete_def(&self, name: &str) -> bool {
        self.get(name).is_some_and(|syms| {
            syms.iter().any(|s| {
                matches!(s.borrow().definition, Some(Definition::Concrete))
            })
        })
    }

    #[allow(dead_code)]
    fn has_extern_decl(&self, name: &str) -> bool {
        self.get(name).is_some_and(|syms| {
            syms.iter()
                .any(|s| matches!(s.borrow().linkage, Some(Linkage::External)))
        })
    }

    fn add_sym(&mut self, name: &str, sym: SymRef) {
        self.entry(name.to_string()).or_default().push(sym.clone());
    }
}

pub fn new() -> ScopeRef {
    Rc::new(RefCell::new(Scope {
        next_id: Rc::new(Cell::new(1usize)),
        id: 0,
        parent: None,
        kind: ScopeKind::File,
        symbols: HashMap::new(),
        externs: Rc::new(RefCell::new(HashMap::new())),
        labels: HashMap::new(),
        offset: 0,
        size: 0,
    }))
}

pub fn open(scope: ScopeRef, kind: ScopeKind) -> ScopeRef {
    let id = scope.borrow().next_id.get();
    scope.borrow().next_id.set(id + 1);

    Rc::new(RefCell::new(Scope {
        next_id: scope.borrow().next_id.clone(),
        id,
        parent: Some(scope.clone()),
        kind,
        symbols: HashMap::new(),
        externs: get_externs(&scope),
        labels: HashMap::new(),
        offset: scope.borrow().offset,
        size: 0,
    }))
}

pub fn get_externs(scope: &ScopeRef) -> Rc<RefCell<SymTab>> {
    scope.borrow().externs.clone()
}

pub fn close(scope: &ScopeRef) -> ScopeRef {
    parent_of(scope)
}

pub fn parent_of(scope: &ScopeRef) -> ScopeRef {
    let sc = scope.borrow();

    assert!(sc.kind != ScopeKind::File);
    assert!(sc.parent.is_some());

    if let Some(parent) = sc.parent.as_ref() {
        parent.clone()
    } else {
        unreachable!();
    }
}

pub fn has_parent(scope: &ScopeRef) -> bool {
    let sc = scope.borrow();
    sc.parent.is_some()
}

pub fn kind_of(scope: &ScopeRef) -> ScopeKind {
    scope.borrow().kind
}

pub fn upto(mut scope: ScopeRef, kind: ScopeKind) -> Option<ScopeRef> {
    while kind_of(&scope) != kind {
        if has_parent(&scope) {
            scope = parent_of(&scope);
        } else {
            break;
        }
    }

    if kind_of(&scope) == kind {
        Some(scope.clone())
    } else {
        None
    }
}

pub fn upto_any(mut scope: ScopeRef, kinds: &[ScopeKind]) -> Option<ScopeRef> {
    let mut kind = kind_of(&scope);

    while !kinds.contains(&kind) {
        if has_parent(&scope) {
            scope = parent_of(&scope);
            kind = kind_of(&scope);
        } else {
            break;
        }
    }

    if kinds.contains(&kind) {
        Some(scope.clone())
    } else {
        None
    }
}

pub fn upto_top(mut scope: ScopeRef) -> ScopeRef {
    while kind_of(&scope) != ScopeKind::File {
        if has_parent(&scope) {
            scope = parent_of(&scope);
        } else {
            break;
        }
    }

    assert!(kind_of(&scope) == ScopeKind::File);

    scope.clone()
}

pub fn add_sym(
    scope: ScopeRef,
    name: &str,
    kind: SymKind,
    size: usize,
    alignment: usize,
    storage_class: Option<StorageClass>,
    definition: Option<Definition>,
    node: Option<AstRef>,
) -> Result<SymRef, String> {
    if let Some(StorageClass::Static) = storage_class {
        if has_parent(&scope) && matches!(kind, SymKind::Function) {
            return Err(format!(
                "invalid storage class for function '{}'",
                name
            ));
        }
    }

    let linkage = determine_linkage(&scope, kind, &storage_class);

    match linkage {
        Some(Linkage::External) => add_extern_sym(
            scope.clone(),
            name,
            kind,
            size,
            alignment,
            definition,
            node,
        ),

        Some(Linkage::Internal) => add_intern_sym(
            scope.clone(),
            name,
            kind,
            size,
            alignment,
            definition,
            node,
        ),

        None => {
            let at_file_scope = scope.borrow().id == 0;

            if at_file_scope {
                if let Some(Definition::Concrete) = definition {
                    if scope.borrow().symbols.has_concrete_def(name) {
                        return Err(format!(
                            "multiple definitions of {}",
                            name
                        ));
                    }
                }
            } else {
                if scope.borrow().symbols.decl(name).is_some() {
                    return Err(format!("multiple definitions of {}", name));
                }

                if let Some(decl) = get_sym(scope.clone(), name) {
                    let sym = decl.borrow();

                    if sym.linkage.is_some()
                        && sym.at_scope == scope.borrow().id
                    {
                        return Err(format!(
                            "redeclaration of {} with no linkage",
                            name
                        ));
                    }
                }
            }

            let pos = if let Some(StorageClass::Static) = storage_class {
                0
            } else {
                make_space(scope.clone(), size, alignment)
            };

            let mangled_name = if let Some(StorageClass::Static) = storage_class
            {
                format!("{}.{}", name, scope.borrow().id)
            } else {
                name.to_string()
            };

            let sym = Rc::new(RefCell::new(Symbol {
                at_scope: scope.borrow().id,
                kind,
                name: mangled_name.clone(),
                node: node.as_ref().map(Rc::downgrade),
                pos,
                size,
                alignment,
                linkage,
                storage_class,
                definition,
            }));

            scope.borrow_mut().symbols.add_sym(name, sym.clone());

            if let Some(StorageClass::Static) = storage_class {
                let top = upto_top(scope.clone());
                top.borrow_mut().symbols.add_sym(&mangled_name, sym.clone());
            }

            Ok(sym.clone())
        }
    }
}

pub fn get_sym(mut scope: ScopeRef, name: &str) -> Option<SymRef> {
    let initial_scope = scope.clone();

    loop {
        if let Some(sym) = scope.borrow().symbols.decl(name) {
            return Some(sym);
        }

        if has_parent(&scope) {
            scope = parent_of(&scope);
        } else {
            break;
        }
    }

    if let Some(decls) = get_externs(&scope).borrow().decls(name) {
        let syms: Vec<SymRef> = decls.to_vec();
        scope = initial_scope;

        loop {
            let scope_id = scope.borrow().id;

            for sym in &syms {
                if sym.borrow().at_scope == scope_id {
                    return Some(sym.clone());
                }
            }

            if has_parent(&scope) {
                scope = parent_of(&scope);
            } else {
                break;
            }
        }
    }

    None
}

pub fn get_all_named_sym_decls(
    mut scope: ScopeRef,
    name: &str,
) -> Option<Vec<SymRef>> {
    let mut syms: Vec<SymRef> = vec![];

    loop {
        if let Some(decls) = scope.borrow().symbols.decls(name) {
            syms = decls.to_vec();
            break;
        }

        if has_parent(&scope) {
            scope = parent_of(&scope);
        } else {
            break;
        }
    }

    if let Some(extern_syms) = get_externs(&scope).borrow().decls(name) {
        let extern_syms: Vec<SymRef> = extern_syms.to_vec();
        syms.extend(extern_syms);
    }

    if !syms.is_empty() { Some(syms) } else { None }
}

pub fn get_label(scope: ScopeRef, name: &str) -> Option<AstRef> {
    if let Some(s) = upto(scope.clone(), ScopeKind::Function) {
        return s
            .as_ref()
            .borrow()
            .labels
            .get(name)
            .and_then(|w| w.upgrade());
    }

    None
}

pub fn has_def(mut scope: ScopeRef, name: &str) -> bool {
    loop {
        if scope.borrow().symbols.def(name).is_some() {
            return true;
        }

        if has_parent(&scope) {
            scope = parent_of(&scope);
        } else {
            return false;
        }
    }
}

pub fn resolve(identifier: &AstRef) -> Option<SymRef> {
    let binding = identifier.borrow();

    match &binding.kind {
        AstKind::Identifier { sym, .. } => {
            sym.as_ref().and_then(|weak| weak.upgrade())
        }
        AstKind::Parameter { name, .. } => {
            if let Some(n) = name {
                get_sym(binding.scope.clone(), n)
            } else {
                None
            }
        }
        AstKind::Variable { name, .. } => get_sym(binding.scope.clone(), name),
        _ => None,
    }
}

pub fn has_linkage(sym: SymRef) -> bool {
    sym.borrow().linkage.is_some()
}

pub fn has_static_storage_duration(sym: SymRef) -> bool {
    if sym.borrow().at_scope == 0 {
        return true;
    }

    let storage_class = sym.borrow().storage_class;

    match storage_class {
        Some(StorageClass::Static) | Some(StorageClass::Extern) => {
            return true;
        }
        _ => {}
    }

    false
}

pub fn sym_as_node(sym: SymRef) -> Option<AstRef> {
    sym.as_ref()
        .borrow()
        .node
        .clone()
        .and_then(|weak| weak.upgrade())
}

#[allow(dead_code)]
pub fn sym_as_type(sym: SymRef) -> Option<TypeRef> {
    sym_as_node(sym.clone()).map(|node| node.borrow().ty.clone())
}

pub fn scope_of(ast: &AstRef) -> ScopeRef {
    ast.borrow().scope.clone()
}

fn align_to(value: usize, alignment: usize) -> usize {
    assert!(alignment.is_power_of_two());
    (value + (alignment - 1)) & !(alignment - 1)
}

pub fn make_space(mut scope: ScopeRef, size: usize, alignment: usize) -> usize {
    assert!(alignment.is_power_of_two());

    let offset;
    let depth;

    {
        let mut scope_mut = scope.borrow_mut();
        scope_mut.offset = align_to(scope_mut.offset + size, alignment);
        offset = scope_mut.offset;
        scope_mut.offset += size;
        scope_mut.size = scope_mut.offset;
        depth = scope_mut.size;
    }

    while has_parent(&scope) {
        scope = parent_of(&scope);
        let mut scope_mut = scope.borrow_mut();

        if scope_mut.size < depth {
            scope_mut.size = depth;
        }
    }

    assert!(align_to(offset, alignment) == offset);

    offset
}

fn add_extern_sym(
    scope: ScopeRef,
    name: &str,
    kind: SymKind,
    size: usize,
    alignment: usize,
    definition: Option<Definition>,
    node: Option<AstRef>,
) -> Result<SymRef, String> {
    let externs = get_externs(&scope);
    let top = upto_top(scope.clone());

    if let Some(Definition::Concrete) = definition {
        if has_parent(&scope) {
            return Err(format!(
                "extern definition of {} not allowed here",
                name
            ));
        } else if externs.borrow().has_concrete_def(name) {
            return Err(format!("multiple definitions of {}", name));
        } else if let Some(def) = top.borrow().symbols.def(name) {
            if let Some(StorageClass::Static) = def.borrow().storage_class {
                return Err(format!(
                    "non-static declaration of '{}' follows static declaration",
                    name
                ));
            }
        }
    }

    if let Some(decl) = scope.borrow().symbols.decl(name) {
        if decl.borrow().linkage.is_none() {
            return Err(format!(
                "extern declaration of {} follows declaration with no linkage",
                name
            ));
        }
    }

    let (linkage, storage_class) =
        if let Some(sym_def) = top.borrow().symbols.decl(name) {
            (sym_def.borrow().linkage, sym_def.borrow().storage_class)
        } else {
            (Some(Linkage::External), Some(StorageClass::Extern))
        };

    let sym = Rc::new(RefCell::new(Symbol {
        at_scope: scope.borrow().id,
        kind,
        name: name.to_string(),
        node: node.as_ref().map(Rc::downgrade),
        pos: 0,
        size,
        alignment,
        linkage,
        storage_class,
        definition,
    }));

    if let Some(Linkage::External) = linkage {
        externs.borrow_mut().add_sym(name, sym.clone());
    }

    scope.borrow_mut().symbols.add_sym(name, sym.clone());

    Ok(sym.clone())
}

fn add_intern_sym(
    scope: ScopeRef,
    name: &str,
    kind: SymKind,
    size: usize,
    alignment: usize,
    definition: Option<Definition>,
    node: Option<AstRef>,
) -> Result<SymRef, String> {
    let top = upto_top(scope.clone());
    let externs = get_externs(&scope);

    if let Some(decl) = externs.borrow().decl(name) {
        if let Some(Linkage::External) = decl.borrow().linkage {
            return Err(format!(
                "static declaration of '{}' follows non-static declaration",
                name
            ));
        }
    }

    if top.borrow().symbols.has_concrete_def(name) {
        if let Some(Definition::Concrete) = definition {
            return Err(format!("multiple definitions of {}", name));
        }
    }

    let sym = Rc::new(RefCell::new(Symbol {
        at_scope: top.borrow().id,
        kind,
        name: name.to_string(),
        node: node.as_ref().map(Rc::downgrade),
        pos: 0,
        size,
        alignment,
        linkage: Some(Linkage::Internal),
        storage_class: Some(StorageClass::Static),
        definition,
    }));

    top.borrow_mut().symbols.add_sym(name, sym.clone());

    if !has_parent(&scope) {
        externs.borrow_mut().add_sym(name, sym.clone());
    }

    Ok(sym.clone())
}

fn determine_linkage(
    scope: &ScopeRef,
    kind: SymKind,
    storage_class: &Option<StorageClass>,
) -> Option<Linkage> {
    match storage_class {
        Some(StorageClass::Static) => {
            if kind_of(scope) == ScopeKind::File {
                Some(Linkage::Internal)
            } else {
                None
            }
        }
        Some(StorageClass::Extern) => Some(Linkage::External),
        Some(StorageClass::Auto) | Some(StorageClass::Register) => {
            assert!(kind_of(scope) != ScopeKind::File);
            None
        }
        None => {
            if kind_of(scope) == ScopeKind::File
                || matches!(kind, SymKind::Function)
            {
                Some(Linkage::External)
            } else {
                None
            }
        }
    }
}

pub fn add_label(
    scope: ScopeRef,
    name: &str,
    stmt: AstRef,
) -> Result<(), String> {
    assert!(kind_of(&scope) != ScopeKind::File);

    if let Some(s) = upto(scope.clone(), ScopeKind::Function) {
        let mut scope_mut = s.borrow_mut();

        if scope_mut.labels.contains_key(name) {
            Err(format!("'{}' label already exists", name))
        } else {
            scope_mut
                .labels
                .insert(name.to_string(), Rc::downgrade(&stmt));

            Ok(())
        }
    } else {
        unreachable!();
    }
}
