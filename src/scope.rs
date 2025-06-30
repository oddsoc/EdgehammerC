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
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::*;

pub type ScopeRef = Rc<RefCell<Scope>>;
pub type SymRef = Rc<RefCell<Symbol>>;

#[derive(Debug, PartialEq)]
pub enum ScopeKind {
    Global,
    Function,
    Block,
    Loop,
    Switch,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Linkage {
    External,
    Internal,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Symbol {
    pub name: String,
    pub node: Option<Weak<RefCell<AST>>>,
    pub offset: usize,
    pub size: usize,
    pub alignment: usize,
    pub linkage: Option<Linkage>,
    pub defined: bool,
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    parent: Option<ScopeRef>,
    offset: usize,
    pub id: usize,
    pub depth: usize,

    labels: HashMap<String, Weak<RefCell<AST>>>,
    symbols: HashMap<String, SymRef>,
}

static SCOPE_ID: AtomicUsize = AtomicUsize::new(0);

fn next_scope_id() -> usize {
    SCOPE_ID.fetch_add(1, Ordering::SeqCst)
}

impl Scope {
    pub fn open(kind: ScopeKind, parent: Option<ScopeRef>) -> ScopeRef {
        if !matches!(kind, ScopeKind::Global) {
            assert!(parent.is_some());
        }

        Rc::new(RefCell::new(Scope {
            kind: kind,
            parent: parent.clone(),
            offset: if parent.is_some() {
                parent.clone().unwrap().as_ref().borrow().offset
            } else {
                0
            },
            id: next_scope_id(),
            depth: 0,
            labels: HashMap::new(),
            symbols: HashMap::new(),
        }))
    }

    pub fn close(&self) -> ScopeRef {
        self.parent.clone().unwrap()
    }

    pub fn update_depth(&self, depth: usize) {
        let mut parent = self.parent.clone();

        while parent.is_some() {
            if parent.clone().unwrap().as_ref().borrow_mut().depth < depth {
                parent.clone().unwrap().as_ref().borrow_mut().depth = depth;
            }
            parent = parent.unwrap().as_ref().borrow().parent.clone();
        }
    }

    pub fn add_tmp_var(&mut self, size: usize, alignment: usize) -> usize {
        assert!(alignment.is_power_of_two());
        self.offset = (self.offset + (alignment - 1)) & !(alignment - 1);
        let offset = self.offset;
        self.offset += size;
        self.depth = self.offset;

        self.update_depth(self.depth);

        offset
    }

    pub fn update_sym(
        &mut self,
        name: &str,
        node: ASTRef,
        defined: bool,
    ) -> Result<(), ()> {
        let linkage: Option<Linkage>;

        if let Some(sym) = self.symbols.get_mut(name) {
            let mut sym_mut = sym.borrow_mut();
            linkage = sym_mut.linkage.clone();
            if sym_mut.node.is_none() || linkage.is_none() {
                if defined {
                    sym_mut.defined = true;
                }
                sym_mut.node = Some(Rc::downgrade(&node));
            }
        } else {
            return Err(());
        }

        if matches!(linkage, Some(Linkage::External)) {
            let mut maybe_scope = self.parent.as_ref().map(Rc::clone);
            while let Some(scope_rc) = maybe_scope {
                let is_global = scope_rc.borrow().parent.is_none();

                if is_global {
                    let global = scope_rc.borrow();
                    if let Some(global_sym) = global.symbols.get(name) {
                        if defined {
                            global_sym.as_ref().borrow_mut().defined = true;
                        }

                        if global_sym.as_ref().borrow().node.is_some() {
                            break;
                        } else {
                            global_sym.as_ref().borrow_mut().node =
                                Some(Rc::downgrade(&node));
                        }
                    }
                    break;
                }

                maybe_scope = scope_rc.borrow().parent.as_ref().map(Rc::clone);
            }
        }

        Ok(())
    }

    pub fn add_sym(
        &mut self,
        name: &str,
        linkage: Option<Linkage>,
        node: Option<ASTRef>,
        size: usize,
        alignment: usize,
    ) -> Result<(), ()> {
        if let Some(sym) = self.symbols.get(name) {
            let sym_ref = sym.borrow();

            if compatible_linkage(&sym_ref.linkage, &linkage) {
                return Ok(());
            } else {
                return Err(());
            }
        }

        assert!(alignment.is_power_of_two());
        self.offset = (self.offset + (alignment - 1)) & !(alignment - 1);
        let offset = self.offset;
        self.offset += size;
        self.depth = self.offset;
        self.update_depth(self.depth);

        let symbol = Rc::new(RefCell::new(Symbol {
            name: name.to_string(),
            node: node.as_ref().map(Rc::downgrade),
            offset,
            size,
            alignment,
            linkage: linkage.clone(),
            defined: if let Some(Linkage::External) = linkage {
                false
            } else {
                true
            },
        }));

        self.symbols.insert(name.to_string(), Rc::clone(&symbol));

        if matches!(linkage, Some(Linkage::External)) {
            let mut maybe_scope = self.parent.as_ref().map(Rc::clone);

            while let Some(scope_rc) = maybe_scope {
                let is_global = scope_rc.borrow().parent.is_none();

                if is_global {
                    let mut global = scope_rc.borrow_mut();
                    if !global.symbols.contains_key(name) {
                        global
                            .symbols
                            .insert(name.to_string(), Rc::clone(&symbol));
                    }
                    break;
                }

                maybe_scope = scope_rc.borrow().parent.as_ref().map(Rc::clone);
            }
        }

        Ok(())
    }
}

fn compatible_linkage(a: &Option<Linkage>, b: &Option<Linkage>) -> bool {
    match (a, b) {
        (None, None) => false,
        (Some(x), Some(y)) => x == y,
        _ => false,
    }
}

pub fn add_label(
    scope: ScopeRef,
    name: &str,
    stmt: ASTRef,
) -> Result<(), String> {
    assert!(scope.as_ref().borrow().kind != ScopeKind::Global);

    if !matches!(scope.as_ref().borrow().kind, ScopeKind::Function) {
        add_label(
            scope.as_ref().borrow().parent.as_ref().unwrap().clone(),
            name,
            stmt,
        )
    } else if scope.borrow().labels.contains_key(name) {
        Err(format!("'{}' label already exists", name))
    } else {
        scope
            .borrow_mut()
            .labels
            .insert(name.to_string(), Rc::downgrade(&stmt));
        Ok(())
    }
}

pub fn loop_or_switch_scope(scope: ScopeRef) -> Option<ScopeRef> {
    if scope.as_ref().borrow().kind == ScopeKind::Loop
        || scope.borrow().kind == ScopeKind::Switch
    {
        Some(scope.clone())
    } else if scope.borrow().parent.is_some() {
        loop_or_switch_scope(
            scope.as_ref().borrow().parent.as_ref().unwrap().clone(),
        )
    } else {
        None
    }
}

pub fn loop_scope(scope: ScopeRef) -> Option<ScopeRef> {
    scope_of(scope, ScopeKind::Loop)
}

pub fn switch_scope(scope: ScopeRef) -> Option<ScopeRef> {
    scope_of(scope, ScopeKind::Switch)
}

fn scope_of(scope: ScopeRef, kind: ScopeKind) -> Option<ScopeRef> {
    if scope.as_ref().borrow().kind == kind {
        Some(scope.clone())
    } else if scope.borrow().parent.is_some() {
        scope_of(
            scope.as_ref().borrow().parent.as_ref().unwrap().clone(),
            kind,
        )
    } else {
        None
    }
}

pub fn find_label(scope: ScopeRef, name: &str) -> Option<ASTRef> {
    if let Some(sc) = scope_of(scope, ScopeKind::Function) {
        sc.as_ref()
            .borrow()
            .labels
            .get(name)
            .and_then(|w| w.upgrade())
    } else {
        None
    }
}

pub fn global_scope(scope: ScopeRef) -> ScopeRef {
    if let Some(parent) = scope.as_ref().borrow().parent.clone() {
        global_scope(parent.clone())
    } else {
        scope.clone()
    }
}

pub fn get_sym(
    scope: ScopeRef,
    name: &str,
    before: Option<ASTRef>,
) -> Option<SymRef> {
    if let Some(sym) = scope.as_ref().borrow().symbols.get(name) {
        if let Some(node) = before.as_ref() {
            if sym.as_ref().borrow().node.is_some() {
                let sym_node =
                    sym.as_ref().borrow().node.as_ref().unwrap().upgrade();
                if sym_node.as_ref().unwrap().borrow().preceeds(&node.borrow())
                {
                    return Some(sym.clone());
                }
            }
        } else {
            return Some(sym.clone());
        }
    }

    None
}

pub fn find_sym(
    scope: ScopeRef,
    name: &str,
    before: Option<ASTRef>,
) -> Option<SymRef> {
    let mut sym = get_sym(scope.clone(), name, before.clone());

    if sym.is_none() {
        if scope.as_ref().borrow().parent.is_some() {
            sym = find_sym(
                scope.as_ref().borrow().parent.as_ref().unwrap().clone(),
                name,
                before,
            );
        }
    }

    sym
}

#[allow(dead_code)]
fn find_node(
    scope: ScopeRef,
    name: &str,
    before: Option<ASTRef>,
) -> Option<ASTRef> {
    if let Some(sym) = find_sym(scope.clone(), name, before) {
        if let Some(sym_node) = &sym.borrow_mut().node {
            sym_node.upgrade()
        } else {
            None
        }
    } else {
        None
    }
}

pub fn find_offset(
    scope: ScopeRef,
    name: &str,
    before: Option<ASTRef>,
) -> Option<usize> {
    if let Some(sym) = find_sym(scope.clone(), name, before) {
        Some(sym.borrow_mut().offset)
    } else {
        None
    }
}

pub fn resolve(identifier: ASTRef) -> Option<SymRef> {
    let scope = identifier.borrow().scope.clone();

    match &identifier.borrow().kind {
        ASTKind::Identifier { name, sym: _ } => {
            return find_sym(scope, name, Some(identifier.clone()));
        }
        _ => return None,
    }
}
