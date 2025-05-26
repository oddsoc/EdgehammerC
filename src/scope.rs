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

use crate::ast::AST;

#[derive(Debug, PartialEq)]
pub enum ScopeKind {
    Global,
    Function,
    Block,
    Loop,
    Switch,
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    parent: Option<ScopeRef>,
    nesting: usize,
    offset: usize,
    pub id: usize,
    pub depth: usize,

    labels: HashMap<String, Weak<AST>>,
    symbols: HashMap<String, (usize, Weak<AST>)>,
}

pub type ScopeRef = Rc<RefCell<Scope>>;

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
            nesting: if parent.is_some() {
                parent.clone().unwrap().as_ref().borrow().nesting + 1
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

    pub fn add_tmp(&mut self, size: usize, alignment: usize) -> usize {
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
        symbol: Rc<AST>,
    ) -> Result<(), ()> {
        let entry = self.find(name);
        assert!(entry.is_some(), "symbol '{}' not found", name);
        self.symbols.insert(
            name.to_string(),
            (entry.unwrap().0, Rc::downgrade(&symbol)),
        );

        Ok(())
    }

    pub fn add_sym(
        &mut self,
        name: &str,
        symbol: Rc<AST>,
        size: usize,
        alignment: usize,
    ) -> Result<(), ()> {
        if self.symbols.contains_key(name) {
            return Err(());
        }

        assert!(alignment.is_power_of_two());
        self.offset = (self.offset + (alignment - 1)) & !(alignment - 1);
        let offset = self.offset;
        self.offset += size;
        self.depth = self.offset;
        self.update_depth(self.depth);
        self.symbols
            .insert(name.to_string(), (offset, Rc::downgrade(&symbol)));

        Ok(())
    }

    pub fn add_label(&mut self, name: &str, stmt: Rc<AST>) -> Result<(), ()> {
        if !matches!(self.kind, ScopeKind::Function) {
            return self
                .parent
                .as_ref()
                .unwrap()
                .borrow_mut()
                .add_label(name, stmt.clone());
        } else if self.labels.contains_key(name) {
            Err(())
        } else {
            self.labels.insert(name.to_string(), Rc::downgrade(&stmt));
            Ok(())
        }
    }

    pub fn loop_or_switch_scope(&self) -> Option<ScopeRef> {
        if self.kind == ScopeKind::Loop || self.kind == ScopeKind::Switch {
            // only search ancestors
            return None;
        }

        let switch_scope = self.scope_of(ScopeKind::Switch);
        let loop_scope = self.scope_of(ScopeKind::Loop);

        if switch_scope.is_some() {
            if loop_scope.is_some()
                && loop_scope.as_ref().unwrap().borrow().nesting
                    > switch_scope.as_ref().unwrap().borrow().nesting
            {
                return loop_scope;
            } else {
                return switch_scope;
            }
        }

        loop_scope
    }

    pub fn loop_scope(&self) -> Option<ScopeRef> {
        self.scope_of(ScopeKind::Loop)
    }

    pub fn switch_scope(&self) -> Option<ScopeRef> {
        self.scope_of(ScopeKind::Switch)
    }

    fn scope_of(&self, kind: ScopeKind) -> Option<ScopeRef> {
        if self.kind == kind {
            // only search ancestors
            return None;
        }

        let mut current = self.parent.clone();

        while let Some(parent_rc) = current {
            let parent_ref = parent_rc.borrow();
            if parent_ref.kind == kind {
                return Some(parent_rc.clone());
            }
            current = parent_ref.parent.clone();
        }

        None
    }

    pub fn find_label(&self, name: &str) -> Option<Rc<AST>> {
        if self.kind == ScopeKind::Function {
            return self.labels.get(name).and_then(|w| w.upgrade());
        }

        self.scope_of(ScopeKind::Function).and_then(|scope| {
            scope.borrow().labels.get(name).and_then(|w| w.upgrade())
        })
    }

    pub fn find(&self, name: &str) -> Option<(usize, Rc<AST>)> {
        let mut parent = self.parent.clone();

        if let Some((offset, sym_weak)) = self.symbols.get(name) {
            if let Some(sym) = sym_weak.upgrade() {
                return Some((*offset, sym));
            }
        }

        while parent.is_some() {
            if let Some((offset, sym_weak)) =
                parent.clone().unwrap().as_ref().borrow().symbols.get(name)
            {
                if let Some(sym) = sym_weak.upgrade() {
                    return Some((*offset, sym));
                }
            }

            parent = parent.unwrap().as_ref().borrow().parent.clone();
        }

        None
    }

    pub fn find_sym(&self, name: &str) -> Option<Rc<AST>> {
        let entry = self.find(name);

        if entry.is_some() {
            Some(entry.unwrap().1.clone())
        } else {
            None
        }
    }

    #[allow(dead_code)]
    pub fn find_off(&self, name: &str) -> Option<usize> {
        let entry = self.find(name);

        if entry.is_some() {
            Some(entry.unwrap().0.clone())
        } else {
            None
        }
    }
}
