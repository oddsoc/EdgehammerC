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

use std::collections::HashMap;

use crate::ast::*;
use crate::errors::{
    Error,
    ErrorClass::{Parsing, Symbolic},
    ParsingError, SymbolError, error,
};
use crate::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymId(pub usize);

#[derive(Debug, Clone, Default)]
pub struct SymTab {
    pub arena: Vec<Symbol>,
    pub entries: HashMap<String, Vec<SymId>>,
    pub scopes: ScopeArena,
}

impl SymTab {
    pub fn new() -> Self {
        SymTab {
            arena: Vec::new(),
            entries: HashMap::new(),
            scopes: ScopeArena::new(),
        }
    }

    fn best_def_from(&self, syms: &[SymId]) -> Option<SymId> {
        syms.iter()
            .find(|&&id| {
                matches!(self[id].definition, Some(Definition::Concrete))
            })
            .copied()
            .or_else(|| {
                syms.iter()
                    .find(|&&id| {
                        matches!(
                            self[id].definition,
                            Some(Definition::Tentative)
                        )
                    })
                    .copied()
            })
    }

    fn add_sym_entry(&mut self, name: &str, sym: SymId) {
        self.entries.entry(name.to_string()).or_default().push(sym);
    }
}

impl std::ops::Index<SymId> for SymTab {
    type Output = Symbol;

    fn index(&self, id: SymId) -> &Symbol {
        &self.arena[id.0]
    }
}

impl std::ops::IndexMut<SymId> for SymTab {
    fn index_mut(&mut self, id: SymId) -> &mut Symbol {
        &mut self.arena[id.0]
    }
}

impl std::ops::Index<ScopeId> for SymTab {
    type Output = Scope;

    fn index(&self, id: ScopeId) -> &Scope {
        &self.scopes.arena[id.0]
    }
}

impl std::ops::IndexMut<ScopeId> for SymTab {
    fn index_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes.arena[id.0]
    }
}

pub type Labels = HashMap<String, AstId>;

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
    kind: ScopeKind,
    name_map: HashMap<String, String>,
    labels: Labels,
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
    Variable,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Symbol {
    pub at_scope: ScopeId,
    pub kind: SymKind,
    pub name: String,
    pub node: Option<AstId>,
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

#[derive(Debug, Clone, Default)]
pub struct ScopeArena {
    pub arena: Vec<Scope>,
    next_id: usize,
}

impl ScopeArena {
    pub fn new() -> Self {
        let arena = vec![Scope {
            parent: None,
            kind: ScopeKind::File,
            name_map: HashMap::new(),
            labels: HashMap::new(),
        }];
        ScopeArena { arena, next_id: 1 }
    }

    pub fn open(&mut self, id: ScopeId, kind: ScopeKind) -> ScopeId {
        let new_id = ScopeId(self.next_id);
        self.next_id += 1;

        self.arena.push(Scope {
            parent: Some(id),
            kind,
            name_map: HashMap::new(),
            labels: HashMap::new(),
        });
        new_id
    }

    pub fn close(&self, id: ScopeId) -> ScopeId {
        self.parent_of(id)
    }

    pub fn parent_of(&self, id: ScopeId) -> ScopeId {
        assert!(self.arena[id.0].kind != ScopeKind::File);
        self.arena[id.0]
            .parent
            .expect("non-file scope must have a parent")
    }

    pub fn has_parent(&self, id: ScopeId) -> bool {
        self.arena[id.0].parent.is_some()
    }

    pub fn kind_of(&self, id: ScopeId) -> ScopeKind {
        self.arena[id.0].kind
    }

    pub fn upto(&self, id: ScopeId, kind: ScopeKind) -> Option<ScopeId> {
        self.upto_any(id, &[kind])
    }

    pub fn upto_any(
        &self,
        mut id: ScopeId,
        kinds: &[ScopeKind],
    ) -> Option<ScopeId> {
        loop {
            if kinds.contains(&self.kind_of(id)) {
                return Some(id);
            }
            if self.has_parent(id) {
                id = self.parent_of(id);
            } else {
                return None;
            }
        }
    }

    pub fn upto_top(&self, mut id: ScopeId) -> ScopeId {
        while self.kind_of(id) != ScopeKind::File {
            id = self.parent_of(id);
        }
        id
    }

    pub fn add_label(
        &mut self,
        id: ScopeId,
        name: &str,
        stmt: AstId,
    ) -> Result<(), Error> {
        assert!(self.kind_of(id) != ScopeKind::File);

        let fn_scope = self
            .upto(id, ScopeKind::Function)
            .expect("label must be inside a function scope");

        if self.arena[fn_scope.0].labels.contains_key(name) {
            Err(error(Parsing(ParsingError::LabelAlreadyDefined(
                name.to_string(),
            ))))
        } else {
            self.arena[fn_scope.0].labels.insert(name.to_string(), stmt);
            Ok(())
        }
    }

    pub fn get_label(&self, id: ScopeId, name: &str) -> Option<AstId> {
        self.upto(id, ScopeKind::Function)
            .and_then(|s| self.arena[s.0].labels.get(name).copied())
    }
}

impl std::ops::Index<ScopeId> for ScopeArena {
    type Output = Scope;

    fn index(&self, id: ScopeId) -> &Scope {
        &self.arena[id.0]
    }
}

impl std::ops::IndexMut<ScopeId> for ScopeArena {
    fn index_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.arena[id.0]
    }
}

impl SymTab {
    pub fn open_scope(&mut self, id: ScopeId, kind: ScopeKind) -> ScopeId {
        self.scopes.open(id, kind)
    }

    pub fn close_scope(&self, id: ScopeId) -> ScopeId {
        self.scopes.close(id)
    }

    pub fn parent_of(&self, id: ScopeId) -> ScopeId {
        self.scopes.parent_of(id)
    }

    pub fn has_parent(&self, id: ScopeId) -> bool {
        self.scopes.has_parent(id)
    }

    pub fn upto(&self, id: ScopeId, kind: ScopeKind) -> Option<ScopeId> {
        self.scopes.upto(id, kind)
    }

    pub fn upto_any(
        &self,
        id: ScopeId,
        kinds: &[ScopeKind],
    ) -> Option<ScopeId> {
        self.scopes.upto_any(id, kinds)
    }

    pub fn upto_top(&self, id: ScopeId) -> ScopeId {
        self.scopes.upto_top(id)
    }

    pub fn add_label(
        &mut self,
        id: ScopeId,
        name: &str,
        stmt: AstId,
    ) -> Result<(), Error> {
        self.scopes.add_label(id, name, stmt)
    }

    pub fn get_label(&self, id: ScopeId, name: &str) -> Option<AstId> {
        self.scopes.get_label(id, name)
    }

    fn current_scope_decl(&self, id: ScopeId, name: &str) -> Option<SymId> {
        let sc = &self.scopes.arena[id.0];
        let unique = sc.name_map.get(name)?.clone();
        self.decl(&unique)
    }

    fn determine_linkage(
        &self,
        id: ScopeId,
        kind: SymKind,
        storage_class: &Option<StorageClass>,
    ) -> Option<Linkage> {
        match storage_class {
            Some(StorageClass::Static) => {
                if self.scopes.kind_of(id) == ScopeKind::File {
                    Some(Linkage::Internal)
                } else {
                    None
                }
            }
            Some(StorageClass::Extern) => Some(Linkage::External),
            Some(StorageClass::Auto) | Some(StorageClass::Register) => {
                assert!(self.scopes.kind_of(id) != ScopeKind::File);
                None
            }
            None => {
                if self.scopes.kind_of(id) == ScopeKind::File
                    || matches!(kind, SymKind::Function)
                {
                    Some(Linkage::External)
                } else {
                    None
                }
            }
        }
    }

    pub fn declare_sym(
        &mut self,
        id: ScopeId,
        name: &str,
        kind: SymKind,
        storage_class: Option<StorageClass>,
        definition: Option<Definition>,
        node: Option<AstId>,
    ) -> Result<SymId, Error> {
        if matches!(storage_class, Some(StorageClass::Static))
            && self.scopes.has_parent(id)
            && matches!(kind, SymKind::Function)
        {
            return Err(error(Symbolic(
                SymbolError::InvalidStorageClassForFunction(name.to_string()),
            )));
        }

        let linkage = self.determine_linkage(id, kind, &storage_class);

        match linkage {
            Some(linkage) => self.add_linked_sym(
                id,
                name,
                kind,
                storage_class,
                definition,
                node,
                linkage,
            ),
            None => self.add_local_sym(
                id,
                name,
                kind,
                storage_class,
                definition,
                node,
            ),
        }
    }

    fn add_linked_sym(
        &mut self,
        id: ScopeId,
        name: &str,
        kind: SymKind,
        storage_class: Option<StorageClass>,
        definition: Option<Definition>,
        node: Option<AstId>,
        linkage: Linkage,
    ) -> Result<SymId, Error> {
        {
            if let Some(Definition::Concrete) = definition {
                if self.scopes.has_parent(id) {
                    return Err(error(Symbolic(
                        SymbolError::InvalidStorageClassForFunction(format!(
                            "extern definition of {} not allowed here",
                            name
                        )),
                    )));
                }
                if self.has_concrete_def(name) {
                    return Err(error(Symbolic(
                        SymbolError::MultipleDefinitions(name.to_string()),
                    )));
                }
                if let Some(def) = self.def(name) {
                    if let Some(StorageClass::Static) = self[def].storage_class
                    {
                        return Err(error(Symbolic(
                            SymbolError::InvalidStorageClassForFunction(
                                format!(
                                    "non-static declaration of '{}' follows static declaration",
                                    name
                                ),
                            ),
                        )));
                    }
                }
            }

            if linkage == Linkage::Internal {
                if let Some(existing) = self.decl(name) {
                    if let Some(Linkage::External) = self[existing].linkage {
                        return Err(error(Symbolic(
                            SymbolError::InvalidStorageClassForFunction(
                                format!(
                                    "static declaration of '{}' follows non-static declaration",
                                    name
                                ),
                            ),
                        )));
                    }
                }
            }

            if let Some(decl) = self.current_scope_decl(id, name) {
                match self[decl].linkage {
                    Some(Linkage::Internal) => {
                        if storage_class.is_none() && kind != SymKind::Function
                        {
                            return Err(error(Symbolic(
                                SymbolError::InvalidStorageClassForFunction(
                                    format!(
                                        "extern declaration of {} follows declaration with \
                                     internal linkage",
                                        name
                                    ),
                                ),
                            )));
                        }
                    }
                    None => {
                        return Err(error(Symbolic(
                            SymbolError::InvalidStorageClassForFunction(
                                format!(
                                    "extern declaration of {} follows declaration with no \
                                 linkage",
                                    name
                                ),
                            ),
                        )));
                    }
                    _ => {}
                }
            }
        }

        let (final_linkage, final_storage_class) =
            if let Some(existing) = self.decl(name) {
                (self[existing].linkage, self[existing].storage_class)
            } else {
                (Some(linkage), storage_class)
            };

        let sym_id = SymId(self.arena.len());
        self.arena.push(Symbol {
            at_scope: id,
            kind,
            name: name.to_string(),
            node,
            linkage: final_linkage,
            storage_class: final_storage_class,
            definition,
        });
        self.add_sym_entry(name, sym_id);

        self.scopes.arena[id.0]
            .name_map
            .insert(name.to_string(), name.to_string());

        Ok(sym_id)
    }

    fn add_local_sym(
        &mut self,
        id: ScopeId,
        name: &str,
        kind: SymKind,
        storage_class: Option<StorageClass>,
        definition: Option<Definition>,
        node: Option<AstId>,
    ) -> Result<SymId, Error> {
        let scope_id = id;

        if self.scopes.arena[id.0].name_map.contains_key(name) {
            return Err(error(Symbolic(SymbolError::MultipleDefinitions(
                name.to_string(),
            ))));
        }

        if let Some(sym) = self.get_sym(id, name) {
            let s = &self[sym];
            if s.linkage.is_some() && s.at_scope == scope_id {
                return Err(error(Symbolic(
                    SymbolError::RedeclarationWithNoLinkage(name.to_string()),
                )));
            }
        }

        let unique_name = format!("{}.{}", name, scope_id.0);

        let sym_id = SymId(self.arena.len());
        self.arena.push(Symbol {
            at_scope: scope_id,
            kind,
            name: unique_name.clone(),
            node,
            linkage: None,
            storage_class,
            definition,
        });
        self.add_sym_entry(&unique_name, sym_id);

        self.scopes.arena[id.0]
            .name_map
            .insert(name.to_string(), unique_name.clone());

        Ok(sym_id)
    }

    pub fn update_sym(
        &mut self,
        id: ScopeId,
        sym: SymId,
        definition: Option<Definition>,
    ) -> Result<(), Error> {
        let (linkage, name) = { (self[sym].linkage, self[sym].name.clone()) };

        match linkage {
            Some(Linkage::External) => {
                if let Some(Definition::Concrete) = definition {
                    if self.scopes.has_parent(id) {
                        return Err(error(Symbolic(
                            SymbolError::InvalidStorageClassForFunction(
                                format!(
                                    "extern definition of {} not allowed here",
                                    name
                                ),
                            ),
                        )));
                    }
                    if self.has_concrete_def(&name) {
                        return Err(error(Symbolic(
                            SymbolError::MultipleDefinitions(name.clone()),
                        )));
                    }
                    if let Some(def) = self.def(&name) {
                        if let Some(StorageClass::Static) =
                            self[def].storage_class
                        {
                            return Err(error(Symbolic(
                                SymbolError::InvalidStorageClassForFunction(
                                    format!(
                                        "non-static declaration of '{}' follows static \
                                     declaration",
                                        name
                                    ),
                                ),
                            )));
                        }
                    }
                }
                if let Some(decl) = self.current_scope_decl(id, &name) {
                    if self[decl].linkage.is_none() {
                        return Err(error(Symbolic(
                            SymbolError::InvalidStorageClassForFunction(
                                format!(
                                    "extern declaration of {} follows declaration with \
                                 no linkage",
                                    name
                                ),
                            ),
                        )));
                    }
                }

                let (final_linkage, final_storage_class) =
                    if let Some(existing) = self.decl(&name) {
                        (self[existing].linkage, self[existing].storage_class)
                    } else {
                        (Some(Linkage::External), Some(StorageClass::Extern))
                    };

                self[sym].linkage = final_linkage;
                self[sym].storage_class = final_storage_class;
                self[sym].definition = definition;
                Ok(())
            }

            Some(Linkage::Internal) => {
                if let Some(existing) = self.decl(&name) {
                    if let Some(Linkage::External) = self[existing].linkage {
                        return Err(error(Symbolic(
                            SymbolError::InvalidStorageClassForFunction(
                                format!(
                                    "static declaration of '{}' follows non-static \
                                 declaration",
                                    name
                                ),
                            ),
                        )));
                    }
                }
                if self.has_concrete_def(&name) {
                    if let Some(Definition::Concrete) = definition {
                        return Err(error(Symbolic(
                            SymbolError::MultipleDefinitions(name.clone()),
                        )));
                    }
                }

                self[sym].linkage = Some(Linkage::Internal);
                self[sym].storage_class = Some(StorageClass::Static);
                self[sym].definition = definition;
                Ok(())
            }

            None => {
                if self.scopes.kind_of(id) == ScopeKind::File {
                    if let Some(Definition::Concrete) = definition {
                        if self.has_concrete_def(&name) {
                            return Err(error(Symbolic(
                                SymbolError::MultipleDefinitions(name.clone()),
                            )));
                        }
                    }
                } else {
                    if let Some(existing) = self.get_sym(id, &name) {
                        if self[existing].linkage.is_some()
                            && self[existing].at_scope == id
                        {
                            return Err(error(Symbolic(
                                SymbolError::RedeclarationWithNoLinkage(
                                    name.clone(),
                                ),
                            )));
                        }
                    }
                }

                self[sym].definition = definition;
                Ok(())
            }
        }
    }

    pub fn get_sym(&self, mut id: ScopeId, name: &str) -> Option<SymId> {
        loop {
            if let Some(unique) =
                self.scopes.arena[id.0].name_map.get(name).cloned()
            {
                return self.decl(&unique);
            }
            if self.scopes.has_parent(id) {
                id = self.scopes.parent_of(id);
            } else {
                break;
            }
        }

        None
    }

    pub fn get_all_named_sym_decls(
        &self,
        mut id: ScopeId,
        name: &str,
    ) -> Option<Vec<SymId>> {
        loop {
            if let Some(unique) =
                self.scopes.arena[id.0].name_map.get(name).cloned()
            {
                return self.decls(&unique).map(|v| v.clone());
            }
            if self.scopes.has_parent(id) {
                id = self.scopes.parent_of(id);
            } else {
                break;
            }
        }

        None
    }

    pub fn has_def(&self, mut id: ScopeId, name: &str) -> bool {
        loop {
            if let Some(unique) =
                self.scopes.arena[id.0].name_map.get(name).cloned()
            {
                return self.def(&unique).is_some();
            }
            if self.scopes.has_parent(id) {
                id = self.scopes.parent_of(id);
            } else {
                return false;
            }
        }
    }
}

pub trait SymTabOps {
    fn decls<'a>(&'a self, name: &'a str) -> Option<&'a Vec<SymId>>;
    fn decl(&self, name: &str) -> Option<SymId>;
    fn def(&self, name: &str) -> Option<SymId>;
    fn defs(&self) -> Vec<SymId>;
    fn has_concrete_def(&self, name: &str) -> bool;
    #[allow(dead_code)]
    fn has_extern_decl(&self, name: &str) -> bool;
}

impl SymTabOps for SymTab {
    fn decls<'a>(&'a self, name: &'a str) -> Option<&'a Vec<SymId>> {
        self.entries.get(name)
    }

    fn decl(&self, name: &str) -> Option<SymId> {
        self.entries
            .get(name)
            .and_then(|syms| syms.first().copied())
    }

    fn def(&self, name: &str) -> Option<SymId> {
        self.entries
            .get(name)
            .and_then(|syms| self.best_def_from(syms))
    }

    fn defs(&self) -> Vec<SymId> {
        self.entries
            .values()
            .filter_map(|syms| self.best_def_from(syms))
            .collect()
    }

    fn has_concrete_def(&self, name: &str) -> bool {
        self.entries.get(name).is_some_and(|syms| {
            syms.iter().any(|&id| {
                matches!(self[id].definition, Some(Definition::Concrete))
            })
        })
    }

    #[allow(dead_code)]
    fn has_extern_decl(&self, name: &str) -> bool {
        self.entries.get(name).is_some_and(|syms| {
            syms.iter()
                .any(|&id| matches!(self[id].linkage, Some(Linkage::External)))
        })
    }
}

pub fn resolve(symtab: &SymTab, arena: &AstArena, id: &AstId) -> Option<SymId> {
    let binding = &arena[*id];

    match &binding.kind {
        AstKind::Identifier { sym, .. } => *sym,
        AstKind::Variable { name, .. } => {
            symtab.get_sym(binding.scope, arena.token_str(name))
        }
        AstKind::Parameter { name, .. } => {
            symtab.get_sym(binding.scope, arena.token_str(name))
        }
        AstKind::Cast { expr, .. } => resolve(symtab, arena, expr),
        _ => None,
    }
}

pub fn has_linkage(symtab: &SymTab, sym: SymId) -> bool {
    symtab[sym].linkage.is_some()
}

pub fn has_static_storage_duration(symtab: &SymTab, sym: SymId) -> bool {
    let s = &symtab[sym];
    s.at_scope == ScopeId(0)
        || matches!(
            s.storage_class,
            Some(StorageClass::Static) | Some(StorageClass::Extern)
        )
}

pub fn sym_as_node(symtab: &SymTab, sym: SymId) -> Option<AstId> {
    symtab[sym].node
}

#[allow(dead_code)]
pub fn sym_as_type(
    arena: &AstArena,
    symtab: &SymTab,
    sym: SymId,
) -> Option<TypeRef> {
    sym_as_node(symtab, sym).map(|id| arena[id].ty.clone())
}

pub fn scope_of(arena: &AstArena, id: AstId) -> ScopeId {
    arena[id].scope
}
