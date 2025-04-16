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

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Scope {
    parent: Option<ScopeRef>,
    offset: usize,
    pub depth: usize,
    symbols: HashMap<String, (usize, Rc<dyn Any>)>,
}

pub type ScopeRef = Rc<RefCell<Scope>>;

impl Scope {
    pub fn open(parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope {
            parent: parent.clone(),
            offset: if parent.is_some() {
                parent.clone().unwrap().as_ref().borrow().offset
            } else {
                0
            },
            depth: 0,
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

    pub fn add_sym(
        &mut self,
        name: &str,
        symbol: Rc<dyn Any>,
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
        self.symbols.insert(name.to_string(), (offset, symbol));

        Ok(())
    }

    pub fn find(&self, name: &str) -> Option<(usize, Rc<dyn Any>)> {
        let mut parent = self.parent.clone();

        if let Some((offset, sym)) = self.symbols.get(name) {
            return Some((*offset, sym.clone()));
        }

        while parent.is_some() {
            if let Some((offset, sym)) =
                parent.clone().unwrap().as_ref().borrow().symbols.get(name)
            {
                return Some((*offset, sym.clone()));
            }

            parent = parent.unwrap().as_ref().borrow().parent.clone();
        }

        None
    }

    pub fn find_sym(&self, name: &str) -> Option<Rc<dyn Any>> {
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
