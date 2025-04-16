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
use std::rc::Rc;

use crate::expr::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::scope::*;

macro_rules! accept {
    ($parser:expr, $kind:path) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind) {
            $parser.advance();
            true
        } else {
            false
        }
    };

    ($parser:expr, $kind:path, $_:tt) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind(_)) {
            $parser.advance();
            true
        } else {
            false
        }
    };
}

macro_rules! expect {
    ($parser:expr, $kind:path) => {
        if !matches!($parser.next.as_ref().unwrap().kind, $kind) {
            let msg = format!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.next.as_ref().unwrap().kind
            );
            return Err(msg);
        } else {
            $parser.advance();
        }
    };

    ($parser:expr, $kind:path, $_:tt) => {
        if !matches!($parser.next.as_ref().unwrap().kind, $kind(_)) {
            let msg = format!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.next.as_ref().unwrap().kind
            );
            return Err(msg);
        } else {
            $parser.advance();
        }
    };
}

macro_rules! peek {
    ($parser:expr, $kind:path) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind) {
            true
        } else {
            false
        }
    };

    ($parser:expr, $kind:path, $_:tt) => {
        if matches!($parser.next.as_ref().unwrap().kind, $kind(_)) {
            true
        } else {
            false
        }
    };
}

macro_rules! peek_any {
    ($parser:expr) => {
        $parser.next.as_ref().unwrap().kind.clone()
    };
}

macro_rules! yank {
    ($parser:expr, $kind:path) => {
        match &$parser.last.as_ref().unwrap().kind {
            $kind(data) => data.clone(),
            _ => panic!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.last.as_ref().unwrap().kind
            ),
        }
    };
}

macro_rules! resolve {
    ($parser:expr, $name:expr) => {
        $parser
            .scope
            .as_ref()
            .borrow()
            .find_sym($name)
            .and_then(|outer_rc| {
                outer_rc
                    .downcast::<Rc<RefCell<AST>>>()
                    .ok()
                    .map(|inner_rc| (*inner_rc).clone())
            })
    };
}

#[allow(unused_macros)]
macro_rules! is_a {
    ($symbol:expr, $variant:pat) => {
        $symbol
            .as_ref()
            .map(|symbol| matches!(&symbol.borrow().kind, $variant))
            .unwrap_or(false)
    };
}

#[macro_export]
macro_rules! as_a {
    ($opt_rc:expr, $pat:pat => $body:block else $else_block:block) => {
        if let Some(ref inner_rc) = $opt_rc {
            let borrow = inner_rc.borrow();
            if let $pat = &borrow.kind $body else $else_block
        } else $else_block
    };

    ($opt_rc:expr, $pat:pat => $body:block) => {
        if let Some(ref inner_rc) = $opt_rc {
            let borrow = inner_rc.borrow();
            if let $pat = &borrow.kind $body
        }
    };
}

macro_rules! as_a_mut {
    ($opt_rc:expr, $pat:pat => $body:block else $else_block:block) => {
        if let Some(ref inner_rc) = $opt_rc {
            let mut borrow = inner_rc.borrow_mut();
            if let $pat = &mut borrow.kind $body else $else_block
        } else $else_block
    };

    ($opt_rc:expr, $pat:pat => $body:block) => {
        if let Some(ref inner_rc) = $opt_rc {
            let mut borrow = inner_rc.borrow_mut();
            if let $pat = &mut borrow.kind $body
        }
    };
}

macro_rules! new_node {
    ($parser:expr, $variant:ident) => {
        Rc::new(RefCell::new(AST { kind: ASTKind::$variant, scope: $parser.scope.clone() }))
    };

    ($parser:expr, $variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(RefCell::new(AST { kind: ASTKind::$variant( $($args),* ), scope: $parser.scope.clone() }))
    };

    ($parser:expr, $variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(RefCell::new(AST { kind: ASTKind::$variant {
            $($field: $value),*
        }, scope: $parser.scope.clone() }))
    };
}

#[derive(Debug)]
pub struct AST {
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
        params: ASTVec,
        block: Option<ASTRef>,
        rtype: ASTRef,
        scope: ScopeRef,
    },
    Block {
        body: ASTVec,
    },
    Variable {
        name: String,
        typ: ASTRef,
        init: Option<ASTRef>,
    },
    Identifier {
        name: String,
    },
    Return {
        expr: ASTRef,
    },
    ExprStmt {
        expr: ASTRef,
    },
    Complement {
        expr: ASTRef,
    },
    Negate {
        expr: ASTRef,
    },
    Not {
        expr: ASTRef,
    },
    PreIncr {
        expr: ASTRef,
    },
    PreDecr {
        expr: ASTRef,
    },
    PostIncr {
        expr: ASTRef,
    },
    PostDecr {
        expr: ASTRef,
    },
    Add {
        left: ASTRef,
        right: ASTRef,
    },
    Subtract {
        left: ASTRef,
        right: ASTRef,
    },
    Multiply {
        left: ASTRef,
        right: ASTRef,
    },
    Divide {
        left: ASTRef,
        right: ASTRef,
    },
    Modulo {
        left: ASTRef,
        right: ASTRef,
    },
    LShift {
        left: ASTRef,
        right: ASTRef,
    },
    RShift {
        left: ASTRef,
        right: ASTRef,
    },
    And {
        left: ASTRef,
        right: ASTRef,
    },
    Or {
        left: ASTRef,
        right: ASTRef,
    },
    Xor {
        left: ASTRef,
        right: ASTRef,
    },
    LogicAnd {
        left: ASTRef,
        right: ASTRef,
    },
    LogicOr {
        left: ASTRef,
        right: ASTRef,
    },
    Equal {
        left: ASTRef,
        right: ASTRef,
    },
    NotEq {
        left: ASTRef,
        right: ASTRef,
    },
    LessThan {
        left: ASTRef,
        right: ASTRef,
    },
    LessOrEq {
        left: ASTRef,
        right: ASTRef,
    },
    GreaterThan {
        left: ASTRef,
        right: ASTRef,
    },
    GreaterOrEq {
        left: ASTRef,
        right: ASTRef,
    },
    Assign {
        left: ASTRef,
        right: ASTRef,
    },
}

pub type ASTRef = Rc<RefCell<AST>>;
pub type ASTVec = Vec<ASTRef>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    last: Option<Token>,
    next: Option<Token>,
    scope: ScopeRef,
    validate: bool,
}

fn precedence_of(kind: &TokenKind) -> i32 {
    return match kind {
        TokenKind::Mult => 55,
        TokenKind::Div => 55,
        TokenKind::Mod => 55,
        TokenKind::Plus => 50,
        TokenKind::Minus => 50,
        TokenKind::LShift => 45,
        TokenKind::RShift => 45,
        TokenKind::LThan => 40,
        TokenKind::LThanEq => 40,
        TokenKind::GThan => 40,
        TokenKind::GThanEq => 40,
        TokenKind::Eq => 35,
        TokenKind::NotEq => 35,
        TokenKind::And => 30,
        TokenKind::Xor => 25,
        TokenKind::Or => 20,
        TokenKind::LAnd => 15,
        TokenKind::LOr => 10,
        // ?: => 5
        TokenKind::Assign => 0,
        TokenKind::PlusEq => 0,
        TokenKind::MinusEq => 0,
        TokenKind::MultEq => 0,
        TokenKind::DivEq => 0,
        TokenKind::ModEq => 0,
        TokenKind::OrEq => 0,
        TokenKind::AndEq => 0,
        TokenKind::InvEq => 0,
        _ => 0,
    };
}

impl Parser {
    pub fn new(filename: &str, validate: bool) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(filename),
            last: None,
            next: None,
            scope: Scope::open(None),
            validate: validate,
        };

        parser.advance();

        return parser;
    }

    fn open_scope(&mut self) {
        self.scope = Scope::open(Some(self.scope.clone()));
    }

    fn close_scope(&mut self) {
        self.scope = self.scope.clone().borrow().close();
    }

    fn add_sym(&mut self, name: &str, sym: ASTRef) -> Result<(), String> {
        let s = self.scope.as_ref().borrow_mut().add_sym(
            &name,
            Rc::new(sym.clone()) as Rc<dyn Any>,
            4,
            4,
        );

        if self.validate && s.is_err() {
            Err(format!("'{}' is already defined", name))
        } else {
            Ok(())
        }
    }

    pub fn parse(&mut self) -> Result<ASTVec, String> {
        return self.program();
    }

    fn program(&mut self) -> Result<ASTVec, String> {
        let mut prog: ASTVec = vec![];

        while !self.eof() {
            prog.push(self.declaration()?);
        }

        return Ok(prog);
    }

    fn declaration(&mut self) -> Result<ASTRef, String> {
        let typ = self.type_spec()?;
        expect!(self, TokenKind::Identifier, _);
        let name = yank!(self, TokenKind::Identifier);

        if peek!(self, TokenKind::LParen) {
            self.function_decl(typ, name)
        } else {
            self.variable_decl(typ, name)
        }
    }

    fn function_params(&mut self) -> Result<ASTVec, String> {
        expect!(self, TokenKind::LParen);
        accept!(self, TokenKind::Void);
        expect!(self, TokenKind::RParen);

        let params: ASTVec = vec![];

        Ok(params)
    }

    fn function_sig(
        &mut self,
        rtype: ASTRef,
        name: String,
    ) -> Result<ASTRef, String> {
        let params = self.function_params()?;
        let sym = resolve!(self, &name);

        as_a!(sym, ASTKind::Function {
            name: _, params: _, block: _, rtype: _, scope: _
        } => {
            // TODO: check for signature match
            return Ok(sym.clone().unwrap());
        } else {
            if sym.is_some() {
                return Err(format!(
                        "'{}' redefined as a different kind of symbol",
                        name));
            }
        });

        let func = new_node!(
            self,
            Function {
                name: name.clone(),
                params: params.clone(),
                block: None,
                rtype: rtype.clone(),
                scope: self.scope.clone(),
            }
        );

        self.add_sym(&name, func.clone())?;

        Ok(func)
    }

    fn function_decl(
        &mut self,
        typ: ASTRef,
        name: String,
    ) -> Result<ASTRef, String> {
        self.open_scope();
        let func = self.function_sig(typ, name)?;

        if accept!(self, TokenKind::Semicolon) {
            self.close_scope();
            return Ok(func);
        }

        as_a_mut!(Some(func.clone()), ASTKind::Function {
                    name, params: _, block, rtype: _, scope: _
        } => {
            if block.is_some() {
                self.close_scope();
                return Err(format!("function '{}' already defined", name));
            }

            *block = Some(self.block()?);
        });

        self.close_scope();

        Ok(func)
    }

    fn variable_decl(
        &mut self,
        typ: ASTRef,
        name: String,
    ) -> Result<ASTRef, String> {
        let var = new_node!(
            self,
            Variable {
                name: name.clone(),
                typ: typ.clone(),
                init: None
            }
        );

        self.add_sym(&name, var.clone())?;

        let initializer = if accept!(self, TokenKind::Assign) {
            Some(self.expr(0)?)
        } else {
            None
        };

        expect!(self, TokenKind::Semicolon);

        as_a_mut!(Some(var.clone()), ASTKind::Variable {
            name: _, typ: _, init
        } => {
            *init = initializer;
        });

        Ok(var)
    }

    fn is_type_spec(&self) -> bool {
        return peek!(self, TokenKind::Int);
    }

    fn type_spec(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Int);
        return Ok(new_node!(self, Int));
    }

    fn stmt_or_decl(&mut self) -> Result<ASTRef, String> {
        if peek!(self, TokenKind::LCurly) {
            self.block()
        } else if self.is_type_spec() {
            self.declaration()
        } else if peek!(self, TokenKind::Return) {
            self.return_stmt()
        } else {
            self.expr_stmt()
        }
    }

    fn block(&mut self) -> Result<ASTRef, String> {
        let mut body: ASTVec = vec![];
        self.open_scope();

        expect!(self, TokenKind::LCurly);

        while !accept!(self, TokenKind::RCurly) {
            if !accept!(self, TokenKind::Semicolon) {
                body.push(self.stmt_or_decl()?);
            }
        }

        self.close_scope();

        return Ok(new_node!(self, Block { body: body }));
    }

    fn return_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Return);
        let expr = self.expr(0)?;
        expect!(self, TokenKind::Semicolon);
        return Ok(new_node!(self, Return { expr: expr }));
    }

    fn expr_stmt(&mut self) -> Result<ASTRef, String> {
        let stmt = new_node!(
            self,
            ExprStmt {
                expr: self.expr(0)?
            }
        );
        expect!(self, TokenKind::Semicolon);
        return Ok(stmt);
    }

    fn expr(&mut self, min_prec: i32) -> Result<ASTRef, String> {
        let mut left = self.factor()?;
        let mut prec = precedence_of(&peek_any!(self));

        while self.peek_binop() && prec >= min_prec {
            if accept!(self, TokenKind::Assign) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.assignment(left, right)?;
            } else if accept!(self, TokenKind::PlusEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.plus_eq(left, right)?;
            } else if accept!(self, TokenKind::MinusEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.minus_eq(left, right)?;
            } else if accept!(self, TokenKind::MultEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.mult_eq(left, right)?;
            } else if accept!(self, TokenKind::DivEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.div_eq(left, right)?;
            } else if accept!(self, TokenKind::ModEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.mod_eq(left, right)?;
            } else if accept!(self, TokenKind::AndEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.and_eq(left, right)?;
            } else if accept!(self, TokenKind::OrEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.or_eq(left, right)?;
            } else if accept!(self, TokenKind::XorEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.xor_eq(left, right)?;
            } else if accept!(self, TokenKind::LShiftEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.lshift_eq(left, right)?;
            } else if accept!(self, TokenKind::RShiftEq) {
                let right = self.expr(precedence_of(&peek_any!(self)))?;
                left = self.rshift_eq(left, right)?;
            } else {
                left = self.binop(left, prec)?;
                prec = precedence_of(&peek_any!(self));
            }
        }

        if self.validate && min_prec == 0 {
            if eval(left.clone()).is_none() {
                return Err("invalid expression".to_string());
            }
        }

        return Ok(left);
    }

    fn peek_binop(&self) -> bool {
        return match peek_any!(self) {
            TokenKind::Mult
            | TokenKind::Div
            | TokenKind::Mod
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::LShift
            | TokenKind::RShift
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Xor
            | TokenKind::LAnd
            | TokenKind::LOr
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::LThan
            | TokenKind::LThanEq
            | TokenKind::GThan
            | TokenKind::GThanEq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::MultEq
            | TokenKind::DivEq
            | TokenKind::ModEq
            | TokenKind::AndEq
            | TokenKind::OrEq
            | TokenKind::XorEq
            | TokenKind::LShiftEq
            | TokenKind::RShiftEq
            | TokenKind::Assign => true,
            _ => false,
        };
    }

    fn binop(&mut self, left: ASTRef, prec: i32) -> Result<ASTRef, String> {
        if accept!(self, TokenKind::Mult) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Multiply {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Div) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Divide {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Mod) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Modulo {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Plus) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Add {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Minus) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Subtract {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LShift) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LShift {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::RShift) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                RShift {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::And) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                And {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Or) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Or {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Xor) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Xor {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LAnd) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LogicAnd {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LOr) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LogicOr {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::Eq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                Equal {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::NotEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                NotEq {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LThan) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LessThan {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::LThanEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                LessOrEq {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::GThan) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                GreaterThan {
                    left: left,
                    right: right
                }
            ));
        } else if accept!(self, TokenKind::GThanEq) {
            let right = self.expr(prec + 1)?;
            return Ok(new_node!(
                self,
                GreaterOrEq {
                    left: left,
                    right: right
                }
            ));
        } else {
            return self.oops("malformed binary expression");
        }
    }

    fn assignment(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left,
                right: right
            }
        ));
    }

    fn plus_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Add {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn minus_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Subtract {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn mult_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Multiply {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn div_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Divide {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn mod_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Modulo {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn and_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    And {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn or_eq(&mut self, left: ASTRef, right: ASTRef) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Or {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn xor_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    Xor {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn lshift_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    LShift {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn rshift_eq(
        &mut self,
        left: ASTRef,
        right: ASTRef,
    ) -> Result<ASTRef, String> {
        return Ok(new_node!(
            self,
            Assign {
                left: left.clone(),
                right: new_node!(
                    self,
                    RShift {
                        left: left.clone(),
                        right: right.clone()
                    }
                ),
            }
        ));
    }

    fn peek_postfix_op(&self) -> bool {
        match peek_any!(self) {
            TokenKind::Incr | TokenKind::Decr => true,
            _ => false,
        }
    }

    fn postfix(&mut self, mut expr: ASTRef) -> Result<ASTRef, String> {
        while self.peek_postfix_op() {
            if accept!(self, TokenKind::Incr) {
                expr = new_node!(self, PostIncr { expr: expr.clone() });
            } else if accept!(self, TokenKind::Decr) {
                expr = new_node!(self, PostDecr { expr: expr.clone() });
            } else {
                return Err("invalid postfix expression".to_string());
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<ASTRef, String> {
        if peek!(self, TokenKind::ConstInt, _) {
            return self.const_int();
        } else if accept!(self, TokenKind::Tilde) {
            return Ok(new_node!(
                self,
                Complement {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Minus) {
            return Ok(new_node!(
                self,
                Negate {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Not) {
            return Ok(new_node!(
                self,
                Not {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Incr) {
            return Ok(new_node!(
                self,
                PreIncr {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::Decr) {
            return Ok(new_node!(
                self,
                PreDecr {
                    expr: self.factor()?
                }
            ));
        } else if accept!(self, TokenKind::LParen) {
            let mut inner_expr = self.expr(0)?;
            expect!(self, TokenKind::RParen);

            if self.peek_postfix_op() {
                inner_expr = self.postfix(inner_expr.clone())?;
            }

            Ok(inner_expr)
        } else if accept!(self, TokenKind::Identifier, _) {
            let mut expr = new_node!(
                self,
                Identifier {
                    name: yank!(self, TokenKind::Identifier)
                }
            );

            if self.peek_postfix_op() {
                expr = self.postfix(expr.clone())?;
            }

            Ok(expr)
        } else {
            return self.oops("malformed expression");
        }
    }

    fn const_int(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::ConstInt, _);
        let value = yank!(self, TokenKind::ConstInt);
        return Ok(new_node!(self, ConstInt(value)));
    }

    fn oops(&self, why: &str) -> Result<ASTRef, String> {
        return Err(why.to_string());
    }

    fn eof(&self) -> bool {
        return matches!(self.next.as_ref().unwrap().kind, TokenKind::EOF);
    }

    fn advance(&mut self) {
        self.last = self.next.clone();
        self.next = self.lexer.lex();

        if self.next.as_ref().unwrap().kind == TokenKind::Bad {
            panic!("invalid token found");
        }
    }
}
