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

use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenKind;

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

macro_rules! ast_rc {
    ($variant:ident) => {
        Rc::new(RefCell::new(AST::$variant))
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(RefCell::new(AST::$variant( $($args),* )))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(RefCell::new(AST::$variant {
            $($field: $value),*
        }))
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    ConstInt(i64),
    Int,
    Function {
        name: String,
        params: ASTVec,
        stmts: ASTVec,
        rtype: ASTRef,
    },
    Return {
        expr: ASTRef,
    },
    Not {
        expr: ASTRef,
    },
    Negate {
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
}

pub type ASTRef = Rc<RefCell<AST>>;
pub type ASTVec = Vec<ASTRef>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    last: Option<Token>,
    next: Option<Token>,
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
    pub fn new(filename: &str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(filename),
            last: None,
            next: None,
        };

        parser.advance();

        return parser;
    }

    pub fn parse(&mut self) -> Result<ASTVec, String> {
        return self.program();
    }

    fn program(&mut self) -> Result<ASTVec, String> {
        let mut prog: ASTVec = vec![];

        while !self.eof() {
            prog.push(self.function_def()?);
        }

        return Ok(prog);
    }

    fn function_def(&mut self) -> Result<ASTRef, String> {
        let rtype = self.type_decl()?;
        expect!(self, TokenKind::Identifier, _);
        let name = yank!(self, TokenKind::Identifier);
        expect!(self, TokenKind::LParen);
        accept!(self, TokenKind::Void);
        expect!(self, TokenKind::RParen);
        let block = self.block()?;

        return Ok(ast_rc!(Function {
            name: name,
            params: vec![],
            stmts: block,
            rtype: rtype,
        }));
    }

    fn type_decl(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Int);
        return Ok(ast_rc!(Int));
    }

    fn block(&mut self) -> Result<ASTVec, String> {
        expect!(self, TokenKind::LCurly);
        let stmt = self.return_stmt()?;
        expect!(self, TokenKind::RCurly);
        return Ok(vec![stmt]);
    }

    fn return_stmt(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::Return);
        let expr = self.expr(0)?;
        expect!(self, TokenKind::Semicolon);
        return Ok(ast_rc!(Return { expr: expr }));
    }

    fn expr(&mut self, min_prec: i32) -> Result<ASTRef, String> {
        let mut left = self.factor()?;
        let mut prec = precedence_of(&peek_any!(self));

        while self.peek_binop() && prec >= min_prec {
            left = self.binop(left, prec)?;
            prec = precedence_of(&peek_any!(self));
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
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::LThan
            | TokenKind::LThanEq
            | TokenKind::GThan
            | TokenKind::GThanEq => true,
            _ => false,
        };
    }

    fn binop(&mut self, left: ASTRef, prec: i32) -> Result<ASTRef, String> {
        if accept!(self, TokenKind::Mult) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(Multiply {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::Div) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(Divide {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::Mod) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(Modulo {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::Plus) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(Add {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::Minus) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(Subtract {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::LShift) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(LShift {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::RShift) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(RShift {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::And) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(And {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::Or) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(Or {
                left: left,
                right: right
            }));
        } else if accept!(self, TokenKind::Xor) {
            let right = self.expr(prec + 1)?;
            return Ok(ast_rc!(Xor {
                left: left,
                right: right
            }));
        } else {
            return self.oops("malformed binary expression");
        }
    }

    fn factor(&mut self) -> Result<ASTRef, String> {
        if peek!(self, TokenKind::ConstInt, _) {
            return self.const_int();
        } else if accept!(self, TokenKind::Tilde) {
            return Ok(ast_rc!(Not {
                expr: self.factor()?
            }));
        } else if accept!(self, TokenKind::Minus) {
            return Ok(ast_rc!(Negate {
                expr: self.factor()?
            }));
        } else if accept!(self, TokenKind::LParen) {
            let inner_expr = self.expr(0)?;
            expect!(self, TokenKind::RParen);
            return Ok(inner_expr);
        } else {
            return self.oops("malformed expression");
        }
    }

    fn const_int(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::ConstInt, _);
        let value = yank!(self, TokenKind::ConstInt);
        return Ok(ast_rc!(ConstInt(value)));
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
