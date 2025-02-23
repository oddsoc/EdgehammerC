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
        Rc::new(AST::$variant)
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(AST::$variant( $($args),* ))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(AST::$variant {
            $($field: $value),*
        })
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
}

type ASTRef = Rc<AST>;
type ASTVec = Vec<ASTRef>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    last: Option<Token>,
    next: Option<Token>,
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
        let expr = self.expr()?;
        expect!(self, TokenKind::Semicolon);
        return Ok(ast_rc!(Return { expr: expr }));
    }

    fn expr(&mut self) -> Result<ASTRef, String> {
        return self.unary_expr();
    }

    fn const_int(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::ConstInt, _);
        let value = yank!(self, TokenKind::ConstInt);
        return Ok(ast_rc!(ConstInt(value)));
    }

    fn paren_expr(&mut self) -> Result<ASTRef, String> {
        expect!(self, TokenKind::LParen);
        let exp = self.expr();
        expect!(self, TokenKind::RParen);

        exp
    }

    fn unary_expr(&mut self) -> Result<ASTRef, String> {
        if accept!(self, TokenKind::Tilde) {
            return Ok(ast_rc!(Not { expr: self.expr()? }));
        } else if accept!(self, TokenKind::Minus) {
            return Ok(ast_rc!(Negate { expr: self.expr()? }));
        } else if peek!(self, TokenKind::LParen) {
            return self.paren_expr();
        } else {
            return self.const_int();
        }
    }

    //fn oops(&self, why: &str) -> Result<ASTRef, String> {
    //    return Err(why.to_string());
    //}

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
