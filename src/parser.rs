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
            panic!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.next.as_ref().unwrap().kind
            );
        } else {
            $parser.advance();
        }
    };

    ($parser:expr, $kind:path, $_:tt) => {
        if !matches!($parser.next.as_ref().unwrap().kind, $kind(_)) {
            panic!(
                "Expected {:?}, but found {:?}",
                stringify!($kind),
                $parser.next.as_ref().unwrap().kind
            );
        } else {
            $parser.advance();
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

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    ConstInt(i64),
    Int,
    Function {
        name: String,
        params: Vec<Rc<AST>>,
        stmts: Vec<Rc<AST>>,
        rtype: Rc<AST>,
    },
    Return {
        expr: Rc<AST>,
    },
}

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

    pub fn parse(&mut self) -> Result<Vec<Rc<AST>>, String> {
        return self.program();
    }

    fn program(&mut self) -> Result<Vec<Rc<AST>>, String> {
        let mut prog: Vec<Rc<AST>> = vec![];

        while !self.eof() {
            prog.push(self.function_def()?);
        }

        return Ok(prog);
    }

    fn function_def(&mut self) -> Result<Rc<AST>, String> {
        let rtype = self.type_decl()?;
        expect!(self, TokenKind::Identifier, _);
        let name = yank!(self, TokenKind::Identifier);
        expect!(self, TokenKind::LParen);
        accept!(self, TokenKind::Void);
        expect!(self, TokenKind::RParen);
        let block = self.block()?;

        return Ok(Rc::new(AST::Function {
            name: name,
            params: vec![],
            stmts: block,
            rtype: rtype,
        }));
    }

    fn type_decl(&mut self) -> Result<Rc<AST>, String> {
        expect!(self, TokenKind::Int);
        return Ok(Rc::new(AST::Int));
    }

    fn block(&mut self) -> Result<Vec<Rc<AST>>, String> {
        expect!(self, TokenKind::LCurly);
        let stmt = self.return_stmt()?;
        expect!(self, TokenKind::RCurly);
        return Ok(vec![stmt]);
    }

    fn return_stmt(&mut self) -> Result<Rc<AST>, String> {
        expect!(self, TokenKind::Return);
        let expr = self.expr()?;
        expect!(self, TokenKind::Semicolon);
        return Ok(Rc::new(AST::Return { expr: expr }));
    }

    fn expr(&mut self) -> Result<Rc<AST>, String> {
        expect!(self, TokenKind::ConstInt, _);
        let value = yank!(self, TokenKind::ConstInt);
        return Ok(Rc::new(AST::ConstInt(value)));
    }

    fn eof(&self) -> bool {
        return matches!(self.next.as_ref().unwrap().kind, TokenKind::EOF);
    }

    fn advance(&mut self) {
        self.last = self.next.clone();
        self.next = self.lexer.lex();

        if self.next.as_ref().unwrap().kind == TokenKind::Bad {
            panic!("Invalid token found");
        }
    }
}
