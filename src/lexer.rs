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

use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pos: usize,
    len: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    LParen,
    RParen,
    LCurly,
    RCurly,
    Colon,
    Semicolon,
    Comma,
    QMark,
    Tilde,
    Minus,
    MinusEq,
    Plus,
    PlusEq,
    Mult,
    MultEq,
    Div,
    DivEq,
    Mod,
    ModEq,
    LShift,
    LShiftEq,
    RShift,
    RShiftEq,
    LThan,
    LThanEq,
    GThan,
    GThanEq,
    Eq,
    NotEq,
    Assign,
    And,
    AndEq,
    Or,
    OrEq,
    Xor,
    XorEq,
    LAnd,
    LOr,
    Not,
    InvEq,
    Decr,
    Incr,
    Void,
    Int,
    Long,
    Signed,
    Unsigned,
    GoTo,
    Return,
    If,
    Else,
    Do,
    While,
    For,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Static,
    Extern,
    Auto,
    Register,
    Identifier(String),
    Label(String),
    ConstInt(i64),
    ConstLong(i64),
    ConstLongLong(i64),
    ConstUnsignedInt(u64),
    ConstUnsignedLong(u64),
    ConstUnsignedLongLong(u64),
    Bad,
    EOF,
}

#[allow(dead_code)]
enum ConstKind {
    Int,
    Long,
    LongLong,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
}

#[derive(Debug)]
pub struct Lexer {
    buffer: Vec<u8>,
    position: usize,
    line: usize,
    column: usize,
    expr_depth: usize,
}

impl Lexer {
    pub fn new(filename: &str) -> Self {
        let mut file = File::open(filename).unwrap();
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer).unwrap();
        Self {
            buffer,
            position: 0,
            line: 1,
            column: 1,
            expr_depth: 0,
        }
    }

    fn new_tok(
        &self,
        kind: TokenKind,
        pos: usize,
        column: usize,
    ) -> Option<Token> {
        let len = self.position - pos;
        Some(Token {
            kind,
            pos,
            len,
            line: self.line,
            column,
        })
    }

    pub fn push_expr(&mut self) {
        self.expr_depth += 1;
    }

    pub fn pop_expr(&mut self) {
        assert!(self.expr_depth > 0);
        self.expr_depth -= 1;
    }

    #[allow(dead_code)]
    pub fn as_str<'a>(&'a self, token: &Token) -> &'a str {
        let utf8 = str::from_utf8(&self.buffer).expect("not utf8!");
        &utf8[token.pos..token.len]
    }

    fn peek(&self) -> Option<u8> {
        if self.position < self.buffer.len() {
            Some(self.buffer[self.position])
        } else {
            None
        }
    }

    fn consume(&mut self) -> Option<u8> {
        let byte = self.peek()?;

        self.position += 1;

        if byte == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some(byte)
    }

    fn skip_comment(&mut self) -> Result<(), ()> {
        let mut byte = self.peek();

        if byte.is_none() {
            return Err(());
        }

        if byte == Some(b'/') {
            while self.peek().is_some() && self.peek() != Some(b'\n') {
                self.consume();
            }
        } else if byte == Some(b'*') {
            self.consume();

            loop {
                byte = self.consume();

                match byte {
                    Some(b'*') => {
                        byte = self.consume();
                        if byte == Some(b'/') {
                            self.consume();
                            return Ok(());
                        }
                    }
                    None => {
                        return Err(());
                    }
                    _ => {}
                }
            }
        } else {
            return Err(());
        }

        Ok(())
    }

    fn consume_const_integer_suffix(&mut self) -> Result<ConstKind, ()> {
        let mut is_unsigned = false;
        let mut nr_longs = 0;

        if let Some(b) = self.peek() {
            match b {
                b'u' | b'U' => {
                    self.consume();
                    is_unsigned = true;
                    if let Some(b2) = self.peek() {
                        if b2 == b'l' || b2 == b'L' {
                            self.consume();
                            nr_longs = 1;
                            if self.peek() == Some(b2) {
                                self.consume();
                                nr_longs = 2;
                            }
                        }
                    }
                }
                b'l' | b'L' => {
                    self.consume();
                    nr_longs = 1;
                    if self.peek() == Some(b) {
                        self.consume();
                        nr_longs = 2;
                    } else if matches!(self.peek(), Some(b'u' | b'U')) {
                        self.consume();
                        is_unsigned = true;
                    }
                }
                _ => {}
            }
        }

        if let Some(b) = self.peek() {
            if b.is_ascii_alphabetic() || b == b'_' {
                return Err(());
            }
        }

        match (is_unsigned, nr_longs) {
            (false, 0) => Ok(ConstKind::Int),
            (true, 0) => Ok(ConstKind::UnsignedInt),
            (false, 1) => Ok(ConstKind::Long),
            (true, 1) => Ok(ConstKind::UnsignedLong),
            (false, 2) => Ok(ConstKind::LongLong),
            (true, 2) => Ok(ConstKind::UnsignedLongLong),
            _ => Err(()),
        }
    }

    fn is_integer_suffix_char(b: u8) -> bool {
        b == b'u' || b == b'U' || b == b'l' || b == b'L'
    }

    fn const_integer(&mut self) -> Option<Token> {
        let pos = self.position;
        let start = self.column;
        let mut lexeme = String::new();
        let mut byte = self.peek().unwrap();

        if byte == b'0' {
            lexeme.push(self.consume().unwrap() as char);

            if (!Self::is_integer_suffix_char(byte))
                && byte.is_ascii_alphabetic()
            {
                return self.new_tok(TokenKind::Bad, pos, start);
            }
        } else {
            while self.position < self.buffer.len() {
                byte = self.peek().unwrap();

                if Self::is_integer_suffix_char(byte) {
                    break;
                }

                if byte.is_ascii_digit() {
                    lexeme.push(self.consume().unwrap() as char);
                } else if byte.is_ascii_alphabetic() || byte == b'_' {
                    return self.new_tok(TokenKind::Bad, pos, start);
                } else {
                    break;
                }
            }
        }

        let int_value = match lexeme.parse::<u64>() {
            Ok(val) => val,
            Err(_) => return self.new_tok(TokenKind::Bad, pos, start),
        };

        match self.consume_const_integer_suffix() {
            Ok(suffix) => match suffix {
                ConstKind::Int => {
                    if let Ok(int_value_i64) = i64::try_from(int_value) {
                        if int_value_i64 >= i32::MIN as i64
                            && int_value_i64 <= i32::MAX as i64
                        {
                            self.new_tok(
                                TokenKind::ConstInt(int_value_i64),
                                pos,
                                start,
                            )
                        } else {
                            self.new_tok(
                                TokenKind::ConstLong(int_value_i64),
                                pos,
                                start,
                            )
                        }
                    } else {
                        self.new_tok(TokenKind::Bad, pos, start)
                    }
                }
                ConstKind::Long => match i64::try_from(int_value) {
                    Ok(val) => {
                        self.new_tok(TokenKind::ConstLong(val), pos, start)
                    }
                    Err(_) => self.new_tok(TokenKind::Bad, pos, start),
                },
                ConstKind::LongLong => match lexeme.parse::<i64>() {
                    Ok(val) => {
                        self.new_tok(TokenKind::ConstLongLong(val), pos, start)
                    }
                    Err(_) => self.new_tok(TokenKind::Bad, pos, start),
                },
                ConstKind::UnsignedInt
                | ConstKind::UnsignedLong
                | ConstKind::UnsignedLongLong => self.new_tok(
                    TokenKind::ConstUnsignedLongLong(int_value),
                    pos,
                    start,
                ),
            },
            Err(_) => self.new_tok(TokenKind::Bad, pos, start),
        }
    }

    fn identifier_or_keyword(&mut self) -> Option<Token> {
        let pos = self.position;
        let start = self.column;
        let mut lexeme = String::new();
        lexeme.push(self.consume().unwrap() as char);

        while self.position < self.buffer.len() {
            let byte = self.peek().unwrap();

            if byte.is_ascii_alphanumeric() || byte == b'_' {
                lexeme.push(self.consume().unwrap() as char);
            } else {
                break;
            }
        }

        match lexeme.as_str() {
            "if" => {
                return self.new_tok(TokenKind::If, pos, start);
            }
            "else" => {
                return self.new_tok(TokenKind::Else, pos, start);
            }
            "do" => {
                return self.new_tok(TokenKind::Do, pos, start);
            }
            "while" => {
                return self.new_tok(TokenKind::While, pos, start);
            }
            "for" => {
                return self.new_tok(TokenKind::For, pos, start);
            }
            "switch" => {
                return self.new_tok(TokenKind::Switch, pos, start);
            }
            "case" => {
                return self.new_tok(TokenKind::Case, pos, start);
            }
            "default" => {
                return self.new_tok(TokenKind::Default, pos, start);
            }
            "break" => {
                return self.new_tok(TokenKind::Break, pos, start);
            }
            "continue" => {
                return self.new_tok(TokenKind::Continue, pos, start);
            }
            "goto" => {
                return self.new_tok(TokenKind::GoTo, pos, start);
            }
            "return" => {
                return self.new_tok(TokenKind::Return, pos, start);
            }
            "void" => {
                return self.new_tok(TokenKind::Void, pos, start);
            }
            "int" => {
                return self.new_tok(TokenKind::Int, pos, start);
            }
            "long" => {
                return self.new_tok(TokenKind::Long, pos, start);
            }
            "signed" => {
                return self.new_tok(TokenKind::Signed, pos, start);
            }
            "unsigned" => {
                return self.new_tok(TokenKind::Unsigned, pos, start);
            }
            "static" => {
                return self.new_tok(TokenKind::Static, pos, start);
            }
            "extern" => {
                return self.new_tok(TokenKind::Extern, pos, start);
            }
            "auto" => {
                return self.new_tok(TokenKind::Auto, pos, start);
            }
            "register" => {
                return self.new_tok(TokenKind::Register, pos, start);
            }
            _ => {}
        }

        if self.expr_depth == 0 {
            let mut byte = self.peek().unwrap();
            loop {
                match byte {
                    b' ' | b'\t' | b'\n' => {
                        // skip whitespace
                        self.consume();
                    }

                    _ => break,
                }

                if self.peek().is_none() {
                    break;
                } else {
                    byte = self.peek().unwrap();
                }
            }

            if byte == b':' {
                let tok = self.new_tok(TokenKind::Label(lexeme), pos, start);
                self.consume();
                return tok;
            }
        }

        self.new_tok(TokenKind::Identifier(lexeme), pos, start)
    }

    pub fn lex(&mut self) -> Option<Token> {
        while self.position < self.buffer.len() {
            let start = self.position;
            let byte = if self.peek().is_some() {
                self.peek().unwrap()
            } else {
                return None;
            };

            match byte {
                b' ' | b'\t' | b'\n' => {
                    // skip whitespace
                    self.consume();
                }
                b'/' => {
                    self.consume();
                    match self.peek() {
                        Some(b'/') | Some(b'*') => {
                            self.skip_comment().unwrap();
                        }
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::DivEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Div,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'(' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::LParen,
                        start,
                        self.column - 1,
                    );
                }
                b')' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::RParen,
                        start,
                        self.column - 1,
                    );
                }
                b'{' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::LCurly,
                        start,
                        self.column - 1,
                    );
                }
                b'}' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::RCurly,
                        start,
                        self.column - 1,
                    );
                }
                b':' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::Colon,
                        start,
                        self.column - 1,
                    );
                }
                b';' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::Semicolon,
                        start,
                        self.column - 1,
                    );
                }
                b',' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::Comma,
                        start,
                        self.column - 1,
                    );
                }
                b'?' => {
                    self.consume();
                    return self.new_tok(
                        TokenKind::QMark,
                        start,
                        self.column - 1,
                    );
                }
                b'~' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::InvEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Tilde,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'-' => {
                    self.consume();
                    match self.peek() {
                        Some(b'-') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::Decr,
                                start,
                                self.column - 1,
                            );
                        }
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::MinusEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Minus,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'+' => {
                    self.consume();
                    match self.peek() {
                        Some(b'+') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::Incr,
                                start,
                                self.column - 1,
                            );
                        }
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::PlusEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Plus,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'*' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::MultEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Mult,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'%' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::ModEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Mod,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'<' => {
                    self.consume();
                    match self.peek() {
                        Some(b'<') => {
                            self.consume();
                            match self.peek() {
                                Some(b'=') => {
                                    self.consume();
                                    return self.new_tok(
                                        TokenKind::LShiftEq,
                                        start,
                                        self.column - 1,
                                    );
                                }
                                _ => {
                                    return self.new_tok(
                                        TokenKind::LShift,
                                        start,
                                        self.column - 1,
                                    );
                                }
                            }
                        }
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::LThanEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::LThan,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'>' => {
                    self.consume();
                    match self.peek() {
                        Some(b'>') => {
                            self.consume();
                            match self.peek() {
                                Some(b'=') => {
                                    self.consume();
                                    return self.new_tok(
                                        TokenKind::RShiftEq,
                                        start,
                                        self.column - 1,
                                    );
                                }
                                _ => {
                                    return self.new_tok(
                                        TokenKind::RShift,
                                        start,
                                        self.column - 1,
                                    );
                                }
                            }
                        }
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::GThanEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::GThan,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'=' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::Eq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Assign,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'!' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::NotEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Not,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'&' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::AndEq,
                                start,
                                self.column - 1,
                            );
                        }
                        Some(b'&') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::LAnd,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::And,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'^' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::XorEq,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Xor,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'|' => {
                    self.consume();
                    match self.peek() {
                        Some(b'=') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::OrEq,
                                start,
                                self.column - 1,
                            );
                        }
                        Some(b'|') => {
                            self.consume();
                            return self.new_tok(
                                TokenKind::LOr,
                                start,
                                self.column - 1,
                            );
                        }
                        _ => {
                            return self.new_tok(
                                TokenKind::Or,
                                start,
                                self.column - 1,
                            );
                        }
                    }
                }
                b'0'..=b'9' => {
                    return self.const_integer();
                }
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                    return self.identifier_or_keyword();
                }
                _ => return self.new_tok(TokenKind::Bad, start, self.column),
            }
        }

        self.new_tok(TokenKind::EOF, self.position, self.column)
    }
}
