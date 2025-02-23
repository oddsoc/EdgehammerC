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

use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    LParen,
    RParen,
    LCurly,
    RCurly,
    Semicolon,
    Tilde,
    Minus,
    MinusEq,
    Plus,
    PlusEq,
    Decr,
    Incr,
    Void,
    Int,
    Return,
    Identifier(String),
    ConstInt(i64),
    Bad,
    EOF,
}

#[derive(Debug)]
pub struct Lexer {
    buffer: Vec<u8>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(filename: &str) -> Self {
        let mut file = File::open(filename).unwrap();
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer).unwrap();
        Self {
            buffer: buffer,
            position: 0,
            line: 1,
            column: 1,
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.position < self.buffer.len() {
            Some(self.buffer[self.position])
        } else {
            None
        }
    }
    /*
        fn peek_a(&self, b: u8) -> bool {
            let c = self.peek();

            if c.is_some() && c == Some(b) {
                return true;
            }

            false
        }

        fn consume_if(&mut self, b: u8) -> bool {
            if self.peek_a(b) {
                self.consume();
                return true;
            }

            false
        }
    */

    fn consume(&mut self) -> Option<u8> {
        let byte = self.peek()?;

        self.position += 1;

        if byte == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        return Some(byte);
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

    fn const_integer(&mut self) -> Option<Token> {
        let start = self.column;
        let mut lexeme = String::new();
        let mut byte = self.peek().unwrap();

        if byte == b'0' {
            lexeme.push(self.consume().unwrap() as char);
        } else {
            while self.position < self.buffer.len() {
                byte = self.peek().unwrap();

                if byte.is_ascii_digit() {
                    lexeme.push(self.consume().unwrap() as char);
                } else if byte.is_ascii_alphabetic() || byte == b'_' {
                    return Some(Token {
                        kind: TokenKind::Bad,
                        line: self.line,
                        column: start,
                    });
                } else {
                    break;
                }
            }
        }

        Some(Token {
            kind: TokenKind::ConstInt(lexeme.parse::<i64>().unwrap()),
            line: self.line,
            column: start,
        })
    }

    fn identifier_or_keyword(&mut self) -> Option<Token> {
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
            "return" => {
                return Some(Token {
                    kind: TokenKind::Return,
                    line: self.line,
                    column: start,
                })
            }
            "void" => {
                return Some(Token {
                    kind: TokenKind::Void,
                    line: self.line,
                    column: start,
                })
            }
            "int" => {
                return Some(Token {
                    kind: TokenKind::Int,
                    line: self.line,
                    column: start,
                })
            }
            _ => {}
        }

        Some(Token {
            kind: TokenKind::Identifier(lexeme),
            line: self.line,
            column: start,
        })
    }

    pub fn lex(&mut self) -> Option<Token> {
        while self.position < self.buffer.len() {
            let byte = if self.peek().is_some() {
                self.peek().unwrap()
            } else {
                return None;
            };

            match byte {
                // skip whitespace
                b' ' | b'\t' | b'\n' => {
                    self.consume();
                }
                b'/' => {
                    self.consume();
                    match self.peek() {
                        Some(b'/') | Some(b'*') => {
                            self.skip_comment().unwrap();
                        }
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Bad,
                                line: self.line,
                                column: self.column - 1,
                            });
                        }
                    }
                }
                b'(' => {
                    self.consume();
                    return Some(Token {
                        kind: TokenKind::LParen,
                        line: self.line,
                        column: self.column - 1,
                    });
                }
                b')' => {
                    self.consume();
                    return Some(Token {
                        kind: TokenKind::RParen,
                        line: self.line,
                        column: self.column - 1,
                    });
                }
                b'{' => {
                    self.consume();
                    return Some(Token {
                        kind: TokenKind::LCurly,
                        line: self.line,
                        column: self.column - 1,
                    });
                }
                b'}' => {
                    self.consume();
                    return Some(Token {
                        kind: TokenKind::RCurly,
                        line: self.line,
                        column: self.column - 1,
                    });
                }
                b';' => {
                    self.consume();
                    return Some(Token {
                        kind: TokenKind::Semicolon,
                        line: self.line,
                        column: self.column - 1,
                    });
                }
                b'~' => {
                    self.consume();
                    return Some(Token {
                        kind: TokenKind::Tilde,
                        line: self.line,
                        column: self.column - 1,
                    });
                }
                b'-' => {
                    self.consume();
                    match self.peek() {
                        Some(b'-') => {
                            return Some(Token {
                                kind: TokenKind::Decr,
                                line: self.line,
                                column: self.column - 1,
                            });
                        }
                        Some(b'=') => {
                            return Some(Token {
                                kind: TokenKind::MinusEq,
                                line: self.line,
                                column: self.column - 1,
                            });
                        }
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Minus,
                                line: self.line,
                                column: self.column - 1,
                            });
                        }
                    }
                }
                b'+' => {
                    self.consume();
                    match self.peek() {
                        Some(b'+') => {
                            return Some(Token {
                                kind: TokenKind::Incr,
                                line: self.line,
                                column: self.column - 1,
                            });
                        }
                        Some(b'=') => {
                            return Some(Token {
                                kind: TokenKind::PlusEq,
                                line: self.line,
                                column: self.column - 1,
                            });
                        }
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Plus,
                                line: self.line,
                                column: self.column - 1,
                            });
                        }
                    }
                }
                b'0'..=b'9' => {
                    return self.const_integer();
                }
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                    return self.identifier_or_keyword();
                }
                _ => {
                    return Some(Token {
                        kind: TokenKind::Bad,
                        line: self.line,
                        column: self.column,
                    })
                }
            }
        }

        Some(Token {
            kind: TokenKind::EOF,
            line: self.line,
            column: self.column,
        })
    }
}
