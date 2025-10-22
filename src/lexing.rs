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

#[cfg(feature = "simd")]
use std::simd::prelude::*;

use std::ops::Range;
use std::str;

use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Debug, PartialEq, Clone)]
pub struct Tokeniser<'buf> {
    buf: &'buf str,
    tok: Token,
    saw: char,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub loc: TokenLoc,
    pub tag: TokenTag,
}

pub type TokenLoc = Range<usize>;

#[derive(Debug, PartialEq, Clone)]
#[allow(unused)]
pub enum TokenTag {
    Start,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Arrow,
    DoubleQuote,
    Quote,
    Question,
    Tilde,
    Minus,
    MinusEq,
    Plus,
    PlusEq,
    Asterisk,
    MultEq,
    ForwardSlash,
    DivideEq,
    Percent,
    ModEq,
    LeftShift,
    LeftShiftEq,
    RightShift,
    RightShiftEq,
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
    Eq,
    NotEq,
    Assign,
    Ampersand,
    AndEq,
    Bar,
    OrEq,
    Caret,
    XorEq,
    LAnd,
    LOr,
    Bang,
    InvEq,
    Decr,
    Incr,
    Void,
    Int,
    Long,
    Signed,
    Unsigned,
    Double,
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
    Const,
    Volatile,
    Restrict,
    Union,
    Enum,
    Struct,
    TypeDef,
    SizeOf,
    Short,
    Inline,
    Float,
    Char,
    _Bool,
    _Complex,
    _Imaginary,
    Identifier,
    ConstInt(i64),
    ConstLong(i64),
    ConstLongLong(i64),
    ConstUnsignedInt(u64),
    ConstUnsignedLong(u64),
    ConstUnsignedLongLong(u64),
    ConstDouble(f64),
    ConstString(bool),
    ConstChar(char),
    End,
}

type Tag = TokenTag;

enum ConstTag {
    Int,
    Long,
    LongLong,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
}

#[inline(never)]
fn keyword_map(ident: &str) -> Option<Tag> {
    use TokenTag::*;
    Some(match ident {
        "auto" => Auto,
        "break" => Break,
        "case" => Case,
        "char" => Char,
        "const" => Const,
        "continue" => Continue,
        "default" => Default,
        "do" => Do,
        "double" => Double,
        "else" => Else,
        "enum" => Enum,
        "extern" => Extern,
        "float" => Float,
        "for" => For,
        "goto" => GoTo,
        "if" => If,
        "inline" => Inline,
        "int" => Int,
        "long" => Long,
        "register" => Register,
        "restrict" => Restrict,
        "return" => Return,
        "short" => Short,
        "signed" => Signed,
        "sizeof" => SizeOf,
        "static" => Static,
        "struct" => Struct,
        "switch" => Switch,
        "typedef" => TypeDef,
        "union" => Union,
        "unsigned" => Unsigned,
        "void" => Void,
        "volatile" => Volatile,
        "while" => While,
        "_Bool" => _Bool,
        "_Complex" => _Complex,
        "_Imaginary" => _Imaginary,
        _ => return None,
    })
}

#[inline]
fn is_identifier_start(ch: char) -> bool {
    (ch >= 'a' && ch <= 'z')
        || ch == '_'
        || (ch >= 'A' && ch <= 'Z')
        || (!ch.is_ascii() && is_xid_start(ch))
}

#[inline]
fn is_identifier_continue(ch: char) -> bool {
    (ch >= 'a' && ch <= 'z')
        || ch == '_'
        || (ch >= 'A' && ch <= 'Z')
        || (ch >= '0' && ch <= '9')
        || (!ch.is_ascii() && is_xid_continue(ch))
}

impl<'buf> Tokeniser<'buf> {
    pub fn new(buf: &'buf str) -> Self {
        Self {
            buf: buf,
            tok: Token::default(),
            saw: '\0',
        }
    }

    #[inline]
    pub fn text(&'buf self) -> &'buf str {
        &self.buf[self.tok.loc.clone()]
    }

    #[inline]
    fn peek_byte(&mut self) -> u8 {
        let end = self.tok.loc.end;
        let buf = self.buf.as_bytes();

        if end == buf.len() {
            0
        } else {
            unsafe { *buf.get_unchecked(end) }
        }
    }

    #[inline]
    fn peek(&mut self) -> char {
        let b = self.peek_byte();

        if b < 128 {
            self.saw = b as char;
        } else {
            let end = self.tok.loc.end;
            self.saw =
                unsafe { self.buf[end..].chars().next().unwrap_unchecked() };
        }

        self.saw
    }

    #[inline]
    fn take(&mut self) {
        self.tok.loc.end += self.saw.len_utf8();
    }

    #[inline]
    fn take_and_peek(&mut self) -> char {
        self.take();
        self.peek()
    }

    #[inline]
    fn take_and_peek_byte(&mut self) -> u8 {
        self.take();
        self.peek_byte()
    }

    fn const_integer_suffix(&mut self) -> Result<ConstTag, ()> {
        let mut is_unsigned = false;
        let mut nr_longs = 0;

        match self.peek_byte() {
            b'u' | b'U' => {
                let ch = self.take_and_peek_byte();
                is_unsigned = true;
                if ch == b'l' || ch == b'L' {
                    self.take();
                    nr_longs = 1;
                    if self.peek_byte() == ch {
                        self.take();
                        nr_longs = 2;
                    }
                }
            }
            b'l' | b'L' => {
                let ch = self.peek_byte();
                let ch2 = self.take_and_peek_byte();
                nr_longs = 1;
                if ch2 == ch {
                    self.take();
                    nr_longs = 2;
                } else if ch2 == b'u' || ch2 == b'U' {
                    self.take();
                    is_unsigned = true;
                }
            }
            _ => {}
        }

        if is_identifier_start(self.peek()) {
            return Err(());
        }

        match (is_unsigned, nr_longs) {
            (false, 0) => Ok(ConstTag::Int),
            (true, 0) => Ok(ConstTag::UnsignedInt),
            (false, 1) => Ok(ConstTag::Long),
            (true, 1) => Ok(ConstTag::UnsignedLong),
            (false, 2) => Ok(ConstTag::LongLong),
            (true, 2) => Ok(ConstTag::UnsignedLongLong),
            _ => Err(()),
        }
    }

    fn const_double(&mut self) -> Result<(), String> {
        let exp_frac = if self.text().as_bytes().first() == Some(&b'.') {
            false
        } else if self.peek() == '.' {
            self.take();
            false
        } else if self.text().len() == 0 {
            true
        } else {
            false
        };

        if exp_frac {
            self.decimal_digits()?;
        } else if self.peek().is_ascii_digit() {
            self.decimal_digits()?;
        }

        if self.peek() == 'e' || self.peek() == 'E' {
            self.take();
            if self.peek() == '+' || self.peek() == '-' {
                self.take();
            }
            self.decimal_digits()?;

            if self.peek() == 'f'
                || self.peek() == 'F'
                || self.peek() == 'l'
                || self.peek() == 'L'
            {
                self.take();
            }
        }

        let byte = self.peek();

        if is_identifier_start(byte) || byte == '.' {
            return Err("invalid double literal".into());
        }

        let value = match self.text().parse::<f64>() {
            Ok(val) => val,
            Err(_) => return Err("invalid double literal".into()),
        };

        self.tok.tag = Tag::ConstDouble(value);
        Ok(())
    }

    #[inline]
    fn octal_digits(&mut self) -> Result<(), String> {
        let mut len = 0;
        while matches!(self.peek(), '0'..='7') {
            self.take();
            len += 1;
        }
        if len > 0 {
            Ok(())
        } else {
            Err("expected octal digits".into())
        }
    }

    #[inline]
    fn hex_digits(&mut self) -> Result<(), String> {
        let mut len = 0;
        while self.peek().is_ascii_hexdigit() {
            self.take();
            len += 1;
        }
        if len > 0 {
            Ok(())
        } else {
            Err("expected hexadecimal digits".into())
        }
    }

    fn const_number(&mut self) -> Result<(), String> {
        let mut ch = self.peek_byte();
        let mut radix = 10;

        if ch == b'0' {
            self.take();
            match self.peek_byte() {
                b'x' | b'X' => {
                    self.take();
                    self.hex_digits()?;
                    radix = 16;
                }
                b'0'..=b'7' => {
                    self.octal_digits()?;
                    radix = 8;
                }
                b'.' => {
                    return self.const_double();
                }
                _ => {}
            }
        } else if ch == b'.' {
            return self.const_double();
        } else {
            self.decimal_digits()?;
        }

        ch = self.peek_byte();

        if ch == b'.' || ch == b'e' || ch == b'E' {
            self.const_double()
        } else {
            self.const_integer(radix)
        }
    }

    #[inline]
    fn decimal_digits(&mut self) -> Result<(), String> {
        let mut len = 0;
        while self.peek().is_ascii_digit() {
            self.take();
            len += 1;
        }
        if len > 0 {
            Ok(())
        } else {
            Err("expected digits".into())
        }
    }

    fn escape_sequence(&mut self) -> Result<(), String> {
        let mut ch = self.peek();

        match ch {
            '\\' | '\'' | '"' | '?' | 'n' | 'r' | 't' | '0' | 'a' | 'b'
            | 'f' | 'v' => {
                self.take();
                Ok(())
            }
            'x' => {
                ch = self.take_and_peek();

                if !ch.is_ascii_hexdigit() {
                    return Err("not a valid hex escape sequence".into());
                }

                ch = self.take_and_peek();

                if !ch.is_ascii_hexdigit() {
                    return Err("not a valid hex escape sequence".into());
                } else {
                    self.take();
                }

                Ok(())
            }
            'u' => {
                self.take();
                for _ in 0..4 {
                    ch = self.peek();

                    if !ch.is_ascii_hexdigit() {
                        return Err(
                            "not a valid unicode escape sequence".into()
                        );
                    } else {
                        self.take();
                    }
                }
                Ok(())
            }
            _ => {
                for _ in 0..3 {
                    if ch < '0' || ch > '7' {
                        return Err("not a valid octal escape sequence".into());
                    } else {
                        self.take();
                    }
                }

                Ok(())
            }
        }
    }

    fn const_string(&mut self) -> Result<(), String> {
        let mut ch: char;
        let mut has_esc_seq = false;
        loop {
            self.take();
            while {
                ch = self.peek();
                ch != '"' && ch != '\0'
            } {
                if ch == '\\' {
                    has_esc_seq = true;
                    self.take();
                    self.escape_sequence()?;
                    continue;
                }

                self.take();
            }

            self.take();

            if self.peek() != '"' {
                break;
            }
        }

        self.tok.tag = Tag::ConstString(has_esc_seq);

        Ok(())
    }

    fn const_char(&mut self) -> Result<(), String> {
        let mut ch: char;
        self.take();
        while {
            ch = self.peek();
            ch != '\'' && ch != '\0'
        } {
            if ch == '\\' {
                self.take();
            }
            self.take();
        }

        self.take();

        self.tok.tag = Tag::ConstChar(self.text().chars().next().unwrap());

        Ok(())
    }

    fn const_integer(&mut self, radix: u32) -> Result<(), String> {
        let digits = if radix == 10 {
            self.text()
        } else if radix == 8 {
            &self.text()[1..]
        } else {
            &self.text()[2..]
        };

        if let Some(c) = digits.as_bytes().first() {
            if *c == b'0' && digits.len() > 1 && radix == 10 {
                return Err("leading zero in integer constant".into());
            }
        }

        let int_value = u64::from_str_radix(digits, radix).unwrap();

        match self.const_integer_suffix() {
            Ok(suffix) => match suffix {
                ConstTag::Int => {
                    if let Ok(int_value_i64) = i64::try_from(int_value) {
                        if int_value_i64 >= i32::MIN as i64
                            && int_value_i64 <= i32::MAX as i64
                        {
                            self.tok.tag = Tag::ConstInt(int_value_i64);
                            Ok(())
                        } else if int_value_i64 >= i64::MIN
                            && int_value_i64 <= i64::MAX
                        {
                            self.tok.tag = Tag::ConstLong(int_value_i64);
                            Ok(())
                        } else {
                            Err("invalid integer literal".into())
                        }
                    } else if int_value <= u64::MAX {
                        self.tok.tag = Tag::ConstUnsignedLongLong(int_value);
                        Ok(())
                    } else {
                        Err("invalid integer literal".into())
                    }
                }
                ConstTag::Long => {
                    if let Ok(val) = i64::try_from(int_value) {
                        self.tok.tag = Tag::ConstLong(val);
                        Ok(())
                    } else if int_value <= u64::MAX {
                        self.tok.tag = Tag::ConstUnsignedLongLong(int_value);
                        Ok(())
                    } else {
                        Err("invalid integer literal".into())
                    }
                }
                ConstTag::LongLong => {
                    if let Ok(val) = i64::try_from(int_value) {
                        self.tok.tag = Tag::ConstLongLong(val);
                        Ok(())
                    } else if int_value <= u64::MAX {
                        self.tok.tag = Tag::ConstUnsignedLongLong(int_value);
                        Ok(())
                    } else {
                        Err("invalid integer literal".into())
                    }
                }
                ConstTag::UnsignedInt => {
                    if int_value <= u32::MAX as u64 {
                        self.tok.tag = Tag::ConstUnsignedInt(int_value);
                        Ok(())
                    } else if int_value <= u64::MAX {
                        self.tok.tag = Tag::ConstUnsignedLong(int_value);
                        Ok(())
                    } else {
                        Err("invalid integer literal".into())
                    }
                }
                ConstTag::UnsignedLong => {
                    if int_value <= u64::MAX {
                        self.tok.tag = Tag::ConstUnsignedLong(int_value);
                        Ok(())
                    } else {
                        self.tok.tag = Tag::ConstUnsignedLongLong(int_value);
                        Ok(())
                    }
                }
                ConstTag::UnsignedLongLong => {
                    if int_value <= u64::MAX {
                        self.tok.tag = Tag::ConstUnsignedLongLong(int_value);
                        Ok(())
                    } else {
                        Err("invalid integer literal".into())
                    }
                }
            },
            Err(_) => Err("invalid integer literal".into()),
        }
    }

    fn identifier_or_keyword(&mut self) -> Result<(), String> {
        if is_identifier_start(self.peek()) {
            self.take();
            while is_identifier_continue(self.peek()) {
                self.take();
            }
        } else {
            return Err(format!("invalid identifier: {}", self.peek()).into());
        }

        if let Some(tag) = keyword_map(self.text()) {
            self.tok.tag = tag;
        } else {
            self.tok.tag = Tag::Identifier;
        }

        Ok(())
    }

    #[cfg(feature = "simd")]
    fn skip_line_simd(&mut self) {
        let buf = self.buf.as_bytes();
        let mut pos = self.tok.loc.end;

        while pos + 32 < buf.len() {
            let chunk = u8x32::from_slice(&buf[pos..pos + 32]);
            let is_nl = chunk.simd_eq(u8x32::splat(b'\n'));

            let mask = is_nl.to_bitmask();

            if mask != 0 {
                let nl_idx = mask.trailing_zeros() as usize;
                self.tok.loc.end = pos + nl_idx;
                return;
            }

            pos += 32;
        }

        self.tok.loc.end = pos;

        self.skip_line_scalar();
    }

    #[inline]
    fn skip_line_scalar(&mut self) {
        let mut ch: char;

        while {
            ch = self.peek();
            ch != '\0' && ch != '\n'
        } {
            self.take();
        }

        if ch == '\n' {
            self.take();
        }
    }

    fn skip_line(&mut self) {
        #[cfg(feature = "simd")]
        self.skip_line_simd();
        #[cfg(not(feature = "simd"))]
        self.skip_line_scalar();
    }

    #[cfg(feature = "simd")]
    fn skip_block_comment_simd(&mut self) -> Result<(), String> {
        const LANES: usize = 32;
        let buf = self.buf.as_bytes();
        let mut pos = self.tok.loc.end;

        while pos + LANES + 1 <= buf.len() {
            let v = u8x32::from_slice(&buf[pos..pos + LANES]);
            let stars_mask = v.simd_eq(u8x32::splat(b'*')).to_bitmask() as u64;
            let mut mask = stars_mask;
            while mask != 0 {
                let bit = mask.trailing_zeros() as usize;
                let star_idx = pos + bit;
                if buf[star_idx + 1] == b'/' {
                    self.tok.loc.end = star_idx + 2;
                    return Ok(());
                }
                mask &= !(1 << bit);
            }

            pos += LANES;
        }

        self.tok.loc.end = pos;
        self.skip_block_comment_scalar()
    }

    fn skip_block_comment_scalar(&mut self) -> Result<(), String> {
        let mut ch;

        while {
            ch = self.peek();
            ch != '\0'
        } {
            if ch == '*' {
                if self.take_and_peek() == '/' {
                    self.take();
                    break;
                }
            } else {
                self.take();
            }
        }

        if ch == '\0' {
            Err("unterminated block comment".into())
        } else {
            Ok(())
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), String> {
        #[cfg(feature = "simd")]
        return self.skip_block_comment_simd();
        #[cfg(not(feature = "simd"))]
        return self.skip_block_comment_scalar();
    }

    #[cfg(feature = "simd")]
    fn skip_whitespace_simd(&mut self) {
        let buf = self.buf.as_bytes();
        let mut pos = self.tok.loc.end;

        while pos + 32 <= buf.len() {
            let chunk = u8x32::from_slice(&buf[pos..pos + 32]);

            let is_space = chunk.simd_eq(u8x32::splat(b' '));
            let is_tab = chunk.simd_eq(u8x32::splat(b'\t'));
            let is_nl = chunk.simd_eq(u8x32::splat(b'\n'));
            let is_cr = chunk.simd_eq(u8x32::splat(b'\r'));
            let is_vtab = chunk.simd_eq(u8x32::splat(b'\x0b'));
            let is_ff = chunk.simd_eq(u8x32::splat(b'\x0c'));

            let is_ws = is_space | is_tab | is_nl | is_cr | is_vtab | is_ff;
            let mask = is_ws.to_bitmask();

            if mask != u32::MAX as u64 {
                let first_non_ws = (!mask).trailing_zeros() as usize;
                self.tok.loc.end = pos + first_non_ws;
                return;
            }

            pos += 32;
        }

        self.tok.loc.end = pos;
        self.skip_whitespace_scalar();
    }

    fn skip_whitespace_scalar(&mut self) {
        let mut b: u8;
        while {
            b = self.peek_byte();
            b == b' '
                || b == b'\t'
                || b == b'\n'
                || b == b'\r'
                || b == b'\x0b'
                || b == b'\x0c'
        } {
            self.take();
        }
    }

    fn is_whitespace(&mut self) -> bool {
        let b = self.peek_byte();
        b == b' '
            || b == b'\n'
            || b == b'\t'
            || b == b'\r'
            || b == b'\x0b'
            || b == b'\x0c'
    }

    fn skip_whitespace(&mut self) {
        #[cfg(feature = "simd")]
        self.skip_whitespace_simd();
        #[cfg(not(feature = "simd"))]
        self.skip_whitespace_scalar();
    }

    fn scan(&mut self) -> Result<Option<()>, String> {
        if self.is_whitespace() {
            self.skip_whitespace();
        }
        self.tok.loc.start = self.tok.loc.end;
        let ch: char;

        match {
            ch = self.peek_byte() as char;
            ch
        } {
            '/' => match self.take_and_peek_byte() as char {
                '/' => {
                    self.take();
                    self.skip_line();
                    Ok(None)
                }
                '*' => {
                    self.take();
                    self.skip_block_comment()?;
                    Ok(None)
                }
                '=' => {
                    self.take();
                    self.tok.tag = Tag::DivideEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::ForwardSlash;
                    Ok(Some(()))
                }
            },
            '#' => {
                self.take();
                self.skip_line();
                Ok(None)
            }
            '(' => {
                self.take();
                self.tok.tag = Tag::LeftParen;
                Ok(Some(()))
            }
            ')' => {
                self.take();
                self.tok.tag = Tag::RightParen;
                Ok(Some(()))
            }
            '{' => {
                self.take();
                self.tok.tag = Tag::LeftBrace;
                Ok(Some(()))
            }
            '}' => {
                self.take();
                self.tok.tag = Tag::RightBrace;
                Ok(Some(()))
            }
            '[' => {
                self.take();
                self.tok.tag = Tag::LeftBracket;
                Ok(Some(()))
            }
            ']' => {
                self.take();
                self.tok.tag = Tag::RightBracket;
                Ok(Some(()))
            }
            ':' => {
                self.take();
                self.tok.tag = Tag::Colon;
                Ok(Some(()))
            }
            ';' => {
                self.take();
                self.tok.tag = Tag::Semicolon;
                Ok(Some(()))
            }
            ',' => {
                self.take();
                self.tok.tag = Tag::Comma;
                Ok(Some(()))
            }
            '.' => {
                if self.take_and_peek().is_ascii_digit() {
                    self.const_double()?;
                    Ok(Some(()))
                } else {
                    self.tok.tag = Tag::Dot;
                    Ok(Some(()))
                }
            }
            '?' => {
                self.take();
                self.tok.tag = Tag::Question;
                Ok(Some(()))
            }
            '~' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::InvEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Tilde;
                    Ok(Some(()))
                }
            },
            '-' => match self.take_and_peek_byte() as char {
                '-' => {
                    self.take();
                    self.tok.tag = Tag::Decr;
                    Ok(Some(()))
                }
                '=' => {
                    self.take();
                    self.tok.tag = Tag::MinusEq;
                    Ok(Some(()))
                }
                '>' => {
                    self.take();
                    self.tok.tag = Tag::Arrow;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Minus;
                    Ok(Some(()))
                }
            },
            '+' => match self.take_and_peek_byte() as char {
                '+' => {
                    self.take();
                    self.tok.tag = Tag::Incr;
                    Ok(Some(()))
                }
                '=' => {
                    self.take();
                    self.tok.tag = Tag::PlusEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Plus;
                    Ok(Some(()))
                }
            },
            '*' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::MultEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Asterisk;
                    Ok(Some(()))
                }
            },
            '%' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::ModEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Percent;
                    Ok(Some(()))
                }
            },
            '<' => match self.take_and_peek_byte() as char {
                '<' => match self.take_and_peek_byte() as char {
                    '=' => {
                        self.take();
                        self.tok.tag = Tag::LeftShiftEq;
                        Ok(Some(()))
                    }
                    _ => {
                        self.tok.tag = Tag::LeftShift;
                        Ok(Some(()))
                    }
                },
                '=' => {
                    self.take();
                    self.tok.tag = Tag::LessOrEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Less;
                    Ok(Some(()))
                }
            },
            '>' => match self.take_and_peek_byte() as char {
                '>' => match self.take_and_peek_byte() as char {
                    '=' => {
                        self.take();
                        self.tok.tag = Tag::RightShiftEq;
                        Ok(Some(()))
                    }
                    _ => {
                        self.tok.tag = Tag::RightShift;
                        Ok(Some(()))
                    }
                },
                '=' => {
                    self.take();
                    self.tok.tag = Tag::GreaterOrEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Greater;
                    Ok(Some(()))
                }
            },
            '=' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::Eq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Assign;
                    Ok(Some(()))
                }
            },
            '!' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::NotEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Bang;
                    Ok(Some(()))
                }
            },
            '&' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::AndEq;
                    Ok(Some(()))
                }
                '&' => {
                    self.take();
                    self.tok.tag = Tag::LAnd;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Ampersand;
                    Ok(Some(()))
                }
            },
            '^' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::XorEq;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Caret;
                    Ok(Some(()))
                }
            },
            '|' => match self.take_and_peek_byte() as char {
                '=' => {
                    self.take();
                    self.tok.tag = Tag::OrEq;
                    Ok(Some(()))
                }
                '|' => {
                    self.take();
                    self.tok.tag = Tag::LOr;
                    Ok(Some(()))
                }
                _ => {
                    self.tok.tag = Tag::Bar;
                    Ok(Some(()))
                }
            },
            '"' => {
                self.const_string()?;
                Ok(Some(()))
            }
            '\'' => {
                self.const_char()?;
                Ok(Some(()))
            }
            '0'..='9' => {
                self.const_number()?;
                Ok(Some(()))
            }
            '\0' => {
                self.tok.tag = Tag::End;
                Ok(Some(()))
            }
            _ => {
                self.identifier_or_keyword()?;
                Ok(Some(()))
            }
        }
    }

    pub fn advance(&mut self) -> Result<(), String> {
        while self.scan()?.is_none() {}
        Ok(())
    }
}

impl<'buf> Iterator for Tokeniser<'buf> {
    type Item = Result<Token, String>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.advance() {
            Ok(_) => {
                if self.tok.tag != Tag::End {
                    Some(Ok(self.tok.clone()))
                } else {
                    None
                }
            }
            Err(e) => Some(Err(e)),
        }
    }
}

impl<'buf> Token {
    pub fn default() -> Self {
        Self {
            loc: 0..0,
            tag: Tag::Start,
        }
    }

    #[inline]
    pub fn as_str(&self, buf: &'buf str) -> &'buf str {
        &buf[self.loc.clone()]
    }

    #[inline]
    pub fn to_string(&self, buf: &'buf str) -> String {
        self.as_str(buf).to_string()
    }

    #[allow(unused)]
    pub fn line_and_col(&self, buf: &'buf str) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;

        for (i, c) in buf.char_indices() {
            if i >= self.loc.start {
                break;
            }
            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        (line, col)
    }
}
