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

use std::str;

use crate::errors::{
    Error, ErrorClass::Tokenising, TokenisingError, error, error_at,
};
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Debug, PartialEq, Clone)]
pub struct Tokeniser<'buf> {
    buf: &'buf str,
    tok: Token,
    saw: char,
}

pub struct TokeniserIter<'a, 'buf> {
    tokeniser: &'a mut Tokeniser<'buf>,
}

pub use crate::token::{Token, TokenTag};

type Tag = TokenTag;

enum ConstTag {
    Int,
    Long,
    LongLong,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
}

#[inline(always)]
fn keyword_map(ident: &str) -> Tag {
    use TokenTag::*;
    match ident.len() {
        1 => Identifier,
        2 => match ident {
            "if" => If,
            "do" => Do,
            _ => Identifier,
        },
        _ => match ident {
            "auto" => Auto,
            "break" => Break,
            "case" => Case,
            "char" => Char,
            "const" => Const,
            "continue" => Continue,
            "default" => Default,
            "double" => Double,
            "else" => Else,
            "enum" => Enum,
            "extern" => Extern,
            "float" => Float,
            "for" => For,
            "goto" => GoTo,
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
            _ => Identifier,
        },
    }
}

fn hex_digit(b: u8) -> u32 {
    match b {
        b'0'..=b'9' => (b - b'0') as u32,
        b'a'..=b'f' => (b - b'a' + 10) as u32,
        b'A'..=b'F' => (b - b'A' + 10) as u32,
        _ => 0,
    }
}

#[cold]
#[inline(never)]
fn keyword_map_ucn(source: &str) -> Tag {
    use TokenTag::*;
    let mut buf = [0u8; 10];
    let mut len = 0;
    let mut chars = source.bytes();

    while let Some(b) = chars.next() {
        if b != b'\\' {
            if len >= buf.len() || !b.is_ascii() {
                return UcnIdentifier;
            }
            buf[len] = b;
            len += 1;
            continue;
        }

        let cp = match chars.next() {
            Some(b'u') => {
                let a = chars.next().map(hex_digit);
                let b = chars.next().map(hex_digit);
                let c = chars.next().map(hex_digit);
                let d = chars.next().map(hex_digit);
                match (a, b, c, d) {
                    (Some(a), Some(b), Some(c), Some(d)) => {
                        (a << 12) | (b << 8) | (c << 4) | d
                    }
                    _ => return UcnIdentifier,
                }
            }
            Some(b'U') => {
                let hex = [0; 8].map(|_| chars.next().map(hex_digit));
                match hex {
                    [
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                    ] => {
                        (a << 28)
                            | (b << 24)
                            | (c << 20)
                            | (d << 16)
                            | (e << 12)
                            | (f << 8)
                            | (g << 4)
                            | h
                    }
                    _ => return UcnIdentifier,
                }
            }
            _ => return UcnIdentifier,
        };

        if cp > 0x7F || len >= buf.len() {
            return UcnIdentifier;
        }

        buf[len] = cp as u8;
        len += 1;
    }

    match &buf[..len] {
        b"auto" => Auto,
        b"break" => Break,
        b"case" => Case,
        b"char" => Char,
        b"const" => Const,
        b"continue" => Continue,
        b"default" => Default,
        b"do" => Do,
        b"double" => Double,
        b"else" => Else,
        b"enum" => Enum,
        b"extern" => Extern,
        b"float" => Float,
        b"for" => For,
        b"goto" => GoTo,
        b"if" => If,
        b"inline" => Inline,
        b"int" => Int,
        b"long" => Long,
        b"register" => Register,
        b"restrict" => Restrict,
        b"return" => Return,
        b"short" => Short,
        b"signed" => Signed,
        b"sizeof" => SizeOf,
        b"static" => Static,
        b"struct" => Struct,
        b"switch" => Switch,
        b"typedef" => TypeDef,
        b"union" => Union,
        b"unsigned" => Unsigned,
        b"void" => Void,
        b"volatile" => Volatile,
        b"while" => While,
        b"_Bool" => _Bool,
        b"_Complex" => _Complex,
        b"_Imaginary" => _Imaginary,
        _ => UcnIdentifier,
    }
}

#[inline]
fn is_identifier_start(ch: char) -> bool {
    ch.is_ascii_lowercase()
        || ch.is_ascii_uppercase()
        || (ch == '_')
        || (!ch.is_ascii() && is_xid_start(ch))
}

#[inline]
fn is_identifier_continue(ch: char) -> bool {
    ch.is_ascii_lowercase()
        || ch.is_ascii_uppercase()
        || (ch == '_')
        || ch.is_ascii_digit()
        || (!ch.is_ascii() && is_xid_continue(ch))
}

impl<'buf> Tokeniser<'buf> {
    pub fn new(buf: &'buf str) -> Self {
        Self {
            buf,
            tok: Token::default(),
            saw: '\0',
        }
    }

    #[inline(always)]
    pub fn text(&'buf self) -> &'buf str {
        unsafe { self.buf.get_unchecked(self.tok.loc.start..self.tok.loc.end) }
    }

    #[inline(always)]
    fn peek_byte(&mut self) -> u8 {
        let end = self.tok.loc.end;
        let buf = self.buf.as_bytes();
        let b: u8;

        if end == buf.len() {
            b = 0
        } else {
            b = unsafe { *buf.get_unchecked(end) };
            self.saw = b as char;
        }

        b
    }

    #[inline(always)]
    fn peek(&mut self) -> char {
        let b = self.peek_byte();

        if b > 128 {
            let end = self.tok.loc.end;
            let ch =
                unsafe { self.buf[end..].chars().next().unwrap_unchecked() };
            self.saw = ch;
            ch
        } else {
            b as char
        }
    }

    #[inline(always)]
    fn take(&mut self) {
        self.tok.loc.end += self.saw.len_utf8();
    }

    #[inline(always)]
    fn take_byte(&mut self) {
        self.tok.loc.end += 1;
    }

    #[inline(always)]
    fn take_and_peek(&mut self) -> char {
        self.take();
        self.peek()
    }

    #[inline(always)]
    fn take_byte_and_peek_byte(&mut self) -> u8 {
        self.take_byte();
        self.peek_byte()
    }

    fn const_integer_suffix(&mut self) -> Result<ConstTag, ()> {
        let mut is_unsigned = false;
        let mut nr_longs = 0;

        match self.peek_byte() {
            b'u' | b'U' => {
                let ch = self.take_byte_and_peek_byte();
                is_unsigned = true;
                if ch == b'l' || ch == b'L' {
                    self.take_byte();
                    nr_longs = 1;
                    if self.peek_byte() == ch {
                        self.take_byte();
                        nr_longs = 2;
                    }
                }
            }
            b'l' | b'L' => {
                let ch = self.peek_byte();
                let ch2 = self.take_byte_and_peek_byte();
                nr_longs = 1;
                if ch2 == ch {
                    self.take_byte();
                    nr_longs = 2;
                } else if ch2 == b'u' || ch2 == b'U' {
                    self.take_byte();
                    is_unsigned = true;
                }
            }
            _ => {}
        }

        if is_identifier_continue(self.peek()) {
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

    fn const_double(&mut self) -> Result<(), Error> {
        let exp_frac = if self.text().as_bytes().first() == Some(&b'.') {
            false
        } else if self.peek_byte() == b'.' {
            self.take_byte();
            false
        } else {
            self.text().is_empty()
        };

        if exp_frac || self.peek().is_ascii_digit() {
            self.decimal_digits()?;
        }

        if self.peek_byte() == b'e' || self.peek_byte() == b'E' {
            self.take_byte();
            if self.peek_byte() == b'+' || self.peek_byte() == b'-' {
                self.take_byte();
            }
            self.decimal_digits()?;

            if self.peek_byte() == b'f'
                || self.peek_byte() == b'F'
                || self.peek_byte() == b'l'
                || self.peek_byte() == b'L'
            {
                self.take_byte();
            }
        }

        let ch = self.peek();

        if is_identifier_continue(ch) || ch == '.' {
            return Err(error(Tokenising(
                TokenisingError::InvalidDoubleLiteral,
            )));
        }

        let value = match self.text().parse::<f64>() {
            Ok(val) => val,
            Err(_) => {
                return Err(error(Tokenising(
                    TokenisingError::InvalidDoubleLiteral,
                )));
            }
        };

        self.tok.tag = Tag::ConstDouble(value);
        Ok(())
    }

    #[inline(always)]
    fn digits(&mut self, radix: u32) -> (usize, u64, bool) {
        let safe_digits = match radix {
            8 => 21,
            16 => 16,
            _ => 19, // decimal
        };

        let mut len = 0usize;
        let mut value: u64 = 0;

        while len < safe_digits {
            let digit = match self.peek_byte() {
                b @ b'0'..=b'9' => (b - b'0') as u32,
                b @ b'a'..=b'z' => (b - b'a' + 10) as u32,
                b @ b'A'..=b'Z' => (b - b'A' + 10) as u32,
                _ => return (len, value, false),
            };

            if digit >= radix {
                return (len, value, false);
            }

            value = value * radix as u64 + digit as u64;
            self.take_byte();
            len += 1;
        }

        // slow path
        let mut overflowed = false;

        loop {
            let digit = match self.peek_byte() {
                b @ b'0'..=b'9' => (b - b'0') as u32,
                b @ b'a'..=b'z' => (b - b'a' + 10) as u32,
                b @ b'A'..=b'Z' => (b - b'A' + 10) as u32,
                _ => break,
            };

            if digit >= radix {
                break;
            }

            value = match value
                .checked_mul(radix as u64)
                .and_then(|v| v.checked_add(digit as u64))
            {
                Some(v) => v,
                None => {
                    overflowed = true;
                    value
                }
            };

            self.take_byte();
            len += 1;
        }

        (len, value, overflowed)
    }

    fn const_number(&mut self) -> Result<(), Error> {
        let first = self.peek_byte();
        let mut value: u64 = 0;
        let mut overflowed = false;

        if first == b'0' {
            self.take_byte();
            match self.peek_byte() {
                b'x' | b'X' => {
                    self.take_byte();
                    let (len, v, of) = self.digits(16);
                    if len == 0 {
                        return Err(error(Tokenising(
                            TokenisingError::ExpectedHexadecimalDigits,
                        )));
                    }
                    value = v;
                    overflowed = of;
                }
                b'0'..=b'7' => {
                    let (_, v, of) = self.digits(8);
                    value = v;
                    overflowed = of;
                }
                b'.' => {
                    return self.const_double();
                }
                _ => {}
            }
        } else if first == b'.' {
            return self.const_double();
        } else {
            let (_, v, of) = self.digits(10);
            value = v;
            overflowed = of;
        }

        match self.peek_byte() {
            b'.' | b'e' | b'E' => self.const_double(),
            _ => self.const_integer(value, overflowed),
        }
    }

    #[inline]
    fn decimal_digits(&mut self) -> Result<(), Error> {
        let mut len = 0;
        while self.peek_byte().is_ascii_digit() {
            self.take_byte();
            len += 1;
        }
        if len > 0 {
            Ok(())
        } else {
            Err(error(Tokenising(TokenisingError::ExpectedDigits)))
        }
    }

    #[cold]
    #[inline(never)]
    fn escape_sequence_value(&mut self) -> Result<u32, Error> {
        let mut ch = self.peek();

        match ch {
            '\\' | '\'' | '"' | '?' | 'n' | 'r' | 't' | 'a' | 'b' | 'f'
            | 'v' => {
                self.take_byte();
                Ok(ch as u32)
            }
            '0' => {
                self.take_byte();
                Ok(0)
            }
            'x' => {
                ch = self.take_and_peek();

                if !ch.is_ascii_hexdigit() {
                    return Err(error(Tokenising(
                        TokenisingError::NotValidHexEscapeSequence,
                    )));
                }

                let mut value = hex_digit(ch as u8);
                ch = self.take_and_peek();

                while ch.is_ascii_hexdigit() {
                    value = value * 16 + hex_digit(ch as u8);
                    ch = self.take_and_peek();
                }

                Ok(value & 0xFF)
            }
            'u' => {
                self.take_byte();
                let mut cp: u32 = 0;
                for _ in 0..4 {
                    ch = self.peek();

                    if !ch.is_ascii_hexdigit() {
                        return Err(error(Tokenising(
                            TokenisingError::NotValidUnicodeEscapeSequence,
                        )));
                    }
                    cp = cp * 16 + hex_digit(ch as u8);
                    self.take_byte();
                }
                Ok(cp)
            }
            'U' => {
                self.take_byte();
                let mut cp: u32 = 0;
                for _ in 0..8 {
                    ch = self.peek();

                    if !ch.is_ascii_hexdigit() {
                        return Err(error(Tokenising(
                            TokenisingError::NotValidUnicodeEscapeSequence,
                        )));
                    }
                    cp = cp * 16 + hex_digit(ch as u8);
                    self.take_byte();
                }
                Ok(cp)
            }
            _ if ('1'..='7').contains(&ch) => {
                let mut value = (ch as u32) - ('0' as u32);
                self.take_byte();

                ch = self.peek();
                if ('0'..='7').contains(&ch) {
                    value = value * 8 + (ch as u32) - ('0' as u32);
                    self.take_byte();

                    ch = self.peek();
                    if ('0'..='7').contains(&ch) {
                        value = value * 8 + (ch as u32) - ('0' as u32);
                        self.take_byte();
                    }
                }

                Ok(value)
            }
            _ => Err(error(Tokenising(
                TokenisingError::NotValidOctalEscapeSequence,
            ))),
        }
    }

    #[cold]
    #[inline(never)]
    fn escape_sequence(&mut self) -> Result<(), Error> {
        let mut ch = self.peek();

        match ch {
            '\\' | '\'' | '"' | '?' | 'n' | 'r' | 't' | 'a' | 'b' | 'f'
            | 'v' => {
                self.take_byte();
                Ok(())
            }
            '0' => {
                self.take_byte();
                Ok(())
            }
            'x' => {
                ch = self.take_and_peek();

                if !ch.is_ascii_hexdigit() {
                    return Err(error(Tokenising(
                        TokenisingError::NotValidHexEscapeSequence,
                    )));
                }

                ch = self.take_and_peek();

                while ch.is_ascii_hexdigit() {
                    ch = self.take_and_peek();
                }

                Ok(())
            }
            'u' => {
                self.take_byte();
                for _ in 0..4 {
                    ch = self.peek();

                    if !ch.is_ascii_hexdigit() {
                        return Err(error(Tokenising(
                            TokenisingError::NotValidUnicodeEscapeSequence,
                        )));
                    }
                    self.take_byte();
                }
                Ok(())
            }
            'U' => {
                self.take_byte();
                for _ in 0..8 {
                    ch = self.peek();

                    if !ch.is_ascii_hexdigit() {
                        return Err(error(Tokenising(
                            TokenisingError::NotValidUnicodeEscapeSequence,
                        )));
                    }
                    self.take_byte();
                }
                Ok(())
            }
            _ if ('1'..='7').contains(&ch) => {
                self.take_byte();

                ch = self.peek();
                if ('0'..='7').contains(&ch) {
                    self.take_byte();

                    ch = self.peek();
                    if ('0'..='7').contains(&ch) {
                        self.take_byte();
                    }
                }

                Ok(())
            }
            _ => Err(error(Tokenising(
                TokenisingError::NotValidOctalEscapeSequence,
            ))),
        }
    }

    fn const_string(&mut self) -> Result<(), Error> {
        let mut has_esc_seq = false;
        loop {
            self.take_byte();
            loop {
                let b = self.peek_byte();
                match b {
                    b'"' | b'\0' => break,
                    b'\\' => {
                        has_esc_seq = true;
                        self.take_byte();
                        self.escape_sequence()?;
                    }
                    0x80..=0xFF => {
                        self.peek();
                        self.take();
                    }
                    _ => self.take_byte(),
                }
            }
            self.take_byte();
            if self.peek() != '"' {
                break;
            }
        }
        self.tok.tag = Tag::ConstString(has_esc_seq);
        Ok(())
    }

    fn const_char(&mut self) -> Result<(), Error> {
        self.take_byte();

        let mut value: u32 = 0;
        let mut count = 0u32;

        loop {
            let b = self.peek_byte();
            match b {
                b'\'' | b'\0' => break,
                b'\\' => {
                    self.take_byte();
                    value =
                        value.rotate_left(8) | self.escape_sequence_value()?;
                }
                0x80..=0xFF => {
                    let ch = self.peek();
                    value = value.rotate_left(8) | ch as u32;
                    self.take();
                }
                _ => {
                    value = value.rotate_left(8) | b as u32;
                    self.take_byte();
                }
            }
            count += 1;
        }

        self.take_byte();

        if count == 0 {
            return Err(error(Tokenising(TokenisingError::InvalidCharLiteral)));
        }

        let c = char::from_u32(value).unwrap_or('\0');
        self.tok.tag = Tag::ConstChar(c);

        Ok(())
    }

    fn const_integer(
        &mut self,
        value: u64,
        overflowed: bool,
    ) -> Result<(), Error> {
        let suffix = match self.const_integer_suffix() {
            Ok(suffix) => suffix,
            Err(_) => {
                return Err(error(Tokenising(
                    TokenisingError::InvalidIntegerLiteral,
                )));
            }
        };

        if overflowed {
            return Err(error(Tokenising(
                TokenisingError::InvalidIntegerLiteral,
            )));
        }

        self.tok.tag = match suffix {
            ConstTag::Int => match i64::try_from(value) {
                Ok(v) if v >= i32::MIN as i64 && v <= i32::MAX as i64 => {
                    Tag::ConstInt(v)
                }
                Ok(v) => Tag::ConstLong(v),
                Err(_) => Tag::ConstUnsignedLongLong(value),
            },
            ConstTag::Long => match i64::try_from(value) {
                Ok(v) => Tag::ConstLong(v),
                Err(_) => Tag::ConstUnsignedLongLong(value),
            },
            ConstTag::LongLong => match i64::try_from(value) {
                Ok(v) => Tag::ConstLongLong(v),
                Err(_) => Tag::ConstUnsignedLongLong(value),
            },
            ConstTag::UnsignedInt => {
                if value <= u32::MAX as u64 {
                    Tag::ConstUnsignedInt(value)
                } else {
                    Tag::ConstUnsignedLong(value)
                }
            }
            ConstTag::UnsignedLong => Tag::ConstUnsignedLong(value),
            ConstTag::UnsignedLongLong => Tag::ConstUnsignedLongLong(value),
        };

        Ok(())
    }

    #[cold]
    #[inline(never)]
    fn identifier_ucn(&mut self) -> Result<(), Error> {
        loop {
            self.take_byte();

            match self.peek() {
                'u' => {
                    self.take_byte();
                    for _ in 0..4 {
                        if !self.peek().is_ascii_hexdigit() {
                            return Err(error(Tokenising(
                                TokenisingError::NotValidUnicodeEscapeSequence,
                            )));
                        }
                        self.take_byte();
                    }
                }
                'U' => {
                    self.take_byte();
                    for _ in 0..8 {
                        if !self.peek().is_ascii_hexdigit() {
                            return Err(error(Tokenising(
                                TokenisingError::NotValidUnicodeEscapeSequence,
                            )));
                        }
                        self.take_byte();
                    }
                }
                _ => {
                    return Err(error(Tokenising(
                        TokenisingError::NotValidUnicodeEscapeSequence,
                    )));
                }
            }

            self.identifier_continue();

            if self.peek_byte() != b'\\' {
                break;
            }
        }

        self.tok.tag = keyword_map_ucn(self.text());

        Ok(())
    }

    #[inline]
    fn identifier(&mut self) -> Result<(), Error> {
        if is_identifier_start(self.peek()) {
            self.take();
            self.identifier_continue();
        } else {
            return Err(error_at(
                self.tok.loc,
                Tokenising(TokenisingError::InvalidIdentifier(
                    self.peek().to_string(),
                )),
            ));
        }

        let res = {
            let b =
                unsafe { *self.buf.as_bytes().get_unchecked(self.tok.loc.end) };
            b != b'\\'
        };
        if res {
            self.tok.tag = keyword_map(self.text());
            Ok(())
        } else {
            self.identifier_ucn()
        }
    }

    #[cfg(feature = "simd")]
    fn identifier_continue_simd(&mut self) {
        const LANES: usize = 16;
        let buf = self.buf.as_bytes();
        let mut pos = self.tok.loc.end;

        while pos + LANES <= buf.len() {
            let chunk = u8x16::from_slice(&buf[pos..pos + LANES]);

            let is_lower =
                (chunk - u8x16::splat(b'a')).simd_le(u8x16::splat(b'z' - b'a'));
            let is_upper =
                (chunk - u8x16::splat(b'A')).simd_le(u8x16::splat(b'Z' - b'A'));
            let is_digit =
                (chunk - u8x16::splat(b'0')).simd_le(u8x16::splat(b'9' - b'0'));
            let is_underscore = chunk.simd_eq(u8x16::splat(b'_'));

            let is_ident = is_lower | is_upper | is_digit | is_underscore;
            let mask = is_ident.to_bitmask();

            if mask != u16::MAX as u64 {
                let first_non_ident = (!mask).trailing_zeros() as usize;
                self.tok.loc.end = pos + first_non_ident;
                return;
            }

            pos += LANES;
        }

        self.tok.loc.end = pos;
        self.identifier_continue_scalar();
    }

    #[inline]
    fn identifier_continue_scalar(&mut self) {
        while is_identifier_continue(self.peek()) {
            self.take();
        }
    }

    #[inline(always)]
    fn identifier_continue(&mut self) {
        #[cfg(feature = "simd")]
        self.identifier_continue_simd();
        #[cfg(not(feature = "simd"))]
        self.identifier_continue_scalar();
    }

    #[cfg(feature = "simd")]
    fn skip_line_simd(&mut self) {
        let buf = self.buf.as_bytes();
        let mut pos = self.tok.loc.end;

        while pos + 64 < buf.len() {
            let chunk = u8x64::from_slice(&buf[pos..pos + 64]);
            let is_nl = chunk.simd_eq(u8x64::splat(b'\n'));

            let mask = is_nl.to_bitmask();

            if mask != 0 {
                let nl_idx = mask.trailing_zeros() as usize;
                self.tok.loc.end = pos + nl_idx;
                return;
            }

            pos += 64;
        }

        self.tok.loc.end = pos;

        self.skip_line_scalar();
    }

    #[inline]
    fn skip_line_scalar(&mut self) {
        let mut ch: u8;

        while {
            ch = self.peek_byte();
            ch != b'\n' && ch != b'\0'
        } {
            self.take_byte();
        }

        if ch == b'\n' {
            self.take_byte();
        }
    }

    fn skip_line(&mut self) {
        #[cfg(feature = "simd")]
        self.skip_line_simd();
        #[cfg(not(feature = "simd"))]
        self.skip_line_scalar();
    }

    #[cfg(feature = "simd")]
    fn skip_block_comment_simd(&mut self) -> Result<(), Error> {
        const LANES: usize = 64;
        let buf = self.buf.as_bytes();
        let mut pos = self.tok.loc.end;

        while pos + LANES < buf.len() {
            let v = u8x64::from_slice(&buf[pos..pos + LANES]);
            let stars = v.simd_eq(u8x64::splat(b'*')).to_bitmask();
            if stars != 0 {
                let slashes = v.simd_eq(u8x64::splat(b'/')).to_bitmask();
                let pair_mask = stars & (slashes >> 1);
                if pair_mask != 0 {
                    let bit = pair_mask.trailing_zeros() as usize;
                    self.tok.loc.end = pos + bit + 2;
                    return Ok(());
                }
                if stars & (1 << (LANES - 1)) != 0 && buf[pos + LANES] == b'/' {
                    self.tok.loc.end = pos + LANES + 1;
                    return Ok(());
                }
            }
            pos += LANES;
        }

        self.tok.loc.end = pos;
        self.skip_block_comment_scalar()
    }

    fn skip_block_comment_scalar(&mut self) -> Result<(), Error> {
        let mut ch: u8;

        while {
            ch = self.peek_byte();
            ch != b'\0'
        } {
            if ch == b'*' {
                if self.take_byte_and_peek_byte() == b'/' {
                    self.take_byte();
                    break;
                }
            } else {
                self.take_byte();
            }
        }

        if ch == b'\0' {
            Err(error(Tokenising(TokenisingError::UnterminatedBlockComment)))
        } else {
            Ok(())
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), Error> {
        #[cfg(feature = "simd")]
        return self.skip_block_comment_simd();
        #[cfg(not(feature = "simd"))]
        return self.skip_block_comment_scalar();
    }

    #[cfg(feature = "simd")]
    fn skip_whitespace_simd(&mut self) {
        let buf = self.buf.as_bytes();
        let mut pos = self.tok.loc.end;

        while pos + 16 <= buf.len() {
            let chunk = u8x16::from_slice(&buf[pos..pos + 16]);
            let is_ws_range =
                (chunk - u8x16::splat(0x09)).simd_le(u8x16::splat(4));
            let is_space = chunk.simd_eq(u8x16::splat(b' '));
            let is_ws = is_ws_range | is_space;
            let mask = is_ws.to_bitmask();

            if mask != u16::MAX as u64 {
                let first_non_ws = (!mask).trailing_zeros() as usize;
                self.tok.loc.end = pos + first_non_ws;
                return;
            }

            pos += 16;
        }

        self.tok.loc.end = pos;
        self.skip_whitespace_scalar();
    }

    #[inline]
    fn skip_whitespace_scalar(&mut self) {
        let mut b: u8;
        while {
            b = self.peek_byte();
            b == b' ' || b == b'\n' || (b > 8 && b < 14)
        } {
            self.take_byte();
        }
    }

    #[inline]
    fn is_whitespace(&mut self) -> bool {
        let b = self.peek_byte();
        b == b' ' || (b > 8 && b < 14)
    }

    fn skip_whitespace(&mut self) {
        #[cfg(feature = "simd")]
        {
            let buf = self.buf.as_bytes();
            let mut pos = self.tok.loc.end;
            let start = pos;

            // No bounds check needed, buffer padding guarantees this is safe
            while pos - start < 4 {
                let b = unsafe { *buf.get_unchecked(pos) };
                if !((b > 8 && b < 14) || b == b' ') {
                    self.tok.loc.end = pos;
                    return;
                }
                pos += 1;
            }

            self.tok.loc.end = pos;
        }
        #[cfg(feature = "simd")]
        self.skip_whitespace_simd();
        #[cfg(not(feature = "simd"))]
        self.skip_whitespace_scalar();
    }

    #[inline]
    fn scan(&mut self) -> Result<(), Error> {
        loop {
            if self.is_whitespace() {
                self.skip_whitespace();
            }
            self.tok.loc.start = self.tok.loc.end;
            let ch: u8 = self.peek_byte();

            if ch == b'(' {
                self.take_byte();
                self.tok.tag = Tag::LeftParen;
            } else {
                match ch {
                    b'/' => match self.take_byte_and_peek_byte() {
                        b'/' => {
                            self.take_byte();
                            self.skip_line();
                            continue;
                        }
                        b'*' => {
                            self.take_byte();
                            self.skip_block_comment()?;
                            continue;
                        }
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::DivideEq;
                        }
                        _ => {
                            self.tok.tag = Tag::ForwardSlash;
                        }
                    },
                    b'#' => {
                        self.take_byte();
                        self.skip_line();
                        continue;
                    }
                    b')' => {
                        self.take_byte();
                        self.tok.tag = Tag::RightParen;
                    }
                    b'{' => {
                        self.take_byte();
                        self.tok.tag = Tag::LeftBrace;
                    }
                    b'}' => {
                        self.take_byte();
                        self.tok.tag = Tag::RightBrace;
                    }
                    b'[' => {
                        self.take_byte();
                        self.tok.tag = Tag::LeftBracket;
                    }
                    b']' => {
                        self.take_byte();
                        self.tok.tag = Tag::RightBracket;
                    }
                    b':' => {
                        self.take_byte();
                        self.tok.tag = Tag::Colon;
                    }
                    b';' => {
                        self.take_byte();
                        self.tok.tag = Tag::Semicolon;
                    }
                    b',' => {
                        self.take_byte();
                        self.tok.tag = Tag::Comma;
                    }
                    b'.' => {
                        if self.take_and_peek().is_ascii_digit() {
                            self.const_double()?;
                        } else {
                            self.tok.tag = Tag::Dot;
                        }
                    }
                    b'?' => {
                        self.take_byte();
                        self.tok.tag = Tag::Question;
                    }
                    b'~' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::InvEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Tilde;
                        }
                    },
                    b'-' => match self.take_byte_and_peek_byte() {
                        b'-' => {
                            self.take_byte();
                            self.tok.tag = Tag::Decr;
                        }
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::MinusEq;
                        }
                        b'>' => {
                            self.take_byte();
                            self.tok.tag = Tag::Arrow;
                        }
                        _ => {
                            self.tok.tag = Tag::Minus;
                        }
                    },
                    b'+' => match self.take_byte_and_peek_byte() {
                        b'+' => {
                            self.take_byte();
                            self.tok.tag = Tag::Incr;
                        }
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::PlusEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Plus;
                        }
                    },
                    b'*' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::MultEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Asterisk;
                        }
                    },
                    b'%' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::ModEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Percent;
                        }
                    },
                    b'<' => match self.take_byte_and_peek_byte() {
                        b'<' => match self.take_byte_and_peek_byte() {
                            b'=' => {
                                self.take_byte();
                                self.tok.tag = Tag::LeftShiftEq;
                            }
                            _ => {
                                self.tok.tag = Tag::LeftShift;
                            }
                        },
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::LessOrEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Less;
                        }
                    },
                    b'>' => match self.take_byte_and_peek_byte() {
                        b'>' => match self.take_byte_and_peek_byte() {
                            b'=' => {
                                self.take_byte();
                                self.tok.tag = Tag::RightShiftEq;
                            }
                            _ => {
                                self.tok.tag = Tag::RightShift;
                            }
                        },
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::GreaterOrEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Greater;
                        }
                    },
                    b'=' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::Eq;
                        }
                        _ => {
                            self.tok.tag = Tag::Assign;
                        }
                    },
                    b'!' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::NotEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Bang;
                        }
                    },
                    b'&' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::AndEq;
                        }
                        b'&' => {
                            self.take_byte();
                            self.tok.tag = Tag::LAnd;
                        }
                        _ => {
                            self.tok.tag = Tag::Ampersand;
                        }
                    },
                    b'^' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::XorEq;
                        }
                        _ => {
                            self.tok.tag = Tag::Caret;
                        }
                    },
                    b'|' => match self.take_byte_and_peek_byte() {
                        b'=' => {
                            self.take_byte();
                            self.tok.tag = Tag::OrEq;
                        }
                        b'|' => {
                            self.take_byte();
                            self.tok.tag = Tag::LOr;
                        }
                        _ => {
                            self.tok.tag = Tag::Bar;
                        }
                    },
                    b'"' => {
                        self.const_string()?;
                    }
                    b'\'' => {
                        self.const_char()?;
                    }
                    b'0'..=b'9' => {
                        self.const_number()?;
                    }
                    b'\0' => {
                        self.tok.tag = Tag::End;
                    }
                    _ => {
                        self.identifier()?;
                    }
                }
            }
            break;
        }
        Ok(())
    }

    pub fn iter(&mut self) -> TokeniserIter<'_, 'buf> {
        TokeniserIter { tokeniser: self }
    }
}

impl<'a, 'buf> Iterator for TokeniserIter<'a, 'buf> {
    type Item = Result<&'a Token, Error>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.tokeniser.scan() {
            Ok(_) => {
                if self.tokeniser.tok.tag != Tag::End {
                    let ptr: *const Token = &self.tokeniser.tok;
                    Some(Ok(unsafe { &*ptr }))
                } else {
                    None
                }
            }
            Err(e) => Some(Err(e)),
        }
    }
}

impl<'a, 'buf> IntoIterator for &'a mut Tokeniser<'buf> {
    type Item = Result<&'a Token, Error>;
    type IntoIter = TokeniserIter<'a, 'buf>;

    fn into_iter(self) -> Self::IntoIter {
        TokeniserIter { tokeniser: self }
    }
}
