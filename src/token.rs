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

use crate::errors::SourceLoc;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token {
    pub loc: SourceLoc,
    pub tag: TokenTag,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    UcnIdentifier,
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

impl std::fmt::Display for TokenTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTag::Start => write!(f, "start"),
            TokenTag::LeftParen => write!(f, "("),
            TokenTag::RightParen => write!(f, ")"),
            TokenTag::LeftBrace => write!(f, "{{"),
            TokenTag::RightBrace => write!(f, "}}"),
            TokenTag::LeftBracket => write!(f, "["),
            TokenTag::RightBracket => write!(f, "]"),
            TokenTag::Colon => write!(f, ":"),
            TokenTag::Semicolon => write!(f, ";"),
            TokenTag::Comma => write!(f, ","),
            TokenTag::Dot => write!(f, "."),
            TokenTag::Arrow => write!(f, "->"),
            TokenTag::DoubleQuote => write!(f, "\""),
            TokenTag::Quote => write!(f, "'"),
            TokenTag::Question => write!(f, "?"),
            TokenTag::Tilde => write!(f, "~"),
            TokenTag::Minus => write!(f, "-"),
            TokenTag::MinusEq => write!(f, "-="),
            TokenTag::Plus => write!(f, "+"),
            TokenTag::PlusEq => write!(f, "+="),
            TokenTag::Asterisk => write!(f, "*"),
            TokenTag::MultEq => write!(f, "*="),
            TokenTag::ForwardSlash => write!(f, "/"),
            TokenTag::DivideEq => write!(f, "/="),
            TokenTag::Percent => write!(f, "%"),
            TokenTag::ModEq => write!(f, "%="),
            TokenTag::LeftShift => write!(f, "<<"),
            TokenTag::LeftShiftEq => write!(f, "<<="),
            TokenTag::RightShift => write!(f, ">>"),
            TokenTag::RightShiftEq => write!(f, ">>="),
            TokenTag::Less => write!(f, "<"),
            TokenTag::LessOrEq => write!(f, "<="),
            TokenTag::Greater => write!(f, ">"),
            TokenTag::GreaterOrEq => write!(f, ">="),
            TokenTag::Eq => write!(f, "=="),
            TokenTag::NotEq => write!(f, "!="),
            TokenTag::Assign => write!(f, "="),
            TokenTag::Ampersand => write!(f, "&"),
            TokenTag::AndEq => write!(f, "&="),
            TokenTag::Bar => write!(f, "|"),
            TokenTag::OrEq => write!(f, "|="),
            TokenTag::Caret => write!(f, "^"),
            TokenTag::XorEq => write!(f, "^="),
            TokenTag::LAnd => write!(f, "&&"),
            TokenTag::LOr => write!(f, "||"),
            TokenTag::Bang => write!(f, "!"),
            TokenTag::InvEq => write!(f, "!="),
            TokenTag::Decr => write!(f, "--"),
            TokenTag::Incr => write!(f, "++"),
            TokenTag::Void => write!(f, "void"),
            TokenTag::Int => write!(f, "int"),
            TokenTag::Long => write!(f, "long"),
            TokenTag::Signed => write!(f, "signed"),
            TokenTag::Unsigned => write!(f, "unsigned"),
            TokenTag::Double => write!(f, "double"),
            TokenTag::GoTo => write!(f, "goto"),
            TokenTag::Return => write!(f, "return"),
            TokenTag::If => write!(f, "if"),
            TokenTag::Else => write!(f, "else"),
            TokenTag::Do => write!(f, "do"),
            TokenTag::While => write!(f, "while"),
            TokenTag::For => write!(f, "for"),
            TokenTag::Switch => write!(f, "switch"),
            TokenTag::Case => write!(f, "case"),
            TokenTag::Default => write!(f, "default"),
            TokenTag::Break => write!(f, "break"),
            TokenTag::Continue => write!(f, "continue"),
            TokenTag::Static => write!(f, "static"),
            TokenTag::Extern => write!(f, "extern"),
            TokenTag::Auto => write!(f, "auto"),
            TokenTag::Register => write!(f, "register"),
            TokenTag::Const => write!(f, "const"),
            TokenTag::Volatile => write!(f, "volatile"),
            TokenTag::Restrict => write!(f, "restrict"),
            TokenTag::Union => write!(f, "union"),
            TokenTag::Enum => write!(f, "enum"),
            TokenTag::Struct => write!(f, "struct"),
            TokenTag::TypeDef => write!(f, "typedef"),
            TokenTag::SizeOf => write!(f, "sizeof"),
            TokenTag::Short => write!(f, "short"),
            TokenTag::Inline => write!(f, "inline"),
            TokenTag::Float => write!(f, "float"),
            TokenTag::Char => write!(f, "char"),
            TokenTag::_Bool => write!(f, "_Bool"),
            TokenTag::_Complex => write!(f, "_Complex"),
            TokenTag::_Imaginary => write!(f, "_Imaginary"),
            TokenTag::Identifier => write!(f, "identifier"),
            TokenTag::UcnIdentifier => write!(f, "identifier"),
            TokenTag::ConstInt(val) => write!(f, "{}", val),
            TokenTag::ConstLong(val) => write!(f, "{}L", val),
            TokenTag::ConstLongLong(val) => write!(f, "{}LL", val),
            TokenTag::ConstUnsignedInt(val) => write!(f, "{}U", val),
            TokenTag::ConstUnsignedLong(val) => write!(f, "{}UL", val),
            TokenTag::ConstUnsignedLongLong(val) => write!(f, "{}ULL", val),
            TokenTag::ConstDouble(val) => write!(f, "{}", val),
            TokenTag::ConstString(escaped) => {
                if *escaped {
                    write!(f, "string literal")
                } else {
                    write!(f, "\"...\"")
                }
            }
            TokenTag::ConstChar(c) => write!(f, "'{}'", c),
            TokenTag::End => write!(f, "end of file"),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            loc: SourceLoc { start: 0, end: 0 },
            tag: TokenTag::Start,
        }
    }
}

impl<'buf> Token {
    #[inline]
    pub fn as_str(&self, buf: &'buf str) -> &'buf str {
        &buf[self.loc.start..self.loc.end]
    }

    #[inline]
    pub fn to_string(&self, buf: &'buf str) -> String {
        self.as_str(buf).to_string()
    }
}
