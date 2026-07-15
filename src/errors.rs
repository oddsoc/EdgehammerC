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

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLoc {
    pub start: usize,
    pub end: usize,
}

impl SourceLoc {
    pub fn line_and_col(&self, buf: &str) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        let bytes = buf.as_bytes();

        let mut chars = buf.char_indices();

        while let Some((i, c)) = chars.next() {
            if i >= self.start {
                break;
            }

            if c == '#' && (i == 0 || bytes[i - 1] == b'\n') {
                let after_hash = &buf[i + c.len_utf8()..];
                let trimmed = after_hash.trim_start();

                let after_keyword =
                    if let Some(rest) = trimmed.strip_prefix("line") {
                        rest.trim_start()
                    } else {
                        trimmed
                    };

                let num_str = after_keyword;
                let num_len = num_str
                    .bytes()
                    .position(|b| !b.is_ascii_digit())
                    .unwrap_or(num_str.len());

                if num_len > 0 {
                    let num: usize = num_str[..num_len].parse().unwrap();
                    for (j, ch) in chars.by_ref() {
                        if j >= self.start {
                            break;
                        }
                        if ch == '\n' {
                            break;
                        }
                    }
                    line = num;
                    col = 1;
                    continue;
                }
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

#[derive(Debug, Clone)]
#[allow(unused)]
pub struct Error {
    pub loc: Option<SourceLoc>,
    pub class: ErrorClass,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum TokenisingError {
    InvalidDoubleLiteral,
    ExpectedOctalDigits,
    ExpectedHexadecimalDigits,
    ExpectedDigits,
    NotValidHexEscapeSequence,
    NotValidUnicodeEscapeSequence,
    NotValidOctalEscapeSequence,
    LeadingZeroInIntegerConstant,
    InvalidIntegerLiteral,
    InvalidIdentifier(String),
    UnterminatedBlockComment,
    InvalidCharLiteral,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum ParsingError {
    ExpectedTokButGot(String),
    UnexpectedEof,
    TypeNamesCannotHaveStorageClass,
    ArrayDimensionCannotBeNegative(i64),
    ArrayDimensionMustBeConstantIntegerExpression,
    InvalidTypeSpecifier,
    InvalidStorageClass,
    ExcessElementsInArrayInitialiser,
    InvalidTypeQualifier,
    UnknownStorageClassSpecifier,
    LoopInitialDeclarationIsInvalid,
    LabelAlreadyDefined(String),
    InvalidPostfixExpression,
    UndeclaredIdentifier(String),
    ADeclaratorListCannotContainFunctionDefinition,
    AbstractDeclaratorCannotHaveIdentifier,
    MalformedBinaryExpression,
    MalformedExpression,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum TypeCheckingError {
    CannotConvertTypeForAssign,
    IncompatibleExprTypes,
    TypeMismatch,
    CannotReturnArray,
    SurplusVoidParam,
    CannotReturnFunction,
    ExpectedScalarType,
    ExpectedArithmeticType,
    NonIntegerSwitchExprType,
    CannotAssignToArrayType,
    CannotAssignToNonLvalue,
    InvalidSubscriptOperands,
    InvalidAddOperands,
    SubtractingDifferingPointers,
    InvalidSubtractOperands,
    ExpectedIntegerType,
    DereferencingRvalue,
    ComparePointerNonZeroInteger,
    IncompatibleTypes,
    CannotCastPointerToDouble,
    CannotCastDoubleToPointer,
    TooManyArguments,
    TooFewArguments,
    FunctionArray,
    EmptyInitialiserList,
    IncompatibleElementInArrayInitialiser,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum SemanticError {
    UnnamedParameterInFunctionDefinition,
    NotAConstExpression,
    NotAFunction,
    NotAnLvalue,
    ContinueNotInALoop,
    BreakNotInALoopOrSwitch,
    LabelNotFound(String),
    CaseOutsideOfSwitch,
    DefaultOutsideOfSwitch,
    DuplicateCaseExpression,
    DuplicateDefaultCase,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum SymbolError {
    InvalidStorageClassForFunction(String),
    MultipleDefinitions(String),
    RedeclarationWithNoLinkage(String),
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum ErrorClass {
    Tokenising(TokenisingError),
    Parsing(ParsingError),
    TypeChecking(TypeCheckingError),
    Semantic(SemanticError),
    Symbolic(SymbolError),
}

impl TokenisingError {
    pub fn message(&self) -> String {
        match self {
            TokenisingError::InvalidDoubleLiteral => {
                "invalid double literal".to_string()
            }
            TokenisingError::ExpectedOctalDigits => {
                "expected octal digits".to_string()
            }
            TokenisingError::ExpectedHexadecimalDigits => {
                "expected hexadecimal digits".to_string()
            }
            TokenisingError::ExpectedDigits => "expected digits".to_string(),
            TokenisingError::NotValidHexEscapeSequence => {
                "not a valid hex escape sequence".to_string()
            }
            TokenisingError::NotValidUnicodeEscapeSequence => {
                "not a valid unicode escape sequence".to_string()
            }
            TokenisingError::NotValidOctalEscapeSequence => {
                "not a valid octal escape sequence".to_string()
            }
            TokenisingError::LeadingZeroInIntegerConstant => {
                "leading zero in integer constant".to_string()
            }
            TokenisingError::InvalidIntegerLiteral => {
                "invalid integer literal".to_string()
            }
            TokenisingError::InvalidIdentifier(ident) => {
                format!("invalid identifier: {}", ident)
            }
            TokenisingError::UnterminatedBlockComment => {
                "unterminated block comment".to_string()
            }
            TokenisingError::InvalidCharLiteral => {
                "invalid character literal".to_string()
            }
        }
    }
}

impl ParsingError {
    pub fn message(&self) -> String {
        match self {
            ParsingError::ExpectedTokButGot(got) => {
                format!("unexpected token '{}'", got)
            }
            ParsingError::UnexpectedEof => "unexpected end of file".to_string(),
            ParsingError::TypeNamesCannotHaveStorageClass => {
                "type names cannot have a storage class".to_string()
            }
            ParsingError::ArrayDimensionCannotBeNegative(val) => {
                format!("array dimension cannot be negative: {}", val)
            }
            ParsingError::ArrayDimensionMustBeConstantIntegerExpression => {
                "array dimension must be a constant integer expression"
                    .to_string()
            }
            ParsingError::InvalidTypeSpecifier => {
                "invalid type specifier".to_string()
            }
            ParsingError::InvalidStorageClass => {
                "invalid storage class".to_string()
            }
            ParsingError::ExcessElementsInArrayInitialiser => {
                "excess elements in array initialiser".to_string()
            }
            ParsingError::InvalidTypeQualifier => {
                "invalid type qualifier".to_string()
            }
            ParsingError::UnknownStorageClassSpecifier => {
                "unknown storage class specifier".to_string()
            }
            ParsingError::LoopInitialDeclarationIsInvalid => {
                "loop initial declaration is invalid".to_string()
            }
            ParsingError::LabelAlreadyDefined(label) => {
                format!("'{}' label already defined", label)
            }
            ParsingError::InvalidPostfixExpression => {
                "invalid postfix expression".to_string()
            }
            ParsingError::UndeclaredIdentifier(ident) => {
                format!("'{}' undeclared", ident)
            }
            ParsingError::ADeclaratorListCannotContainFunctionDefinition => {
                "a declarator list cannot contain a function definition"
                    .to_string()
            }
            ParsingError::AbstractDeclaratorCannotHaveIdentifier => {
                "abstract declarator cannot have an identifier".to_string()
            }
            ParsingError::MalformedBinaryExpression => {
                "malformed binary expression".to_string()
            }
            ParsingError::MalformedExpression => {
                "malformed expression".to_string()
            }
        }
    }
}

impl TypeCheckingError {
    pub fn message(&self) -> String {
        match self {
            TypeCheckingError::CannotConvertTypeForAssign => {
                "cannot convert type for assignment".to_string()
            }
            TypeCheckingError::IncompatibleExprTypes => {
                "expressions have incompatible types".to_string()
            }
            TypeCheckingError::TypeMismatch => "mismatching types".to_string(),
            TypeCheckingError::CannotReturnArray => {
                "a function cannot return an array".to_string()
            }
            TypeCheckingError::SurplusVoidParam => {
                "void must be the only parameter".to_string()
            }
            TypeCheckingError::CannotReturnFunction => {
                "a function cannot return a function".to_string()
            }
            TypeCheckingError::ExpectedScalarType => {
                "expected a scalar type".to_string()
            }
            TypeCheckingError::ExpectedArithmeticType => {
                "expected an arithmetic type".to_string()
            }
            TypeCheckingError::NonIntegerSwitchExprType => {
                "switch expression type must be an integer".to_string()
            }
            TypeCheckingError::CannotAssignToArrayType => {
                "cannot assign to an array".to_string()
            }
            TypeCheckingError::CannotAssignToNonLvalue => {
                "cannot assign to a non-lvalue".to_string()
            }
            TypeCheckingError::InvalidSubscriptOperands => {
                "invalid operands to subscript expression".to_string()
            }
            TypeCheckingError::InvalidAddOperands => {
                "invalid operands to add expression".to_string()
            }
            TypeCheckingError::SubtractingDifferingPointers => {
                "cannot subtract with pointers of differing types".to_string()
            }
            TypeCheckingError::InvalidSubtractOperands => {
                "invalid operands to subtract expression".to_string()
            }
            TypeCheckingError::ExpectedIntegerType => {
                "expected integer type".to_string()
            }
            TypeCheckingError::DereferencingRvalue => {
                "cannot dereference an rvalue".to_string()
            }
            TypeCheckingError::ComparePointerNonZeroInteger => {
                "comparing a pointer with a non-zero integer is invalid"
                    .to_string()
            }
            TypeCheckingError::IncompatibleTypes => {
                "incompatible types".to_string()
            }
            TypeCheckingError::CannotCastPointerToDouble => {
                "cannot cast pointer to double".to_string()
            }
            TypeCheckingError::CannotCastDoubleToPointer => {
                "cannot cast double to pointer".to_string()
            }
            TypeCheckingError::TooManyArguments => {
                "too many arguments".to_string()
            }
            TypeCheckingError::TooFewArguments => {
                "too few arguments".to_string()
            }
            TypeCheckingError::FunctionArray => {
                "cannot have an array of functions".to_string()
            }
            TypeCheckingError::EmptyInitialiserList => {
                "cannot have an empty initialiser list".to_string()
            }
            TypeCheckingError::IncompatibleElementInArrayInitialiser => {
                "incompatible element in array initialiser".to_string()
            }
        }
    }
}

impl SemanticError {
    pub fn message(&self) -> String {
        match self {
            SemanticError::UnnamedParameterInFunctionDefinition => {
                "unnamed parameter in function definition".to_string()
            }
            SemanticError::NotAConstExpression => {
                "not a const expression".to_string()
            }
            SemanticError::NotAnLvalue => "not an lvalue".to_string(),
            SemanticError::NotAFunction => "not a function".to_string(),
            SemanticError::ContinueNotInALoop => {
                "continue not in a loop".to_string()
            }
            SemanticError::BreakNotInALoopOrSwitch => {
                "break not in a loop or switch".to_string()
            }
            SemanticError::LabelNotFound(label) => {
                format!("label {} not found", label)
            }
            SemanticError::CaseOutsideOfSwitch => {
                "case outside of a switch statement".to_string()
            }
            SemanticError::DefaultOutsideOfSwitch => {
                "default outside of a switch statement".to_string()
            }
            SemanticError::DuplicateCaseExpression => {
                "duplicate case expression".to_string()
            }
            SemanticError::DuplicateDefaultCase => {
                "duplicate default case".to_string()
            }
        }
    }
}

impl SymbolError {
    pub fn message(&self) -> String {
        match self {
            SymbolError::InvalidStorageClassForFunction(name) => {
                format!("invalid storage class for function '{}'", name)
            }
            SymbolError::MultipleDefinitions(name) => {
                format!("multiple definitions of {}", name)
            }
            SymbolError::RedeclarationWithNoLinkage(name) => {
                format!("redeclaration of {} with no linkage", name)
            }
        }
    }
}

impl ErrorClass {
    pub fn message(&self) -> String {
        match self {
            ErrorClass::Tokenising(code) => code.message(),
            ErrorClass::Parsing(code) => code.message(),
            ErrorClass::TypeChecking(code) => code.message(),
            ErrorClass::Semantic(code) => code.message(),
            ErrorClass::Symbolic(code) => code.message(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.class.message())
    }
}

const RED: &str = "\x1b[31m";
const RESET: &str = "\x1b[0m";

pub fn report_error(error: &Error, buf: &str, filename: &str) {
    report_errors(std::slice::from_ref(error), buf, filename);
}

pub fn report_errors(errors: &[Error], buf: &str, filename: &str) {
    let line_width = errors
        .iter()
        .filter_map(|e| e.loc.as_ref().map(|l| l.line_and_col(buf).0))
        .max()
        .map_or(1, |n| ((n as f64).log10().floor() as usize) + 1);

    for error in errors {
        if let Some(ref loc) = error.loc {
            let (line, col) = loc.line_and_col(buf);
            let source_line = buf.lines().nth(line - 1).unwrap_or("");
            let token_len = buf[loc.start..loc.end].chars().count().max(1);

            eprintln!(
                "{}:{}:{}: error: {}",
                filename,
                line,
                col,
                error.class.message(),
            );
            for ctx in (1..=2).rev() {
                if let Some(ctx_line) = line.checked_sub(ctx + 1)
                    && let Some(src) = buf.lines().nth(ctx_line)
                {
                    eprintln!(
                        " {:>width$} │  {}",
                        ctx_line + 1,
                        src,
                        width = line_width
                    );
                }
            }
            eprintln!(
                " {:>width$} │  {}",
                line,
                source_line,
                width = line_width
            );
            let prefix = "~".repeat(token_len - 1);
            eprintln!(
                " {:>width$} │  {}{}{}^{}",
                "",
                " ".repeat(col - 1),
                RED,
                prefix,
                RESET,
                width = line_width,
            );
        } else {
            eprintln!("{}: error: {}", filename, error.class.message());
        }
    }
}

pub fn error(class: ErrorClass) -> Error {
    Error { loc: None, class }
}

pub fn error_at(loc: SourceLoc, class: ErrorClass) -> Error {
    Error {
        loc: Some(loc),
        class,
    }
}
