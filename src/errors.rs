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

use crate::lexing::Token;
use std::fmt;

#[derive(Debug, Clone)]
#[allow(unused)]
pub struct Error {
    pub token: Option<Token>,
    pub class: ErrorClass,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum LexingError {
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
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum ParsingError {
    ExpectedButGot { expected: String, got: String },
    ExpectedButReachedEof(String),
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
    Lexing(LexingError),
    Parsing(ParsingError),
    TypeChecking(TypeCheckingError),
    Semantic(SemanticError),
    Symbolic(SymbolError),
}

impl LexingError {
    pub fn message(&self) -> String {
        match self {
            LexingError::InvalidDoubleLiteral => {
                "invalid double literal".to_string()
            }
            LexingError::ExpectedOctalDigits => {
                "expected octal digits".to_string()
            }
            LexingError::ExpectedHexadecimalDigits => {
                "expected hexadecimal digits".to_string()
            }
            LexingError::ExpectedDigits => "expected digits".to_string(),
            LexingError::NotValidHexEscapeSequence => {
                "not a valid hex escape sequence".to_string()
            }
            LexingError::NotValidUnicodeEscapeSequence => {
                "not a valid unicode escape sequence".to_string()
            }
            LexingError::NotValidOctalEscapeSequence => {
                "not a valid octal escape sequence".to_string()
            }
            LexingError::LeadingZeroInIntegerConstant => {
                "leading zero in integer constant".to_string()
            }
            LexingError::InvalidIntegerLiteral => {
                "invalid integer literal".to_string()
            }
            LexingError::InvalidIdentifier(ident) => {
                format!("invalid identifier: {}", ident)
            }
            LexingError::UnterminatedBlockComment => {
                "unterminated block comment".to_string()
            }
        }
    }
}

impl ParsingError {
    pub fn message(&self) -> String {
        match self {
            ParsingError::ExpectedButGot { expected, got } => {
                format!("expected {:?} but got {:?}", expected, got)
            }
            ParsingError::ExpectedButReachedEof(expected) => {
                format!("expected {:?} but reached end of file", expected)
            }
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
            ErrorClass::Lexing(code) => code.message(),
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

/// Create an error from an error code, without a source token.
pub fn error(class: ErrorClass) -> Error {
    Error { token: None, class }
}

/// Create an error from an error code with a source token.
pub fn error_at(token: Token, class: ErrorClass) -> Error {
    Error {
        token: Some(token),
        class,
    }
}
