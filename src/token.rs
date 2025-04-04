use std::{
    cell::RefCell,
    fmt::{self},
    rc::Rc,
};

use crate::error::InterpreterError;

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
#[allow(dead_code)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BangEqual,
    EQUAL,
    EqualEqual,
    GREATER,
    GreaterEqual,
    LESS,
    LessEqual,
    Pow,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PowEqual,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Literal {
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64),
    BOOL(bool),
    NIL,
}

impl Literal {
    pub fn is_truthy(&self) -> bool {
        match &self {
            Literal::BOOL(val) => match val {
                true => true,
                false => false,
            },
            Literal::NIL => false,
            _ => true,
        }
    }
    pub fn extract_number(&self) -> Option<&f64> {
        match &self {
            Literal::NUMBER(val) => Some(val),
            _ => None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.literal.is_some() {
            write!(
                f,
                "[Token {:?}] Lexeme: '{}' -- {:?}",
                self.token_type, self.lexeme, &self.literal,
            )
        } else {
            write!(f, "[Token {:?}] Lexeme: '{}'", self.token_type, self.lexeme)
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Literal::IDENTIFIER(val) => write!(f, "<identifier> {}", val),
            Literal::STRING(val) => write!(f, "{}", val),
            Literal::NUMBER(val) => write!(f, "{}", val),
            Literal::BOOL(val) => write!(f, "{}", val),
            Literal::NIL => write!(f, "null"),
        }
    }
}

impl Token {
    pub fn expect_identifier(&self) -> Result<String, InterpreterError> {
        let line = self.line;

        match &self.literal {
            Some(literal) => match literal {
                Literal::IDENTIFIER(v) => Ok(v.to_string()),
                _ => Err(InterpreterError::Parser {
                    message: "Expected identifier".to_string(),
                    token: self.clone(),
                    line,
                }),
            },
            None => Err(InterpreterError::Parser {
                message: "Expected literal".to_string(),
                token: self.clone(),
                line,
            }),
        }
    }
}

pub type RcMutLiteral = Rc<RefCell<Literal>>;

