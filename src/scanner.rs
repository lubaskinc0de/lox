use crate::error::{ErrorStack, SyntaxError};
use std::{
    fmt::{self, Debug},
    sync::Arc,
    vec,
};

#[derive(Debug)]
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

#[derive(Debug)]
#[allow(dead_code)]
pub enum Literal {
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64),
    BOOL(bool),
    NIL,
}

#[allow(dead_code)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<Literal>,
    line: u32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Type: {:?} | Lexeme: {} | Literal: {:?}",
            self.token_type, self.lexeme, self.literal
        )
    }
}

#[allow(dead_code)]
pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: u32,
    current: u32,
    line: u32,
    err_stack: ErrorStack,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            err_stack: ErrorStack { stack: vec![] },
        }
    }

    fn scan_token(&mut self) -> Result<(), SyntaxError> {
        let c = self.advance();
        match c {
            '(' => Ok(self.add_token(TokenType::LeftParen, None)),
            ')' => Ok(self.add_token(TokenType::RightParen, None)),
            '{' => Ok(self.add_token(TokenType::LeftBrace, None)),
            '}' => Ok(self.add_token(TokenType::RightBrace, None)),
            ',' => Ok(self.add_token(TokenType::COMMA, None)),
            '.' => Ok(self.add_token(TokenType::DOT, None)),
            '-' => Ok(self.add_token(TokenType::MINUS, None)),
            '+' => Ok(self.add_token(TokenType::PLUS, None)),
            ';' => Ok(self.add_token(TokenType::SEMICOLON, None)),
            '*' => Ok(self.add_token(TokenType::STAR, None)),
            '!' => {
                let is_equal = self.match_next('=');
                Ok(self.add_token(
                    if is_equal {
                        TokenType::BangEqual
                    } else {
                        TokenType::BANG
                    },
                    None,
                ))
            }
            '=' => {
                let is_equal = self.match_next('=');
                Ok(self.add_token(
                    if is_equal {
                        TokenType::EqualEqual
                    } else {
                        TokenType::EQUAL
                    },
                    None,
                ))
            }
            '<' => {
                let is_equal = self.match_next('=');
                Ok(self.add_token(
                    if is_equal {
                        TokenType::LessEqual
                    } else {
                        TokenType::LESS
                    },
                    None,
                ))
            }
            '>' => {
                let is_equal = self.match_next('=');
                Ok(self.add_token(
                    if is_equal {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::GREATER
                    },
                    None,
                ))
            }
            '/' => {
                let is_equal = self.match_next('/');

                if !is_equal {
                    Ok(self.add_token(TokenType::SLASH, None))
                } else {
                    Ok(loop {
                        let peek = self.peek();
                        if peek == '\n' || peek == '\0' {
                            break;
                        }
                        self.advance();
                    })
                }
            }
            ' ' | '\r' | '\t' => Ok(()),
            '\n' => {
                self.line += 1;
                Ok(())
            }
            _ => Err(SyntaxError {
                line: self.line,
                loc: self.current,
                message: format!("Unrecognized lexeme: {}", c),
            }),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, ErrorStack> {
        while !(self.is_at_end()) {
            self.start = self.current;
            self.scan_token()
                .unwrap_or_else(|err| self.err_stack.stack.push(Arc::new(err)));
        }
        if !self.err_stack.stack.is_empty() {
            return Err(self.err_stack.clone());
        }
        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: String::new(),
            literal: None,
            line: self.line,
        });
        Ok(&self.tokens)
    }

    fn char_at(&self, index: usize) -> char {
        self.source.as_bytes()[index] as char
    }

    fn advance(&mut self) -> char {
        let res = self.char_at(self.current.try_into().unwrap());
        self.current += 1;
        res
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.char_at(self.current.try_into().unwrap()) != expected {
            return false;
        }
        self.advance();
        return true;
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.char_at(self.current.try_into().unwrap())
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len().try_into().unwrap()
    }

    fn substr(&self, start: u32, end: u32) -> String {
        let collected: String = self
            .source
            .chars()
            .skip(start.try_into().unwrap())
            .take((end - start).try_into().unwrap())
            .collect();
        collected
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<Literal>) -> () {
        let lexeme = self.substr(self.start, self.current);
        match literal {
            None => self.tokens.push(Token {
                token_type: token_type,
                lexeme: lexeme,
                literal: None,
                line: self.line,
            }),
            Some(literal) => self.tokens.push(Token {
                token_type: token_type,
                lexeme: lexeme,
                literal: Some(literal),
                line: self.line,
            }),
        }
    }
}
