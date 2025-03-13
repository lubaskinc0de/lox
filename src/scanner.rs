use crate::error::InterpreterError;
use std::{
    collections::HashMap,
    fmt::{self, Debug},
    rc::Rc,
    vec,
};

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
                self.token_type,
                self.lexeme,
                self.literal.clone().unwrap()
            )
        } else {
            write!(f, "[Token {:?}] Lexeme: '{}'", self.token_type, self.lexeme)
        }
    }
}

impl Token {
    pub fn literal(&self) -> Result<Literal, InterpreterError> {
        let line = self.line;
        match &self.literal {
            Some(literal) => Ok(literal.clone()),
            None => Err(InterpreterError::Parser {
                message: "Expected literal".to_string(),
                token: self.clone(),
                line,
            }),
        }
    }

    pub fn expect_identifier(&self) -> Result<String, InterpreterError> {
        let line = self.line;
        match &self.literal()? {
            Literal::IDENTIFIER(v) => Ok(v.clone()),
            _ => Err(InterpreterError::Parser {
                message: "Expected identifier".to_string(),
                token: self.clone(),
                line: line,
            }),
        }
    }
}

#[allow(dead_code)]
pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    err_stack: Vec<Rc<InterpreterError>>,
    keywords: HashMap<String, TokenType>,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            err_stack: vec![],
            keywords: HashMap::from([
                (String::from("and"), TokenType::AND),
                (String::from("class"), TokenType::CLASS),
                (String::from("else"), TokenType::ELSE),
                (String::from("false"), TokenType::FALSE),
                (String::from("for"), TokenType::FOR),
                (String::from("fun"), TokenType::FUN),
                (String::from("if"), TokenType::IF),
                (String::from("nil"), TokenType::NIL),
                (String::from("or"), TokenType::OR),
                (String::from("print"), TokenType::PRINT),
                (String::from("return"), TokenType::RETURN),
                (String::from("super"), TokenType::SUPER),
                (String::from("this"), TokenType::THIS),
                (String::from("true"), TokenType::TRUE),
                (String::from("var"), TokenType::VAR),
                (String::from("while"), TokenType::WHILE),
            ]),
        }
    }

    fn scan_token(&mut self) -> Result<(), InterpreterError> {
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
            '*' => {
                let is_equal = self.match_next('*');
                Ok(self.add_token(
                    if is_equal {
                        TokenType::Pow
                    } else {
                        TokenType::STAR
                    },
                    None,
                ))
            }
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
            '"' => self.string(),
            val if self.is_digit(val) => self.number(),
            val if val.is_alphabetic() => Ok(self.identifier()),
            _ => Err(InterpreterError::Syntax {
                line: self.line,
                loc: self.get_loc(),
                message: format!("Unrecognized lexeme: {}", c),
            }),
        }
    }

    fn get_loc(&mut self) -> String {
        let initial_current = self.current;
        let mut loc = String::new();
        loop {
            self.current -= 1;

            let peek = self.peek();
            if peek == '\n' || peek == '\0' || self.current == 0 {
                if self.current == 0 {
                    loc.push_str(&peek.to_string());
                }
                self.current = initial_current;
                let mut formatted = loc.chars().rev().collect::<String>();
                formatted.push_str(" <- This symbol");
                break formatted;
            }
            loc.push_str(&peek.to_string());
        }
    }

    fn is_digit(&self, c: char) -> bool {
        return c.is_digit(10);
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, InterpreterError> {
        while !(self.is_at_end()) {
            self.start = self.current;
            self.scan_token()
                .unwrap_or_else(|err| self.err_stack.push(Rc::new(err)));
        }
        if !self.err_stack.is_empty() {
            return Err(InterpreterError::Stack {
                stack: self.err_stack.clone(),
            });
        }
        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: String::new(),
            literal: None,
            line: self.line,
        });
        Ok(self.tokens.clone())
    }

    fn char_at(&self, index: usize) -> char {
        self.source.as_bytes()[index] as char
    }

    fn advance(&mut self) -> char {
        let res = self.char_at(self.current);
        self.current += 1;
        res
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.char_at(self.current) != expected {
            return false;
        }
        self.advance();
        return true;
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.char_at(self.current)
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.char_at(self.current + 1)
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn substr(&self, start: usize, end: usize) -> String {
        let collected: String = self.source.chars().skip(start).take(end - start).collect();
        collected
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<Literal>) -> () {
        let lexeme = self.substr(self.start, self.current);
        match literal {
            None => self.tokens.push(Token {
                token_type,
                lexeme,
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

    fn string(&mut self) -> Result<(), InterpreterError> {
        loop {
            let peek = self.peek();

            if peek == '"' || peek == '\0' {
                break;
            }

            if peek == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(InterpreterError::Syntax {
                line: self.line,
                loc: self.get_loc(),
                message: String::from("Unclosed string literal"),
            });
        }
        self.advance();
        let string_value = self.substr(self.start + 1, self.current - 1);
        self.add_token(TokenType::STRING, Some(Literal::STRING(string_value)));
        Ok(())
    }

    fn number(&mut self) -> Result<(), InterpreterError> {
        loop {
            let peek = self.peek();

            if !self.is_digit(peek) {
                break;
            }

            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();
            loop {
                let peek = self.peek();

                if !self.is_digit(peek) {
                    break;
                }

                self.advance();
            }
        }
        let num_string = self.substr(self.start, self.current);
        let num_value = num_string.parse::<f64>();

        if num_value.is_err() {
            return Err(InterpreterError::Syntax {
                line: self.line,
                loc: self.get_loc(),
                message: String::from("Cannot parse number"),
            });
        }

        let literal = Literal::NUMBER(num_value.unwrap());
        self.add_token(TokenType::NUMBER, Some(literal));
        Ok(())
    }

    fn identifier(&mut self) {
        loop {
            let peek = self.peek();

            if !peek.is_alphabetic() {
                break;
            }

            self.advance();
        }

        let identifier_value = self.substr(self.start, self.current);
        let identifier_literal = Literal::IDENTIFIER(identifier_value.clone());

        match self.keywords.get(&identifier_value) {
            Some(val) => self.add_token(val.clone(), Some(identifier_literal)),
            None => self.add_token(TokenType::IDENTIFIER, Some(identifier_literal)),
        }
    }
}
