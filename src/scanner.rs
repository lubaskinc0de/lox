use crate::{
    error::InterpreterError,
    helper::VoidResult,
    token::{Literal, Token, TokenType},
};
use std::{collections::HashMap, rc::Rc, vec};

#[allow(dead_code)]
pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    err_stack: Vec<Rc<InterpreterError>>,
    keywords: HashMap<String, TokenType>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            err_stack: vec![],
            keywords: HashMap::from([
                (String::from("и"), TokenType::AND),
                (String::from("храм"), TokenType::CLASS),
                (String::from("пиздишь"), TokenType::ELSE),
                (String::from("пиздёж"), TokenType::FALSE),
                (String::from("ебашь"), TokenType::FOR),
                (String::from("молитва"), TokenType::FUN),
                (String::from("если"), TokenType::IF),
                (String::from("нихуя"), TokenType::NIL),
                (String::from("или"), TokenType::OR),
                (String::from("базарь"), TokenType::PRINT),
                (String::from("кэшбэк"), TokenType::RETURN),
                (String::from("бог"), TokenType::SUPER),
                (String::from("это"), TokenType::THIS),
                (String::from("отвечаю"), TokenType::TRUE),
                (String::from("база"), TokenType::VAR),
                (String::from("работаем"), TokenType::WHILE),
            ]),
        }
    }

    fn scan_token(&mut self) -> VoidResult {
        let c = self.advance();
        match c {
            '(' => Ok(self.add_token(TokenType::LeftParen, None)),
            ')' => Ok(self.add_token(TokenType::RightParen, None)),
            '{' => Ok(self.add_token(TokenType::LeftBrace, None)),
            '}' => Ok(self.add_token(TokenType::RightBrace, None)),
            ',' => Ok(self.add_token(TokenType::COMMA, None)),
            '.' => Ok(self.add_token(TokenType::DOT, None)),
            '-' => {
                let is_equal = self.matches('=');
                if is_equal {
                    Ok(self.add_token(TokenType::MinusEqual, None))
                } else {
                    self.current -= 1;
                    if self.match_prev('-')
                        && self.tokens.last().unwrap().token_type == TokenType::MINUS
                    {
                        self.tokens.pop().unwrap();
                        self.current += 1;
                        return Ok(());
                    }
                    self.current += 1;
                    Ok(self.add_token(TokenType::MINUS, None))
                }
            }
            '+' => {
                let is_equal = self.matches('=');
                Ok(self.add_token(
                    if is_equal {
                        TokenType::PlusEqual
                    } else {
                        TokenType::PLUS
                    },
                    None,
                ))
            }
            ';' => Ok(self.add_token(TokenType::SEMICOLON, None)),
            '*' => {
                let is_equal_star = self.matches('*');
                let is_equal_equal = self.matches('=');

                Ok(self.add_token(
                    if is_equal_star {
                        TokenType::Pow
                    } else if is_equal_equal {
                        TokenType::StarEqual
                    } else {
                        TokenType::STAR
                    },
                    None,
                ))
            }
            '!' => {
                let is_equal = self.matches('=');
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
                let is_equal = self.matches('=');
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
                let is_equal = self.matches('=');
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
                let is_equal = self.matches('=');
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
                let is_equal_slash = self.matches('/');
                let is_equal_equal = self.matches('=');

                if is_equal_equal {
                    return Ok(self.add_token(TokenType::SlashEqual, None));
                }

                if !is_equal_slash {
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
            val if val.is_alphabetic() || val == '_' => Ok(self.identifier()),
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
        self.source
            .chars()
            .nth(index)
            .expect("Scanner: char index out of range")
    }

    fn advance(&mut self) -> char {
        let res = self.char_at(self.current);
        self.current += 1;
        res
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.char_at(self.current) != expected {
            return false;
        }
        self.advance();
        return true;
    }

    fn match_prev(&mut self, expected: char) -> bool {
        if self.current == 0 {
            return false;
        }
        if self.char_at(self.current - 1) != expected {
            return false;
        }
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
        if self.current + 1 >= self.source.chars().count() {
            '\0'
        } else {
            self.char_at(self.current + 1)
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.chars().count()
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

    fn string(&mut self) -> VoidResult {
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

    fn number(&mut self) -> VoidResult {
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

            if !peek.is_alphabetic() && peek != '_' {
                break;
            }

            self.advance();
        }

        let identifier_value = self.substr(self.start, self.current);

        match self.keywords.get(&identifier_value) {
            Some(val) => self.add_token(val.clone(), Some(Literal::IDENTIFIER(identifier_value))),
            None => self.add_token(
                TokenType::IDENTIFIER,
                Some(Literal::IDENTIFIER(identifier_value)),
            ),
        }
    }
}
