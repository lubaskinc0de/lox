use std::rc::Rc;

use crate::{
    error::{ErrorStack, ParserError},
    scanner::{Literal, Token, TokenType},
};

#[derive(Debug)]
#[allow(dead_code)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Unary {
        token: Box<Expr>,
        op: Token,
    },
    Literal(Literal),
    Grouping(Box<Expr>),
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Expr, ErrorStack> {
        self.expression()
            .or_else(|err| Err(ErrorStack::new(vec![Rc::new(err)])))
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;

        while self.matches(&[TokenType::EQUAL, TokenType::BangEqual]) {
            let op = self.prev().clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while self.matches(&[
            TokenType::LESS,
            TokenType::LessEqual,
            TokenType::GREATER,
            TokenType::GreaterEqual,
        ]) {
            let op = self.prev().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while self.matches(&[TokenType::MINUS, TokenType::PLUS]) {
            let op = self.prev().clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while self.matches(&[TokenType::STAR, TokenType::SLASH]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[TokenType::BANG, TokenType::MINUS]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                token: Box::new(right),
                op,
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[TokenType::FALSE]) {
            return Ok(Expr::Literal(Literal::BOOL(false)));
        }
        if self.matches(&[TokenType::TRUE]) {
            return Ok(Expr::Literal(Literal::BOOL(true)));
        }
        if self.matches(&[TokenType::NIL]) {
            return Ok(Expr::Literal(Literal::NIL));
        }

        if self.matches(&[TokenType::NUMBER, TokenType::STRING]) {
            return Ok(Expr::Literal(self.prev().literal.clone().unwrap()));
        }

        if self.matches(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            let consumed = self.consume(TokenType::RightParen);

            if consumed.is_none() {
                return Err(ParserError {
                    message: String::from("Expect ')' after expression."),
                    token: self.peek().clone(),
                });
            }
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(ParserError {
            message: String::from("Expected expression"),
            token: self.peek().clone(),
        })
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn prev(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1
        }
        self.prev()
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        return self.peek().token_type == token_type;
    }

    fn matches(&mut self, types: &[TokenType]) -> bool {
        for &token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token_type: TokenType) -> Option<&Token> {
        if self.check(token_type) {
            return Some(self.advance());
        }
        None
    }

    #[allow(dead_code)]
    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.prev().token_type == TokenType::SEMICOLON {
                break;
            }
            match self.peek().token_type {
                TokenType::CLASS
                | TokenType::FUN
                | TokenType::FOR
                | TokenType::VAR
                | TokenType::IF
                | TokenType::PRINT
                | TokenType::WHILE => break,
                _ => {
                    self.advance();
                }
            }
        }
    }
}
