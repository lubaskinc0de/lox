use std::rc::Rc;

use crate::{
    error::InterpreterError,
    scanner::{Literal, Token, TokenType},
    stmt::Stmt,
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
        right: Box<Expr>,
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
        if tokens.is_empty() {
            panic!("Cannot initialize parser with empty tokens")
        }
        Self {
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, InterpreterError> {
        let mut stmts: Vec<Stmt> = vec![];
        while !self.is_at_end() {
            stmts.push(
                self.statement()
                    .map_err(|err| InterpreterError::ErrorStack {
                        stack: vec![Rc::new(err)],
                    })?,
            );
        }
        Ok(stmts)
    }

    fn statement(&mut self) -> Result<Stmt, InterpreterError> {
        if self.matches(&[TokenType::PRINT]) {
            return self.print_stmt();
        }

        self.expression_stmt()
    }

    fn expect_semicolon(&mut self) -> Result<&Token, InterpreterError> {
        let token = self.peek();
        let token_clone = token.clone();
        let line = token.line;

        self.consume(TokenType::SEMICOLON)
            .ok_or(InterpreterError::ParserError {
                message: "Expected ';' after expression".to_string(),
                token: token_clone,
                line,
            })
    }

    fn print_stmt(&mut self) -> Result<Stmt, InterpreterError> {
        let expr = self.expression()?;
        self.expect_semicolon()?;
        Ok(Stmt::Print(expr))
    }

    fn expression_stmt(&mut self) -> Result<Stmt, InterpreterError> {
        let expr = self.expression()?;
        self.expect_semicolon()?;
        return Ok(Stmt::Expression(expr));
    }

    fn expression(&mut self) -> Result<Expr, InterpreterError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, InterpreterError> {
        let mut expr = self.comparison()?;

        while self.matches(&[TokenType::EqualEqual, TokenType::BangEqual]) {
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

    fn comparison(&mut self) -> Result<Expr, InterpreterError> {
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

    fn term(&mut self) -> Result<Expr, InterpreterError> {
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

    fn factor(&mut self) -> Result<Expr, InterpreterError> {
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

    fn unary(&mut self) -> Result<Expr, InterpreterError> {
        if self.matches(&[TokenType::BANG, TokenType::MINUS]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                right: Box::new(right),
                op,
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, InterpreterError> {
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

        if self.matches(&[TokenType::IDENTIFIER]) {
            return Ok(Expr::Literal(self.prev().literal.clone().unwrap()));
        }

        if self.matches(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            let consumed = self.consume(TokenType::RightParen);

            if consumed.is_none() {
                return Err(InterpreterError::ParserError {
                    message: String::from("Expect ')' after expression."),
                    token: self.peek().clone(),
                    line: self.peek().line,
                });
            }
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(InterpreterError::ParserError {
            message: String::from("Expected expression"),
            token: self.peek().clone(),
            line: self.peek().line,
        })
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current)
            .unwrap_or_else(|| self.tokens.get(0).unwrap())
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
