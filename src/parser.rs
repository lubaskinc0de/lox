use std::{cell::Cell, rc::Rc};

use crate::{
    error::InterpreterError,
    scanner::{Literal, Token, TokenType},
    stmt::Stmt,
};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr<'a> {
    Binary {
        left: Box<Expr<'a>>,
        op: &'a Token,
        right: Box<Expr<'a>>,
    },
    Unary {
        right: Box<Expr<'a>>,
        op: &'a Token,
    },
    Literal(&'a Literal),
    Grouping(Box<Expr<'a>>),
    VarRead(&'a Token),
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: Cell<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        if tokens.is_empty() {
            panic!("Cannot initialize parser with empty tokens")
        }
        Self {
            tokens: tokens,
            current: Cell::new(0),
        }
    }

    pub fn parse(&self) -> Result<Vec<Stmt>, InterpreterError> {
        let mut stmts: Vec<Stmt> = vec![];
        loop {
            if self.is_at_end() {
                break;
            }
            let decl = self.declaration().map_err(|err| InterpreterError::Stack {
                stack: vec![Rc::new(err)],
            })?;

            stmts.push(decl);
        }
        Ok(stmts)
    }

    fn declaration(&self) -> Result<Stmt, InterpreterError> {
        if self.matches(&[TokenType::VAR]) {
            return self.var_declaration();
        }

        self.statement()
    }

    fn var_declaration(&self) -> Result<Stmt, InterpreterError> {
        let token = self.peek();
        let line = token.line;

        let consumed = self.consume(TokenType::IDENTIFIER);
        let variable_token = consumed.ok_or(InterpreterError::Parser {
            message: "Expected name after variable declaration".to_string(),
            token: token.clone(),
            line,
        })?;

        variable_token.expect_identifier()?;

        let mut variable_expr: Option<Expr> = None;
        let matches_eq = self.matches(&[TokenType::EQUAL]);
        if matches_eq {
            variable_expr = Some(self.expression()?);
        }

        self.expect_semicolon()?;
        return Ok(Stmt::VarDeclaration {
            expr: variable_expr,
            name: variable_token,
        });
    }

    fn statement(&self) -> Result<Stmt, InterpreterError> {
        if self.matches(&[TokenType::PRINT]) {
            return self.print_stmt();
        }

        self.expression_stmt()
    }

    fn expect_semicolon(&self) -> Result<&Token, InterpreterError> {
        let token = self.peek();
        let token_clone = token.clone();
        let line = token.line;

        self.consume(TokenType::SEMICOLON)
            .ok_or(InterpreterError::Parser {
                message: "Expected ';' after expression".to_string(),
                token: token_clone,
                line,
            })
    }

    fn print_stmt(&self) -> Result<Stmt, InterpreterError> {
        let expr = self.expression()?;
        self.expect_semicolon()?;
        Ok(Stmt::Print(expr))
    }

    fn expression_stmt(&self) -> Result<Stmt, InterpreterError> {
        let expr = self.expression()?;
        self.expect_semicolon()?;
        return Ok(Stmt::Expression(expr));
    }

    fn expression(&self) -> Result<Expr, InterpreterError> {
        self.equality()
    }

    fn equality(&self) -> Result<Expr, InterpreterError> {
        let mut expr = self.comparison()?;

        while self.matches(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let op = self.prev();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&self) -> Result<Expr, InterpreterError> {
        let mut expr = self.term()?;
        while self.matches(&[
            TokenType::LESS,
            TokenType::LessEqual,
            TokenType::GREATER,
            TokenType::GreaterEqual,
        ]) {
            let op = self.prev();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn term(&self) -> Result<Expr, InterpreterError> {
        let mut expr = self.factor()?;
        while self.matches(&[TokenType::MINUS, TokenType::PLUS]) {
            let op = self.prev();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn factor(&self) -> Result<Expr, InterpreterError> {
        let mut expr = self.unary()?;
        while self.matches(&[TokenType::STAR, TokenType::SLASH]) {
            let op = self.prev();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn unary(&self) -> Result<Expr, InterpreterError> {
        if self.matches(&[TokenType::BANG, TokenType::MINUS]) {
            let op = self.prev();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                right: Box::new(right),
                op,
            });
        }
        self.pow()
    }

    fn pow(&self) -> Result<Expr, InterpreterError> {
        let mut expr = self.primary()?;
        while self.matches(&[TokenType::Pow]) {
            let op = self.prev();
            let right = self.primary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn primary(&self) -> Result<Expr, InterpreterError> {
        if self.matches(&[TokenType::FALSE]) {
            return Ok(Expr::Literal(&Literal::BOOL(false)));
        }
        if self.matches(&[TokenType::TRUE]) {
            return Ok(Expr::Literal(&Literal::BOOL(true)));
        }
        if self.matches(&[TokenType::NIL]) {
            return Ok(Expr::Literal(&Literal::NIL));
        }

        if self.matches(&[TokenType::NUMBER, TokenType::STRING]) {
            return Ok(Expr::Literal((self.prev().literal.as_ref()).unwrap()));
        }

        if self.matches(&[TokenType::IDENTIFIER]) {
            return Ok(Expr::VarRead(self.prev()));
        }

        let is_left_paren = self.matches(&[TokenType::LeftParen]);
        let peek = self.peek();
        let peek_clone = peek.clone();
        let line = peek_clone.line;

        if is_left_paren {
            let expr = self.expression()?;
            let consumed = self.consume(TokenType::RightParen);

            if consumed.is_none() {
                return Err(InterpreterError::Parser {
                    message: String::from("Expect ')' after expression."),
                    token: peek_clone,
                    line: line,
                });
            }
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(InterpreterError::Parser {
            message: String::from("Expected expression"),
            token: peek_clone,
            line: line,
        })
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current.get())
            .unwrap_or_else(|| self.tokens.get(0).unwrap())
    }

    fn prev(&self) -> &Token {
        self.tokens.get(self.current.get() - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn advance(&self) -> &Token {
        if !self.is_at_end() {
            self.current.replace(self.current.get() + 1);
        }
        self.prev()
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        return self.peek().token_type == token_type;
    }

    fn matches(&self, types: &[TokenType]) -> bool {
        for &token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&self, token_type: TokenType) -> Option<&Token> {
        if self.check(token_type) {
            return Some(self.advance());
        }
        None
    }
}
