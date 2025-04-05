use std::{borrow::Cow, cell::Cell, collections::HashMap, rc::Rc};

use crate::{
    error::InterpreterError,
    expr::Expr,
    stmt::{FunctionDeclaration, Stmt},
    token::{Literal, Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: Cell<usize>,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum FunctionKind {
    Function,
    Method,
}

type StmtResult<'a> = Result<Stmt<'a>, InterpreterError>;
type ExprResult<'a> = Result<Expr<'a>, InterpreterError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
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
            let decl = self.statement().map_err(|err| InterpreterError::Stack {
                stack: vec![Rc::new(err)],
            })?;

            stmts.push(decl);
        }
        Ok(stmts)
    }

    fn statement(&self) -> StmtResult {
        if self.matches(&[TokenType::VAR]) {
            return self.var_stmt();
        }

        if self.matches(&[TokenType::PRINT]) {
            return self.print_stmt();
        }

        if self.matches(&[TokenType::LeftBrace]) {
            return self.block_stmt();
        }

        if self.matches(&[TokenType::IF]) {
            return self.if_stmt();
        }
        if self.matches(&[TokenType::WHILE]) {
            return self.while_stmt();
        }
        if self.matches(&[TokenType::FOR]) {
            return self.for_stmt();
        }
        if self.matches(&[TokenType::FUN]) {
            return self.function_stmt(FunctionKind::Function);
        }

        self.expression_stmt()
    }

    fn expect_semicolon(&self) -> Result<&Token, InterpreterError> {
        let token = self.peek();
        let line = token.line;

        self.consume(TokenType::SEMICOLON)
            .ok_or(InterpreterError::Parser {
                message: "Expected ';' after expression".to_string(),
                token: token.clone(),
                line,
            })
    }

    fn print_stmt(&self) -> StmtResult {
        let expr = self.expression()?;
        self.expect_semicolon()?;
        Ok(Stmt::Print(expr))
    }

    fn expression_stmt(&self) -> StmtResult {
        let expr = self.expression()?;
        self.expect_semicolon()?;
        return Ok(Stmt::Expression(expr));
    }

    fn block_stmt(&self) -> StmtResult {
        let mut code: Vec<Stmt> = vec![];

        while !self.check(TokenType::RightBrace) {
            code.push(self.statement()?);
        }

        let consumed = self.consume(TokenType::RightBrace);
        let p = self.peek();
        let l = p.line;
        if consumed.is_none() {
            return Err(InterpreterError::Parser {
                message: "Expected '}'".to_string(),
                token: p.clone(),
                line: l,
            });
        }

        Ok(Stmt::Block(code))
    }

    fn var_stmt(&self) -> StmtResult {
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

    fn if_stmt(&self) -> StmtResult {
        let l_paren = self.consume(TokenType::LeftParen);
        let mut peek = self.peek();
        let mut line = peek.line;

        if l_paren.is_none() {
            return Err(InterpreterError::Parser {
                message: "Expected '(' after if".to_string(),
                token: peek.clone(),
                line,
            });
        }
        let cond = self.expression()?;
        let r_paren = self.consume(TokenType::RightParen);
        peek = self.peek();
        line = peek.line;

        if r_paren.is_none() {
            return Err(InterpreterError::Parser {
                message: "Expected ')' after if condition".to_string(),
                token: peek.clone(),
                line,
            });
        }

        let then_branch = self.statement()?;
        let mut else_branch: Option<Box<Stmt>> = None;

        if self.matches(&[TokenType::ELSE]) {
            else_branch = Some(Box::new(self.statement()?));
        };

        Ok(Stmt::If {
            cond: cond,
            then: Box::new(then_branch),
            else_: else_branch,
        })
    }

    fn while_stmt(&self) -> StmtResult {
        let l_paren = self.consume(TokenType::LeftParen);
        let mut peek = self.peek();
        let mut line = peek.line;

        if l_paren.is_none() {
            return Err(InterpreterError::Parser {
                message: "Expected '(' after while".to_string(),
                token: peek.clone(),
                line,
            });
        }
        let cond = self.expression()?;
        let r_paren = self.consume(TokenType::RightParen);
        peek = self.peek();
        line = peek.line;

        if r_paren.is_none() {
            return Err(InterpreterError::Parser {
                message: "Expected ')' after while condition".to_string(),
                token: peek.clone(),
                line,
            });
        }

        let body = self.statement()?;

        Ok(Stmt::While {
            cond: cond,
            body: Box::new(body),
        })
    }

    fn for_stmt(&self) -> StmtResult {
        let l_paren = self.consume(TokenType::LeftParen);
        let mut peek = self.peek();
        let mut line = peek.line;

        if l_paren.is_none() {
            return Err(InterpreterError::Parser {
                message: "Expected '(' after for".to_string(),
                token: peek.clone(),
                line,
            });
        }

        let initializer: Option<Stmt>;
        if self.matches(&[TokenType::VAR]) {
            initializer = Some(self.var_stmt()?);
        } else if self.matches(&[TokenType::SEMICOLON]) {
            initializer = None;
        } else {
            initializer = Some(self.expression_stmt()?);
        }

        let mut condition: Expr = Expr::Literal(Literal::BOOL(true));
        if !self.check(TokenType::SEMICOLON) {
            condition = self.expression()?;
        }
        self.expect_semicolon()?;

        let mut increment: Option<Expr> = None;
        if !self.check(TokenType::RightParen) {
            increment = Some(self.expression()?);
        }

        // matching right paren
        peek = self.peek();
        line = peek.line;

        self.consume(TokenType::RightParen)
            .ok_or(InterpreterError::Parser {
                message: "Expected ')' after for clauses".to_string(),
                token: peek.clone(),
                line,
            })?;

        let mut body = self.statement()?;

        if increment.is_some() {
            body = Stmt::Block(vec![body, Stmt::Expression(increment.unwrap())])
        }

        body = Stmt::While {
            cond: condition,
            body: Box::new(body),
        };

        if initializer.is_some() {
            body = Stmt::Block(vec![initializer.unwrap(), body]);
        }

        Ok(body)
    }

    fn function_stmt(&self, kind: FunctionKind) -> StmtResult {
        let name = self
            .consume(TokenType::IDENTIFIER)
            .ok_or(InterpreterError::Parser {
                message: format!("Expected {kind:#?} name"),
                token: self.peek().clone(),
                line: self.peek().line,
            })?;

        self.consume(TokenType::LeftParen)
            .ok_or(InterpreterError::Parser {
                message: format!("Expected '(' after {kind:#?} name"),
                token: name.clone(),
                line: name.line,
            })?;

        let mut params: Vec<&Token> = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                let param =
                    self.consume(TokenType::IDENTIFIER)
                        .ok_or(InterpreterError::Parser {
                            message: format!("Expected parameter name"),
                            token: self.peek().clone(),
                            line: self.peek().line,
                        })?;
                params.push(param);

                if !self.matches(&[TokenType::COMMA]) {
                    break;
                }
            }
            self.consume(TokenType::RightParen)
                .ok_or(InterpreterError::Parser {
                    message: format!("Expected ')' after {kind:#?} params"),
                    token: self.peek().clone(),
                    line: self.peek().line,
                })?;
        };

        self.consume(TokenType::LeftBrace)
            .ok_or(InterpreterError::Parser {
                message: format!("Expected '{{' before {kind:#?} body"),
                token: self.peek().clone(),
                line: self.peek().line,
            })?;
        let body = self.block_stmt()?;
        let arity = params.len();
        Ok(Stmt::Function(FunctionDeclaration {
            name,
            params,
            arity,
            body: Box::new(body),
        }))
    }

    fn expression(&self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&self) -> ExprResult {
        let expr = self.or()?;

        if self.matches(&[TokenType::EQUAL]) {
            let equals = self.prev();
            let value = self.assignment()?;

            return match expr {
                Expr::VarRead(name_token) => Ok(Expr::Assign {
                    name: name_token,
                    value: Box::new(value),
                }),
                _ => Err(InterpreterError::Parser {
                    message: String::from("Invalid assignment target"),
                    token: equals.clone(),
                    line: equals.line,
                }),
            };
        } else if self.matches(&[
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::SlashEqual,
            TokenType::StarEqual,
        ]) {
            let equals = self.prev();
            let value = self.assignment()?;
            let type_map: HashMap<TokenType, TokenType> = HashMap::from([
                (TokenType::PlusEqual, TokenType::PLUS),
                (TokenType::MinusEqual, TokenType::MINUS),
                (TokenType::StarEqual, TokenType::STAR),
                (TokenType::SlashEqual, TokenType::SLASH),
                (TokenType::PowEqual, TokenType::Pow),
            ]);
            let lexeme_map: HashMap<TokenType, String> = HashMap::from([
                (TokenType::PlusEqual, "+".to_string()),
                (TokenType::MinusEqual, "-".to_string()),
                (TokenType::StarEqual, "*".to_string()),
                (TokenType::SlashEqual, "/".to_string()),
                (TokenType::PowEqual, "**".to_string()),
            ]);

            return match expr {
                Expr::VarRead(name_token) => Ok(Expr::Assign {
                    name: name_token,
                    value: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        op: Cow::Owned(Token {
                            token_type: type_map.get(&equals.token_type).unwrap().to_owned(),
                            lexeme: lexeme_map.get(&equals.token_type).unwrap().to_owned(),
                            literal: None,
                            line: name_token.line,
                        }),
                        right: Box::new(value),
                    }),
                }),
                _ => Err(InterpreterError::Parser {
                    message: String::from("Invalid assignment target"),
                    token: equals.clone(),
                    line: equals.line,
                }),
            };
        }
        Ok(expr)
    }

    fn or(&self) -> ExprResult {
        let mut expr = self.and()?;

        while self.matches(&[TokenType::OR]) {
            let op = self.prev();
            let right = self.and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                op: op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn and(&self) -> ExprResult {
        let mut expr = self.equality()?;

        while self.matches(&[TokenType::AND]) {
            let op = self.prev();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                op: op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn equality(&self) -> ExprResult {
        let mut expr = self.comparison()?;

        while self.matches(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let op = self.prev();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: Cow::Borrowed(op),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&self) -> ExprResult {
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
                op: Cow::Borrowed(op),
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn term(&self) -> ExprResult {
        let mut expr = self.factor()?;
        while self.matches(&[TokenType::MINUS, TokenType::PLUS]) {
            let op = self.prev();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: Cow::Borrowed(op),
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn factor(&self) -> ExprResult {
        let mut expr = self.unary()?;
        while self.matches(&[TokenType::STAR, TokenType::SLASH]) {
            let op = self.prev();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: Cow::Borrowed(op),
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn unary(&self) -> ExprResult {
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

    fn pow(&self) -> ExprResult {
        let mut expr = self.call()?;
        while self.matches(&[TokenType::Pow]) {
            let op = self.prev();
            let right = self.primary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: Cow::Borrowed(op),
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn call(&self) -> ExprResult {
        let mut expr = self.primary()?;

        while self.matches(&[TokenType::LeftParen]) {
            expr = self.finish_call(expr, self.prev())?;
        }
        Ok(expr)
    }

    fn finish_call<'b>(
        &'b self,
        calee: Expr<'b>,
        left_paren: &Token,
    ) -> Result<Expr<'b>, InterpreterError>
    {
        let mut args: Vec<Box<Expr>> = Vec::new();
        if !self.check(TokenType::RightParen) {
            args.push(Box::new(self.expression()?));

            while self.matches(&[TokenType::COMMA]) {
                args.push(Box::new(self.expression()?));
            }
        }
        let closing_paren = self.consume(TokenType::RightParen);
        if closing_paren.is_none() {
            return Err(InterpreterError::Parser {
                message: "Expected ')' after args list".to_string(),
                token: left_paren.clone(),
                line: left_paren.line,
            });
        }
        Ok(Expr::Call {
            calee: Box::new(calee),
            paren: closing_paren.unwrap(),
            args,
        })
    }

    fn primary(&self) -> ExprResult {
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
            return Ok(Expr::Literal((self.prev().literal).clone().unwrap()));
        }

        if self.matches(&[TokenType::IDENTIFIER]) {
            return Ok(Expr::VarRead(self.prev()));
        }

        let is_left_paren = self.matches(&[TokenType::LeftParen]);
        let peek = self.peek();
        let line = peek.line;

        if is_left_paren {
            let expr = self.expression()?;
            let consumed = self.consume(TokenType::RightParen);

            if consumed.is_none() {
                return Err(InterpreterError::Parser {
                    message: String::from("Expect ')' after expression."),
                    token: peek.clone(),
                    line: line,
                });
            }
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(InterpreterError::Parser {
            message: String::from("Expected expression"),
            token: peek.clone(),
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
