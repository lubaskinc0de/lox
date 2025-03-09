use crate::{
    parser::Expr,
    scanner::{Literal, TokenType},
};

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, expr: Expr) -> Literal {
        match expr {
            Expr::Literal(val) => val.clone(),
            Expr::Grouping(expr) => self.interpret(*expr),
            Expr::Unary { right, op } => {
                let evaluated = self.interpret(*right);
                self.evaluate_unary(&op.token_type, evaluated)
            }
            Expr::Binary { left, op, right } => {
                let right_eval = self.interpret(*right);
                let left_eval = self.interpret(*left);

                match op.token_type {
                    TokenType::MINUS | TokenType::PLUS | TokenType::STAR | TokenType::SLASH => {
                        if let (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) =
                            (&left_eval, &right_eval)
                        {
                            self.evaluate_arithmetic(&op.token_type, *left_val, *right_val)
                        } else {
                            panic!("Cannot perform arithmetic on non-numbers");
                        }
                    }
                    TokenType::LESS
                    | TokenType::GREATER
                    | TokenType::LessEqual
                    | TokenType::GreaterEqual => {
                        if let (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) =
                            (&left_eval, &right_eval)
                        {
                            self.evaluate_comparison(&op.token_type, *left_val, *right_val)
                        } else {
                            panic!("Cannot perform comparison on non-numbers");
                        }
                    }
                    TokenType::BangEqual | TokenType::EqualEqual => {
                        self.evaluate_equality(&op.token_type, &left_eval, &right_eval)
                    }
                    _ => panic!("Unsupported operator"),
                }
            }
        }
    }

    fn evaluate_unary(&self, op: &TokenType, right: Literal) -> Literal {
        match op {
            TokenType::MINUS => match right {
                Literal::NUMBER(number) => Literal::NUMBER(-number),
                _ => panic!("Cannot apply minus to non-number"),
            },
            TokenType::BANG => Literal::BOOL(!right.is_truthy()),
            _ => panic!("Unhandled operator"),
        }
    }

    fn evaluate_arithmetic(&self, op: &TokenType, left: f64, right: f64) -> Literal {
        match op {
            TokenType::MINUS => Literal::NUMBER(left - right),
            TokenType::PLUS => Literal::NUMBER(left + right),
            TokenType::STAR => Literal::NUMBER(left * right),
            TokenType::SLASH => Literal::NUMBER(left / right),
            _ => panic!("Unsupported arithmetic operator"),
        }
    }

    fn evaluate_comparison(&self, op: &TokenType, left: f64, right: f64) -> Literal {
        match op {
            TokenType::LESS => Literal::BOOL(left < right),
            TokenType::GREATER => Literal::BOOL(left > right),
            TokenType::LessEqual => Literal::BOOL(left <= right),
            TokenType::GreaterEqual => Literal::BOOL(left >= right),
            _ => panic!("Unsupported comparison operator"),
        }
    }

    fn evaluate_equality(&self, op: &TokenType, left: &Literal, right: &Literal) -> Literal {
        match (left, right) {
            (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) => match op {
                TokenType::BangEqual => Literal::BOOL(left_val != right_val),
                TokenType::EqualEqual => Literal::BOOL(left_val == right_val),
                _ => panic!("Unsupported equality operator"),
            },
            (Literal::STRING(left_val), Literal::STRING(right_val)) => match op {
                TokenType::BangEqual => Literal::BOOL(left_val != right_val),
                TokenType::EqualEqual => Literal::BOOL(left_val == right_val),
                _ => panic!("Unsupported equality operator"),
            },
            (Literal::BOOL(left_val), Literal::BOOL(right_val)) => match op {
                TokenType::BangEqual => Literal::BOOL(left_val != right_val),
                TokenType::EqualEqual => Literal::BOOL(left_val == right_val),
                _ => panic!("Unsupported equality operator"),
            },
            _ => Literal::BOOL(false),
        }
    }
}
