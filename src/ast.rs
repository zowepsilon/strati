use std::collections::HashMap;
use crate::interpreter::Runtime;

#[derive(Debug)]
pub struct Program {
    pub root: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum ExpressionData {
    IntLiteral(String),
    StringLiteral(String),
    Identifier(String),
    Constructor {
        name: Option<String>,
        data: Vec<Expression>,
    },
    Fun {
        args: Vec<(String, Expression)>,
        return_type: Option<Box<Expression>>,
        body: Box<Expression>,
        context: HashMap<String, Expression>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Block {
        statements: Vec<Statement>
    },
    Meta(Box<Expression>),
    BuiltinInt,
    BuiltinString,
    BuiltinType,
    BuiltinFunction {
        name: &'static str,
        handler: fn(&mut Runtime, Vec<Expression>) -> Expression,
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub data: ExpressionData,
    pub type_: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        variable: String,
        annotation: Option<Expression>,
        value: Box<Expression>,
    },
    Expression(Expression),
}

impl ExpressionData {
    pub fn untyped(self) -> Expression {
        Expression {
            data: self,
            type_: None,
        }
    }
}
