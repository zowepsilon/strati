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
    FunType {
        args: Vec<Expression>,
        return_type: Box<Expression>,
    },
    // internal, non-unparsable expressions
    BuiltinInt,
    BuiltinString,
    BuiltinType,
    #[allow(unused)] // TODO: remove this
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
        value: Expression,
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

    pub fn unit() -> ExpressionData {
        ExpressionData::Constructor {
            name: None,
            data: Vec::new(),
        }
    }
}

impl std::fmt::Display for ExpressionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExpressionData as ED;

        match self {
            ED::IntLiteral(i) => write!(f, "{i}"),
            ED::StringLiteral(s) => write!(f, "\"{s}\""),
            ED::Identifier(name) => write!(f, "{name}"),
            ED::Constructor { name, data } => {
                write!(f, ".")?;
                if let Some(name) = name {
                    write!(f, "{name}")?;
                }

                if !data.is_empty() {
                    write!(f, "(")?;

                    for field in data {
                        write!(f, "{}, ", field.data)?;
                    }

                    write!(f, ")")?;
                }

                Ok(())
            },
            ED::Fun { args, return_type, body, context } => todo!(),
            ED::Call { func, args } => todo!(),
            ED::Block { statements } => todo!(),
            ED::Meta(_) => todo!(),
            ED::FunType { args, return_type } => todo!(),
            ED::BuiltinInt => write!(f, "Int"),
            ED::BuiltinString => write!(f, "String"),
            ED::BuiltinType => write!(f, "Type"),
            ED::BuiltinFunction { name, handler: _ } => write!(f, "builtin function {name}"),
        }
    }
}
