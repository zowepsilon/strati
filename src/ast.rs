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
    Const(Box<Expression>),
    Quote(Vec<Statement>),
    FunType {
        args: Vec<Expression>,
        return_type: Option<Box<Expression>>,
    },
    // internal, unparsable expressions
    BuiltinInt,
    BuiltinString,
    BuiltinType,
    BuiltinQuote,
    BuiltinFunction {
        name: &'static str,
        handler: fn(&mut Runtime, Vec<Expression>) -> Expression,
        runtime_available: bool,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub data: ExpressionData,
    pub type_: Option<Box<Expression>>,
}

#[derive(Debug, Clone, Copy)]
pub enum BindingKind {
    Let,
    Const,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Binding {
        kind: BindingKind,
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

        let indent = f.width().unwrap_or(0);
        let offset = 4;

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
                        write!(f, "{:indent$}, ", field.data)?;
                    }

                    write!(f, ")")?;
                }

                Ok(())
            },
            ED::Fun { args, return_type, body, context } => {
                write!(f, "fun (")?;
                for (name, type_) in args {
                    write!(f, "{}: {:indent$}, ", name, type_.data)?;
                }
                write!(f, ")")?;

                if !context.is_empty() {
                    write!(f, " [")?;
                    for (name, value) in context {
                        write!(f, "{name}: {:indent$}", value.data)?;
                    }
                    write!(f, "]")?;
                }

                if let Some(ret) = return_type {
                    write!(f, " -> {:indent$}", ret.data)?;
                }

                write!(f, " {:indent$}", body.data)?;

                Ok(())
            },
            ED::Call { func, args } => {
                write!(f, "{:indent$}(", func.data)?;

                for arg in args {
                    write!(f, "{:indent$}, ", arg.data)?;
                }

                write!(f, ")")?;

                Ok(())
            },
            ED::Block { statements } => {
                if statements.is_empty() {
                    write!(f, "quote {{}}")
                } else {
                    write!(f, "quote {{")?;
                    for stmt in statements {
                        write!(f, "\n{: >indent$}{stmt:indent$}", "", indent = indent+offset)?;
                    }
                    write!(f, "\n{: >indent$}}}", "")?;

                    Ok(())
                }
            },
            ED::Const(inner) => write!(f, "const {:indent$}", inner.data),
            ED::Quote(inner) => {
                if inner.is_empty() {
                    write!(f, "{{}}")
                } else {
                    write!(f, "{{")?;
                    for stmt in inner {
                        write!(f, "\n{: >indent$}{stmt:indent$}", "", indent = indent+offset)?;
                    }
                    write!(f, "\n{: >indent$}}}", "")?;

                    Ok(())
                }
            }
            ED::FunType { args, return_type } => {
                write!(f, "fn(")?;
                for arg in args {
                    write!(f, "{:indent$}", arg.data)?;
                }
                write!(f, ")")?;

                if let Some(ret) = return_type {
                    write!(f, " -> {:indent$}", ret.data)?;
                }

                Ok(())
            },
            ED::BuiltinInt => write!(f, "$Int"),
            ED::BuiltinString => write!(f, "$String"),
            ED::BuiltinType => write!(f, "$Type"),
            ED::BuiltinQuote => write!(f, "$Quote"),
            ED::BuiltinFunction { name, .. } => write!(f, "builtin function {name}"),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indent = f.width().unwrap_or(0);

        match self {
            Statement::Binding { kind, variable, annotation, value } => {

                match kind {
                    BindingKind::Let => write!(f, "let")?,
                    BindingKind::Const => write!(f, "const")?,
                }

                write!(f, " {variable}")?;
                
                if let Some(type_) = annotation {
                    write!(f, ": {:indent$}", type_.data)?;
                }

                write!(f, " = {:indent$}", value.data)?;

                Ok(())
            },
            Statement::Expression(expr) => write!(f, "{:indent$}", expr.data),
        }
    }
}
