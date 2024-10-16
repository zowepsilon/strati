use crate::interpreter::Runtime;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub root: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ident {
    Plain(String),
    Splice(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionData {
    IntLiteral(String),
    StringLiteral(String),
    Identifier(String),
    Splice(String),
    Constructor {
        name: Option<Ident>,
        data: Vec<Expression>,
    },
    Fun {
        args: Vec<(Ident, Expression)>,
        return_type: Option<Box<Expression>>,
        body: Box<Expression>,
        // context variable names cannot be splices
        // because when the context is built (at evaluation)
        // all syntactic sugar should have disappeared
        context: HashMap<String, Expression>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Block {
        statements: Vec<Statement>,
        flatten: bool,
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
    Thunk(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub data: ExpressionData,
    pub type_: Option<Box<Expression>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BindingKind {
    Let,
    Const,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Binding {
        kind: BindingKind,
        variable: Ident,
        annotation: Option<Expression>,
        value: Expression,
    },
    Expression(Expression),
}

impl Ident {
    pub fn plain_ref(&self) -> &String {
        match self {
            Ident::Plain(name) => name,
            Ident::Splice(_) => panic!("expected concrete identifier, found splice"),
        }
    }

    pub fn plain(self) -> String {
        match self {
            Ident::Plain(name) => name,
            Ident::Splice(_) => panic!("expected concrete identifier, found splice"),
        }
    }
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

impl Expression {
    pub fn unit_typed() -> Expression {
        Expression {
            data: ExpressionData::unit(),
            type_: Some(Box::new(ExpressionData::unit().untyped())),
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
            ED::Splice(name) => write!(f, "${name}"),
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
            }
            ED::Fun {
                args,
                return_type,
                body,
                context,
            } => {
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
            }
            ED::Call { func, args } => {
                write!(f, "{:indent$}(", func.data)?;

                for arg in args {
                    write!(f, "{:indent$}, ", arg.data)?;
                }

                write!(f, ")")?;

                Ok(())
            }
            ED::Block { statements, flatten: _ } => {
                if statements.is_empty() {
                    write!(f, "{{}}")
                } else {
                    write!(f, "{{")?;
                    for stmt in statements {
                        write!(
                            f,
                            "\n{: >indent$}{stmt:indent$}",
                            "",
                            indent = indent + offset
                        )?;
                    }
                    write!(f, "\n{: >indent$}}}", "")?;

                    Ok(())
                }
            }
            ED::Const(inner) => write!(f, "const {:indent$}", inner.data),
            ED::Quote(inner) => {
                if inner.is_empty() {
                    write!(f, "quote {{}}")
                } else {
                    write!(f, "quote {{")?;
                    for stmt in inner {
                        write!(
                            f,
                            "\n{: >indent$}{stmt:indent$}",
                            "",
                            indent = indent + offset
                        )?;
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
            }
            ED::Thunk(id) => write!(f, "$thunk({id})"),
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
            Statement::Binding {
                kind,
                variable,
                annotation,
                value,
            } => {
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
            }
            Statement::Expression(expr) => write!(f, "{:indent$}", expr.data),
        }
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ident::Plain(name) => write!(f, "{name}"),
            Ident::Splice(name) => write!(f, "${name}"),
        }
    }
}
