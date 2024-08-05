use std::vec;

use crate::lexer::{Token, TokenData};
use crate::ast::{
    Program,
    Type,
    Expression, 
    ExpressionKind, 
    Statement, 
    LetDeclaration, 
};


#[derive(Debug, Clone)]
pub struct Parser {
    tokens: multipeek::MultiPeek<vec::IntoIter<Token>>,
}

macro_rules! expect {
    ($self:ident, $tok:pat) => {
        match $self.tokens.next()?.data {
            $tok => Some(()),
            other => { 
                eprintln!("{}:{}:{}: expected {}, got {other:?}", file!(), line!(), column!(), stringify!($tok));
                None
            }
        }
    };
}

macro_rules! list {
    ($self:ident, $to_parse:expr, sep: $sep:pat) => {'list: {
        $self.newlines();

        let first = match $self.tokens.peek() {
            None => break 'list Vec::new(),
            Some(Token { data, .. }) => match data {
                $sep => {
                    let _ = $self.tokens.next();
                    $self.newlines();
                    break 'list Vec::new();

                },
                _ => $to_parse,
            }
        };

        let mut args = vec![first];
        $self.newlines();

        loop {
            match $self.tokens.peek() {
                None => break,
                Some(Token { data, .. }) => match data {
                    $sep => $self.newlines(),
                    _ => return None
                }
            };

            $self.newlines();
            args.push($to_parse);
            $self.newlines();
        }
        args
    }};
}

const TRACE: bool = false;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens: multipeek::multipeek(tokens) }
    }

    pub fn parse(mut self) -> Option<Program> {
        if TRACE { dbg!("parse", self.tokens.peek()); }
        let mut declarations = vec![];
        
        self.newlines();

        while let Some(_) = self.tokens.peek() {
            let stmt = match self.let_statement() {
                Some(stmt) => stmt,
                None => {
                    // dbg!(self.tokens.next());
                    // dbg!(declarations);

                    return None;
                }
            };
            declarations.push(stmt);
            self.newlines();
        }
        
        Some(Program::new(declarations))
    }
}

// doc: see syntax.ebnf
impl Parser {
    fn statement(&mut self) -> Option<Statement> {
        if TRACE { dbg!("statement", self.tokens.peek()); }
        
        match self.tokens.peek()?.data.clone() {
            TokenData::Let => Some(Statement::Let(self.let_statement()?)),
            TokenData::Identifier(_) 
                if self.tokens.peek_nth(1)?.data == TokenData::Assign => self.assign_statement(),
            _ => Some(Statement::Expression(self.expression()?))
        }
    }

    fn expression(&mut self) -> Option<Expression> {
        if TRACE { dbg!("expression", self.tokens.peek().unwrap()); }
        match self.tokens.peek()?.data {
            TokenData::Fun => {self.fun_expression()},
            _ => {
                let value = self.expression_value()?;
                match self.tokens.peek()?.data {
                    TokenData::Colon => {
                        let anno = self.annotation()?;
                        Some(Expression { kind: value.kind, ty: anno })
                    },
                    _ => Some(value)
                }
            }
        }
    }

    fn annotation(&mut self) -> Option<Type> {
        if TRACE { dbg!("annotation", self.tokens.peek().unwrap()); }
        expect!(self, TokenData::Colon)?;

        self.ty()
    }

    fn expression_value(&mut self) -> Option<Expression> {
        if TRACE { dbg!("expression_value", self.tokens.peek().unwrap()); }
        let mut maybe_func = match self.tokens.next()?.data {
            TokenData::Integer(value) => Some(ExpressionKind::IntLiteral(value).into()),
            TokenData::String(value) => Some(ExpressionKind::StringLiteral(value).into()),
            TokenData::Identifier(value) => Some(ExpressionKind::Identifier(value).into()),
            TokenData::ParenBlock(inner) => {
                if inner.is_empty() {
                    Some(ExpressionKind::Unit.into())
                } else {
                    let mut inner = Parser::new(inner);
                    inner.newlines();
                    let expr = inner.expression()?;
                    Some(expr)
                }
            },
            TokenData::BraceBlock(inner) => 'brace: {
                if inner.is_empty() {
                    break 'brace Some(ExpressionKind::Block {
                        statements: vec![],
                    }.into());
                }

                let mut inner = Parser::new(inner);

                inner.newlines();

                let mut statements = vec![];

                while let Some(_) = inner.tokens.peek() {
                    statements.push(inner.statement()?);

                    match inner.tokens.peek()?.data {
                        TokenData::NewLine => {
                            let _ = inner.tokens.next();
                        },
                        _ => expect!(inner, TokenData::NewLine)?,
                    }
                    inner.newlines();
                }

                Some(ExpressionKind::Block { statements }.into())
            },
            _ => None,
        }?;

        while let Some(Token {data: TokenData::ParenBlock(_), ..}) = self.tokens.peek() {
            let Some(Token {data: TokenData::ParenBlock(inner), ..}) = self.tokens.next()
                else { unreachable!("self.tokens was peeked successfully") };

            let mut inner = Parser::new(inner);
           
            let args = list!(inner,
                inner.expression()?, 
                sep: TokenData::Comma
            );

            let func = Box::new(maybe_func);

            maybe_func = ExpressionKind::Call {
                func,
                args
            }.into();

        }

        Some(maybe_func)
    }

    fn ty(&mut self) -> Option<Type> {
        if TRACE { dbg!("ty", self.tokens.peek().unwrap()); }
        match self.tokens.next()?.data {
            TokenData::Identifier(x) if x == "int"    => Some(Type::Int),
            TokenData::Identifier(x) if x == "string" => Some(Type::String),
            TokenData::ParenBlock(inner) => {
                if !inner.is_empty() { return None; }
                Some(Type::Unit)
            }
            TokenData::Struct => {
                let Some(Token { data: TokenData::BraceBlock(inner), .. }) = self.tokens.next()
                    else { return None };

                let mut inner = Parser::new(inner);

                let fields = list!(inner, {
                    let Token { data: TokenData::Identifier(name), .. } = inner.tokens.next()?
                        else { return None };

                    let ty = inner.annotation()?;

                    (name, ty)
                }, sep: TokenData::Comma);

                Some(Type::Struct {
                    fields,
                })
            },
            _ => None
        }
    }

    fn let_statement(&mut self) -> Option<LetDeclaration> {
        if TRACE { dbg!("let_statement", self.tokens.peek().unwrap()); }
        expect!(self, TokenData::Let)?;

        let Some(Token { data: TokenData::Identifier(variable), .. }) = self.tokens.next()
            else { return None };



        let annotation = match self.tokens.peek()?.data {
            TokenData::Colon => Some(self.annotation()?),
            _ => None
        };

        expect!(self, TokenData::Assign)?;

        let value = self.expression()?;
        let value = Box::new(value);

        Some(LetDeclaration {
            variable,
            annotation,
            value
        })
    }

    fn assign_statement(&mut self) -> Option<Statement> {
        let TokenData::Identifier(name) = self.tokens.next()?.data
            else { return None };

        expect!(self, TokenData::Assign)?;

        let value = self.expression()?;

        Some(Statement::Assign {
            name,
            value
        })
    }

    fn fun_expression(&mut self) -> Option<Expression> {
        if TRACE { dbg!("fun_expression", self.tokens.peek()); }
        expect!(self, TokenData::Fun)?;

        let Some(Token { data: TokenData::ParenBlock(inner), .. }) = self.tokens.next()
            else { return None };

        let mut inner = Parser::new(inner);

        let args = list!(inner, {
            let Token { data: TokenData::Identifier(name), .. } = inner.tokens.next()?
                else { return None };

            let ty = inner.annotation()?;

            (name, ty)
        }, sep: TokenData::Comma);

        expect!(self, TokenData::Colon)?;

        let return_type = self.ty()?;

        expect!(self, TokenData::ThinArrow)?;

        let body = self.expression()?;
        let body = Box::new(body);
        
        Some(ExpressionKind::Fun {
            args,
            return_type,
            body,
        }.into())
    }

    fn newlines(&mut self) {
        if TRACE {
            dbg!("newlines", self.tokens.peek());
        }
        while let Some(Token { data: TokenData::NewLine, .. }) = self.tokens.peek() {
            self.tokens.next();
        }

    }
}
