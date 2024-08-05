use std::vec;

use crate::lexer::{Token, TokenData};
use crate::ast::{
    Program,
    Expression, 
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
                    dbg!(self.tokens.next());
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
        
        match (self.tokens.peek()?.data.clone(), self.tokens.peek_nth(1)) {
            (TokenData::Let, _) => Some(Statement::Let(self.let_statement()?)),
            (TokenData::Identifier(_), Some(Token { data: TokenData::Assign, .. })) => self.assign_statement(),
            _ => Some(Statement::Expression(self.expression()?))
        }
    }

    fn expression(&mut self) -> Option<Expression> {
        if TRACE { dbg!("expression", self.tokens.peek().unwrap()); }

        let mut expr = match self.tokens.peek()?.data {
            TokenData::Fun => {self.fun_expression()},
            TokenData::Integer(_) => {
                let Some(Token { data: TokenData::Integer(value), .. }) = self.tokens.next()
                    else { unreachable!("self.tokens was peeked successfully") };
                Some(Expression::IntLiteral(value).into())
            },
            TokenData::String(_) => {
                let Some(Token { data: TokenData::String(value), .. }) = self.tokens.next()
                    else { unreachable!("self.tokens was peeked successfully") };
                Some(Expression::StringLiteral(value).into())
            },
            TokenData::Identifier(_) => {
                let Some(Token { data: TokenData::Identifier(value), .. }) = self.tokens.next()
                    else { unreachable!("self.tokens was peeked successfully") };
                Some(Expression::Identifier(value).into())
            },
            TokenData::ParenBlock(_) => {
                let Some(Token { data: TokenData::ParenBlock(inner), .. }) = self.tokens.next()
                    else { unreachable!("self.tokens was peeked successfully") };
                if inner.is_empty() {
                    Some(Expression::Unit.into())
                } else {
                    let mut inner = Parser::new(inner);
                    inner.newlines();
                    let expr = inner.expression()?;
                    Some(expr)
                }
            },
            TokenData::BraceBlock(_) => self.block_expression(),
            _ => None
        }?;

        while let Some(Token {data: TokenData::ParenBlock(_), ..}) = self.tokens.peek() {
            let Some(Token {data: TokenData::ParenBlock(inner), ..}) = self.tokens.next()
                else { unreachable!("self.tokens was peeked successfully") };

            let mut inner = Parser::new(inner);
        
            let args = list!(inner,
                inner.expression()?, 
                sep: TokenData::Comma
            );

            let func = Box::new(expr);

            expr = Expression::Call {
                func,
                args
            }.into();
        }

        Some(expr)
    }

    fn block_expression(&mut self) -> Option<Expression> {
        let Some(Token { data: TokenData::BraceBlock(inner), .. }) = self.tokens.next()
            else { return None };

        if inner.is_empty() {
            return Some(Expression::Block {
                statements: vec![],
            }.into());
        }

        let mut inner = Parser::new(inner);

        inner.newlines();

        let mut statements = vec![];

        while let Some(_) = inner.tokens.peek() {
            statements.push(inner.statement()?);

            match inner.tokens.peek() {
                | Some(Token { data: TokenData::NewLine, .. })
                | None => {
                    let _ = inner.tokens.next();
                },
                _ => expect!(inner, TokenData::NewLine)?,
            }
            inner.newlines();
        }

        Some(Expression::Block { statements }.into())
    }

    fn annotation(&mut self) -> Option<Expression> {
        if TRACE { dbg!("annotation", self.tokens.peek().unwrap()); }
        expect!(self, TokenData::Colon)?;

        self.expression()
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

        expect!(self, TokenData::ThinArrow)?;

        let return_type = self.expression()?;
        let return_type = Box::new(return_type);

        let body = self.block_expression()?;
        let body = Box::new(body);
        
        Some(Expression::Fun {
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
