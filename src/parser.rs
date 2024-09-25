use std::collections::HashMap;
use std::vec;

use crate::ast::{BindingKind, Expression, ExpressionData, Ident, Program, Statement};
use crate::lexer::{Token, TokenData};

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: multipeek::MultiPeek<vec::IntoIter<Token>>,
}

macro_rules! expect {
    ($self:ident, $tok:pat) => {
        match $self.tokens.next()?.data {
            $tok => Some(()),
            other => {
                eprintln!(
                    "{}:{}:{}: expected {}, got {other:?}",
                    file!(),
                    line!(),
                    column!(),
                    stringify!($tok)
                );
                None
            }
        }
    };
}

macro_rules! list {
    ($self:ident, $to_parse:expr, sep: $sep:pat) => {
        'list: {
            $self.newlines();

            let first = match $self.tokens.peek() {
                None => break 'list Vec::new(),
                Some(Token { data, .. }) => match data {
                    $sep => {
                        let _ = $self.tokens.next();
                        $self.newlines();
                        break 'list Vec::new();
                    }
                    _ => $to_parse,
                },
            };

            let mut args = vec![first];
            $self.newlines();

            loop {
                match $self.tokens.peek() {
                    None => break,
                    Some(Token { data, .. }) => match data {
                        $sep => {
                            let _ = $self.tokens.next();
                            $self.newlines();
                        }
                        _ => return None,
                    },
                };

                $self.newlines();
                args.push($to_parse);
                $self.newlines();
            }
            args
        }
    };
}

const TRACE: bool = false;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: multipeek::multipeek(tokens),
        }
    }

    pub fn parse(mut self) -> Option<Program> {
        if TRACE {
            dbg!("parse", self.tokens.peek());
        }

        match self.statements() {
            Some(statements) => Some(Program { root: statements }),
            None => {
                dbg!(self.tokens.next());
                None
            }
        }
    }
}

// doc: see syntax.ebnf
impl Parser {
    fn statement(&mut self) -> Option<Statement> {
        if TRACE {
            dbg!("statement", self.tokens.peek());
        }

        match (self.tokens.peek()?.data.clone(), self.tokens.peek_nth(1)) {
            (TokenData::Let, _) => Some(self.let_statement(BindingKind::Let)?),
            (
                TokenData::Const,
                Some(Token {
                    data: TokenData::Identifier(_) | TokenData::Dollar,
                    ..
                }),
            ) => self.let_statement(BindingKind::Const),
            _ => Some(Statement::Expression(self.expression()?)),
        }
    }

    fn expression(&mut self) -> Option<Expression> {
        if TRACE {
            dbg!("expression", self.tokens.peek().unwrap());
        }

        let mut expr = match self.tokens.peek()?.data {
            TokenData::Fn => self.fntype_expression(),
            TokenData::Fun => self.fun_expression(),
            TokenData::Integer(_) => {
                let Some(Token {
                    data: TokenData::Integer(value),
                    ..
                }) = self.tokens.next()
                else {
                    unreachable!("self.tokens was peeked successfully")
                };
                Some(ExpressionData::IntLiteral(value).untyped())
            }
            TokenData::String(_) => {
                let Some(Token {
                    data: TokenData::String(value),
                    ..
                }) = self.tokens.next()
                else {
                    unreachable!("self.tokens was peeked successfully")
                };
                Some(ExpressionData::StringLiteral(value).untyped())
            }
            TokenData::Identifier(_) => {
                let Some(Token {
                    data: TokenData::Identifier(value),
                    ..
                }) = self.tokens.next()
                else {
                    unreachable!("self.tokens was peeked successfully")
                };
                Some(ExpressionData::Identifier(value).untyped())
            },
            TokenData::Dollar => {
                let _ = self.tokens.next();
                match self.tokens.next()?.data {
                    TokenData::Identifier(name) => Some(ExpressionData::Splice(name).untyped()),
                    _ => None,
                }
            }
            TokenData::ParenBlock(_) => {
                let Some(Token {
                    data: TokenData::ParenBlock(inner),
                    ..
                }) = self.tokens.next()
                else {
                    unreachable!("self.tokens was peeked successfully")
                };
                let mut inner = Parser::new(inner);
                inner.newlines();
                let expr = inner.expression()?;
                Some(expr)
            }
            TokenData::BraceBlock(_) => self.block(),
            TokenData::Dot => self.constructor(),
            TokenData::Const => {
                let _ = self.tokens.next();
                match self.tokens.peek()?.data {
                    TokenData::BraceBlock(_) => {
                        Some(ExpressionData::Const(Box::new(self.block()?)).untyped())
                    }
                    _ => None,
                }
            }
            TokenData::Quote => {
                let _ = self.tokens.next();
                match self.tokens.next()?.data {
                    TokenData::BraceBlock(inner) => {
                        let mut inner = Parser::new(inner);

                        Some(ExpressionData::Quote(inner.statements()?).untyped())
                    }
                    _ => None,
                }
            }
            _ => None,
        }?;

        while let Some(Token {
            data: TokenData::ParenBlock(_),
            ..
        }) = self.tokens.peek()
        {
            let Some(Token {
                data: TokenData::ParenBlock(inner),
                ..
            }) = self.tokens.next()
            else {
                unreachable!("self.tokens was peeked successfully")
            };

            let mut inner = Parser::new(inner);

            let args = list!(inner,
                inner.expression()?,
                sep: TokenData::Comma
            );

            let func = Box::new(expr);

            expr = ExpressionData::Call { func, args }.untyped();
        }

        Some(expr)
    }

    fn block(&mut self) -> Option<Expression> {
        let Some(Token {
            data: TokenData::BraceBlock(inner),
            ..
        }) = self.tokens.next()
        else {
            return None;
        };

        let mut inner = Parser::new(inner);

        Some(
            ExpressionData::Block {
                statements: inner.statements()?,
            }
            .untyped(),
        )
    }

    fn statements(&mut self) -> Option<Vec<Statement>> {
        self.newlines();

        let mut statements = vec![];

        while let Some(_) = self.tokens.peek() {
            statements.push(self.statement()?);

            match self.tokens.peek() {
                Some(Token {
                    data: TokenData::NewLine,
                    ..
                })
                | None => {
                    let _ = self.tokens.next();
                }
                _ => expect!(self, TokenData::NewLine)?,
            }
            self.newlines();
        }

        Some(statements)
    }

    fn annotation(&mut self) -> Option<Expression> {
        if TRACE {
            dbg!("annotation", self.tokens.peek().unwrap());
        }
        expect!(self, TokenData::Colon)?;

        self.expression()
    }

    fn ident(&mut self) -> Option<Ident> {
        match self.tokens.next()?.data {
            TokenData::Identifier(name) => Some(Ident::Plain(name)),
            TokenData::Dollar => match self.tokens.next()?.data {
                TokenData::Identifier(name) => Some(Ident::Splice(name)),
                _ => None,
            },
            _ => None,
        }
    }

    fn let_statement(&mut self, kind: BindingKind) -> Option<Statement> {
        if TRACE {
            dbg!("let_statement", self.tokens.peek().unwrap());
        }

        match kind {
            BindingKind::Let => expect!(self, TokenData::Let)?,
            BindingKind::Const => expect!(self, TokenData::Const)?,
        }
        
        let variable = self.ident()?;

        let annotation = match self.tokens.peek()?.data {
            TokenData::Colon => Some(self.annotation()?),
            _ => None,
        };

        expect!(self, TokenData::Assign)?;

        let value = self.expression()?;

        Some(Statement::Binding {
            kind,
            variable,
            annotation,
            value,
        })
    }

    fn fun_expression(&mut self) -> Option<Expression> {
        if TRACE {
            dbg!("fun_expression", self.tokens.peek());
        }
        expect!(self, TokenData::Fun)?;

        let Some(Token {
            data: TokenData::ParenBlock(inner),
            ..
        }) = self.tokens.next()
        else {
            return None;
        };

        let mut inner = Parser::new(inner);

        let args = list!(inner, {
            let name = inner.ident()?;
            let ty = inner.annotation()?;

            (name, ty)
        }, sep: TokenData::Comma);

        let return_type = match self.tokens.peek() {
            Some(Token {
                data: TokenData::ThinArrow,
                ..
            }) => {
                let _ = self.tokens.next();

                Some(Box::new(self.expression()?))
            }
            _ => None,
        };

        let body = self.block()?;
        let body = Box::new(body);

        Some(
            ExpressionData::Fun {
                args,
                return_type,
                body,
                context: HashMap::new(),
            }
            .untyped(),
        )
    }

    fn fntype_expression(&mut self) -> Option<Expression> {
        if TRACE {
            dbg!("fun_expression", self.tokens.peek());
        }

        expect!(self, TokenData::Fn)?;

        let Some(Token {
            data: TokenData::ParenBlock(inner),
            ..
        }) = self.tokens.next()
        else {
            return None;
        };

        let mut inner = Parser::new(inner);

        let args = list!(inner, {
            inner.expression()?
        }, sep: TokenData::Comma);

        let return_type = match self.tokens.peek() {
            Some(Token {
                data: TokenData::ThinArrow,
                ..
            }) => {
                let _ = self.tokens.next();

                Some(Box::new(self.expression()?))
            }
            _ => None,
        };

        Some(ExpressionData::FunType { args, return_type }.untyped())
    }

    fn constructor(&mut self) -> Option<Expression> {
        expect!(self, TokenData::Dot)?;

        let name = match self.tokens.peek() {
            Some(Token {
                data: TokenData::Identifier(_)
                    | TokenData::Dollar,
                ..
            }) => Some(self.ident()?),
            _ => None,
        };

        let data = match self.tokens.peek() {
            Some(Token {
                data: TokenData::ParenBlock(_),
                ..
            }) => {
                let Some(Token {
                    data: TokenData::ParenBlock(inner),
                    ..
                }) = self.tokens.next()
                else {
                    unreachable!("self.tokens was peeked successfully")
                };

                let mut inner = Parser::new(inner);

                list!(inner,
                    inner.expression()?,
                    sep: TokenData::Comma
                )
            }
            _ => Vec::new(),
        };

        Some(ExpressionData::Constructor { name, data }.untyped())
    }

    fn newlines(&mut self) {
        if TRACE {
            dbg!("newlines", self.tokens.peek());
        }
        while let Some(Token {
            data: TokenData::NewLine,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
        }
    }
}
