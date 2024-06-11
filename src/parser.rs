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
    ($self:ident, $to_parse:expr, sep: $sep:pat, end: $end:pat) => {'list: {
        $self.newlines();

        let first = match $self.tokens.peek()?.data {
            $end => {
                let _ = $self.tokens.next();
                
                break 'list Vec::new();
            },
            $sep => {
                let _ = $self.tokens.next();
                $self.newlines();
                expect!($self, $end)?;

                break 'list Vec::new();

            }
            _ => $to_parse,
        };

        let mut args = vec![first];
        $self.newlines();

        loop {
            match $self.tokens.next()?.data {
                $sep => {
                    $self.newlines();
                    if let $end = $self.tokens.peek()?.data {
                        let _ = $self.tokens.next();

                        break;
                    }
                },
                $end => break,
                _ => return None
            }

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
        Parser { tokens: multipeek::multipeek(tokens.into_iter()) }
    }

    pub fn parse(mut self) -> Option<Program> {
        if TRACE { println!("parse {:?}", self.tokens.peek().unwrap()); }
        let mut declarations = vec![];
        
        self.newlines();

        while let Some(_) = self.tokens.peek() {
            let stmt = match self.let_statement() {
                Some(stmt) => stmt,
                None => {
                    eprintln!("{:?}", self.tokens.next().unwrap());

                    return None;
                }
            };
            declarations.push(stmt);
            self.newlines();
        }
        
        Some(declarations)
    }
}

// doc: see syntax.ebnf
impl Parser {
    fn statement(&mut self) -> Option<Statement> {
        if TRACE { println!("statement {:?}", self.tokens.peek().unwrap()); }
        let stmt = match self.tokens.peek()?.data.clone() {
            TokenData::Let => Some(Statement::Let(self.let_statement()?)),
            TokenData::Identifier(_) 
                if self.tokens.peek_nth(1)?.data == TokenData::Assign => self.assign_statement(),
            _ => Some(Statement::Expression(self.expression()?))
        };
        
        stmt
    }

    fn expression(&mut self) -> Option<Expression> {
        if TRACE { println!("expression {:?}", self.tokens.peek().unwrap()); }
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
        if TRACE { println!("annotation {:?}", self.tokens.peek().unwrap()); }
        expect!(self, TokenData::Colon)?;

        self.ty()
    }


    fn expression_value(&mut self) -> Option<Expression> {
        if TRACE { println!("expression_value {:?}", self.tokens.peek().unwrap()); }
        let maybe_func = match self.tokens.next()?.data {
            TokenData::Integer(value) => Some(ExpressionKind::IntLiteral(value).into()),
            TokenData::String(value) => Some(ExpressionKind::StringLiteral(value).into()),
            TokenData::Identifier(value) => Some(ExpressionKind::Identifier(value).into()),
            TokenData::LeftParen => {
                if let Some(Token { data: TokenData::RightParen, .. }) = self.tokens.peek() {
                    let _ = self.tokens.next();
                    Some(ExpressionKind::Unit.into())
                } else {
                    self.newlines();
                    let expr = self.expression()?;

                    expect!(self, TokenData::RightParen)?;
                    Some(expr)
                }
            },
            TokenData::LeftBrace => {
                self.newlines();

                let mut statements = vec![];

                loop {
                    if let Some(Token { data: TokenData::RightBrace, .. }) = self.tokens.peek() {
                        break;
                    }
                    statements.push(self.statement()?);

                    match self.tokens.peek()?.data {
                        TokenData::NewLine => {
                            let _ = self.tokens.next();
                        },
                        TokenData::RightBrace => continue,
                        _ => expect!(self, TokenData::NewLine)?,
                    }
                    self.newlines();
                }


                expect!(self, TokenData::RightBrace)?;
                Some(ExpressionKind::Block { statements }.into())
            },
            _ => None,
        }?;

        if let Some(Token {data: TokenData::LeftParen, ..}) = self.tokens.peek() {
            let _ = self.tokens.next();
            
            let args = list!(self, 
                self.expression()?, 
                sep: TokenData::Comma, 
                end: TokenData::RightParen
            );
            
            let func = Box::new(maybe_func);

            Some(ExpressionKind::Call {
                func,
                args
            }.into())
            
        } else {
            Some(maybe_func)
        }

    }

    fn ty(&mut self) -> Option<Type> {
        if TRACE { println!("ty {:?}", self.tokens.peek().unwrap()); }
        match self.tokens.next()?.data {
            TokenData::Identifier(x) if x == "int" => Some(Type::Int),
            TokenData::Identifier(x) if x == "string" => Some(Type::String),
            TokenData::LeftParen => {
                expect!(self, TokenData::RightParen)?;
                Some(Type::Unit)
            }
            TokenData::Struct => {
                expect!(self, TokenData::LeftBrace)?;

                let fields = list!(self, {
                    let Token { data: TokenData::Identifier(name), .. } = self.tokens.next()?
                        else { return None };

                    let ty = self.annotation()?;

                    (name, ty)
                }, sep: TokenData::Comma, end: TokenData::RightBrace);

                Some(Type::Struct {
                    fields,
                })
            },
            _ => None
        }
    }


    fn let_statement(&mut self) -> Option<LetDeclaration> {
        if TRACE { println!("let_statement {:?}", self.tokens.peek().unwrap()); }
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
        if TRACE { println!("fun_expression {:?}", self.tokens.peek().unwrap()); }
        expect!(self, TokenData::Fun)?;
        expect!(self, TokenData::LeftParen)?;

        let args = list!(self, {
            let Token { data: TokenData::Identifier(name), .. } = self.tokens.next()?
                else { return None };

            let ty = self.annotation()?;

            (name, ty)
        }, sep: TokenData::Comma, end: TokenData::RightParen);

        let return_type = self.annotation()?;
        expect!(self, TokenData::Arrow)?;

        let body = self.expression()?;
        let body = Box::new(body);
        
        Some(ExpressionKind::Fun {
            args,
            return_type,
            body,
        }.into())
    }

    fn newlines(&mut self) {
        if TRACE { println!("newlines {:?}", self.tokens.peek().unwrap()); }
        while let 
            Some(Token {
                data: TokenData::NewLine, .. 
            }) = self.tokens.peek() {
            self.tokens.next();
        }
    }
}
