use std::{collections::HashMap, iter};

use crate::ast::{
    Expression,
    Statement, LetDeclaration, Program,
};

type Scope = HashMap<String, Expression>;

#[derive(Debug, Clone)]
pub struct Typer {
    // a TypingContext should only modify its bottom (current) scope
    // here scopes are lexical
    scopes: Vec<Scope>,
}

impl Typer {
    pub fn new() -> Typer {
        Typer {
            scopes: vec![ HashMap::new() ],
        }
    }

    pub fn type_program(mut self, untyped_program: Program) -> Option<Program> {
        let mut program = Vec::with_capacity(untyped_program.declarations.len());

        for declaration in untyped_program.declarations {
            program.push(self.type_let_declaration(declaration)?);
        }

        Some(Program::new(program))
    }

    fn get(&self, name: &str) -> Option<Expression> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }

        None
    }

    fn add(&mut self, name: String, ty: Expression) {
        self.scopes
            .last_mut()
            .expect("current scope should exist")
            .insert(name, ty);
    }

    fn begin_subscope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_subscope(&mut self) {
        assert!(self.scopes.len() > 1, "cannot end top scope");
        self.scopes.pop();
    }

    fn type_expression(&mut self, expr: Expression) -> Option<Expression> {
        let Expression {kind, mut ty} = expr;

        let (kind, deduced_type) = match kind {
            v @ Expression::Unit => (v, Expression::Unit),
            v @ Expression::IntLiteral(_) => (v, Expression::Identifier("Int".to_string())),
            v @ Expression::StringLiteral(_) => (v, Expression::Identifier("String".to_string())),
            Expression::Identifier(id) => {
                let ty = self.get(&id)?;
                (Expression::Identifier(id), ty)
            },
            Expression::Fun { args, return_type, body } => {
                self.begin_subscope();
                for (name, ty) in &args {
                    self.add(name.clone(), ty.clone());
                }

                let body = self.type_expression(*body)?;
                if body.ty != return_type {
                    return None;
                }
                self.end_subscope();

                let arg_types = args.iter().map(|(_, ty)| ty.clone()).collect();
                let body = Box::new(body);

                let kind = ExpressionKind::Fun { args, return_type: return_type.clone(), body };
                let return_type = Box::new(return_type);
                
                (kind, Type::Fun { args: arg_types, return_type })
            },
            Expression::Call { func, args: untyped_args } => {
                let func = self.type_expression(*func)?;
                let mut args = Vec::with_capacity(untyped_args.len());
                for arg in untyped_args {
                    args.push(self.type_expression(arg)?);
                }
                match func.ty {
                    Type::Fun { args: ref expected_arg_types, ref return_type } => {
                        if !iter::zip(expected_arg_types, &args)
                            .all(|(expected, Expression { ty, .. })| 
                                 *expected == *ty
                        ) { return None }
                        
                        let return_type = return_type.clone();
                        let func = Box::new(func);

                        let kind = ExpressionKind::Call {
                            func,
                            args,
                        };
                        
                        (kind, *return_type)
                    },
                    _ => return None,
                }
            },
            Expression::Block { statements: untyped_statements } => {
                self.begin_subscope();
                let mut statements = Vec::with_capacity(untyped_statements.len());
                for stmt in untyped_statements {
                    statements.push(self.type_statement(stmt)?);
                }
                self.end_subscope();

                let ty = if let Some(Statement::Expression(Expression { ty, .. })) = statements.last() {
                    ty.clone()
                } else {
                    Type::Unit
                };
                
                let kind = Expression::Block { statements };
                (kind, ty)
            },
        };
        
        if ty == Type::Untyped {
           ty = deduced_type;
        } else if ty != deduced_type {
            return None;
        }

        Some(Expression { kind, ty })
    }

    fn type_statement(&mut self, stmt: Statement) -> Option<Statement> {
        match stmt {
            Statement::Let(declaration) => Some(Statement::Let(self.type_let_declaration(declaration)?)),
            Statement::Expression(expr) => Some(Statement::Expression(self.type_expression(expr)?)),
            Statement::Assign { name, value } => {
                let (name, value) = self.type_assign(name, value)?;
                Some(Statement::Assign { name, value })
            }
        }
    }

    fn type_let_declaration(&mut self, declaration: LetDeclaration) -> Option<LetDeclaration> {
        let LetDeclaration { variable, annotation, value } = declaration;

        let value = self.type_expression(*value)?;
        if let Some(ref expected) = annotation {
            if value.ty != *expected { return None; }
        }

        self.add(variable.clone(), value.ty.clone());
        let value = Box::new(value);

        Some(LetDeclaration {
            variable,
            annotation,
            value,
        })
    }

    fn type_assign(&mut self, name: String, value: Expression) -> Option<(String, Expression)> {
        let expected = self.get(&name)?;
        let value = self.type_expression(value)?;

        if expected != value.ty {
            return None;
        }

        Some((name, value))
    }
}
