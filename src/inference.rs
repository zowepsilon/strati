use std::collections::{HashMap, HashSet};

use crate::{ast::{BindingKind, Expression, Statement}, interpreter::{Runtime, Thunk}};

use crate::ast::ExpressionData as ED;

#[derive(Debug)]
pub struct ConstState {
    pub scopes: Vec<HashMap<String, Expression>>,
    type_variables: HashSet<usize>,
}

impl Runtime {
    fn type_program(&mut self, statements: Vec<Statement>) -> Vec<Statement> {
        //let statements = self.unshadow_variables(statements);
        let statements = statements.into_iter().map(|stmt| self.evaluate_const_bindings_statement(stmt)).collect();
        
        // localiser tous les appels const implicites dans tout le programme
        // créer des variables d'unification pour les arguments à inférer
        // collecter les contraintes sur tout le programme
        // unifier
        // évaluer les appels const avec les arguments unifiés
        // type checking pour la soundness
        
        todo!()
    }

    fn evaluate_const_bindings_statement(&mut self, statement: Statement) -> Statement {
        match statement {
            Statement::Expression(expression) => Statement::Expression(self.evaluate_const_bindings_expression(expression)),
            Statement::Binding { kind: BindingKind::Let, recursive, variable, annotation, value } => {
                Statement::Binding {
                    kind: BindingKind::Let, recursive, variable, annotation,
                    value: self.evaluate_const_bindings_expression(value),
                }
            },
            Statement::Binding { kind: BindingKind::Const, recursive, variable, annotation, value } => {
                if recursive {
                    let id = self.new_rec(variable.plain_ref().clone());
                    let value = self.evaluate(value);

                    self.thunks[id] = Thunk::Value(value);

                    Statement::Binding {
                        kind: BindingKind::Const,
                        value: ED::Thunk(id).untyped(),
                        recursive, variable, annotation
                    }
                } else {
                    let value = self.evaluate(value);

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), value.clone());
                    
                    Statement::Binding { kind: BindingKind::Const, recursive, variable, annotation, value }
                }
            }
        }
    }

    fn evaluate_const_bindings_expression(&mut self, expr: Expression) -> Expression {
        match expr.data {
            | ED::IntLiteral(_)
            | ED::StringLiteral(_)
            | ED::Identifier(_)
            | ED::Splice(_)
            | ED::BuiltinFunction{..}
            | ED::Thunk(_)
            | ED::Const(_)
            | ED::Quote(_)
                => expr,
            ED::Constructor { name, data } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Constructor {
                        name,
                        data: data.into_iter().map(|field| self.evaluate_const_bindings_expression(field)).collect(),
                    }
                }
            },
            ED::Fun { args, return_type, body, context } => {
                self.inherit_scope();
                let body = self.evaluate_const_bindings_expression(*body);
                self.scopes.pop();

                Expression {
                    type_: expr.type_,
                    data: ED::Fun {
                        args, return_type, context,
                        body: Box::new(body),
                    }
                }
            },
            ED::Call { func, args } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Call {
                        func: Box::new(self.evaluate_const_bindings_expression(*func)),
                        args: args.into_iter().map(|field| self.evaluate_const_bindings_expression(field)).collect(),
                    }
                }
            },
            ED::Block { statements, flatten } => {
                self.inherit_scope();

                let statements = statements
                    .into_iter()
                    .map(|stmt| self.evaluate_const_bindings_statement(stmt))
                    .collect();

                self.scopes.pop();

                Expression {
                    type_: expr.type_,
                    data: ED::Block { flatten, statements }
                }
            },
            ED::Equal(left, right) => {
                Expression {
                    type_: expr.type_,
                    data: ED::Equal(
                        Box::new(self.evaluate_const_bindings_expression(*left)),
                        Box::new(self.evaluate_const_bindings_expression(*right)),
                    )
                }
            },
            ED::Add(left, right) => {
                Expression {
                    type_: expr.type_,
                    data: ED::Add(
                        Box::new(self.evaluate_const_bindings_expression(*left)),
                        Box::new(self.evaluate_const_bindings_expression(*right)),
                    )
                }
            },
            | ED::FunType{..}
            | ED::SumType(_, _)
            | ED::SumTypeValue(_)
            | ED::BuiltinInt 
            | ED::BuiltinString 
            | ED::BuiltinType 
            | ED::BuiltinQuote
                => panic!("const expressions in runtime code"),
            ED::Closure {..} => unreachable!("no closure can exist in parsed runtime code"),
        }
    }
}
