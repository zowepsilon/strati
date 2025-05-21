use std::iter;
use std::collections::HashMap;

use crate::ast::{BindingKind, Expression, ExpressionData, Ident, Pattern, Statement};
use crate::interpreter::{Runtime, Thunk, TRACE};

#[derive(Debug)]
pub struct ConstState {
    pub scopes: Vec<HashMap<String, Expression>>,
}

// const time methods
impl Runtime {
    pub fn type_expression(&mut self, expr: Expression) -> Expression {
        if TRACE {
            eprintln!("type_expression: {}", expr.data);
        }

        use ExpressionData as ED;

        match expr.data {
            data @ ED::IntLiteral(_) => Expression {
                data,
                type_: Some(Box::new(ExpressionData::BuiltinInt.untyped())),
            },
            data @ ED::StringLiteral(_) => Expression {
                data,
                type_: Some(Box::new(ED::BuiltinString.untyped())),
            },
            ED::Identifier(name) => Expression {
                type_: Some(Box::new(self.get_type_of_variable(&name))),
                data: ED::Identifier(name),
            },
            ED::Constructor { name, data } => {
                let data: Vec<_> = data
                    .into_iter()
                    .map(|field| self.type_expression(field))
                    .collect();
                let field_types = data
                    .iter()
                    .map(|field| {
                        field
                            .type_
                            .as_deref()
                            .expect("fields should have been typed")
                            .clone()
                    })
                    .collect();

                Expression {
                    data: ED::Constructor {
                        name: name.clone(),
                        data,
                    },
                    type_: Some(Box::new(
                        ED::Constructor {
                            name,
                            data: field_types,
                        }
                        .untyped(),
                    )),
                }
            }
            ED::Add(left, right) => {
                let left = self.type_expression(*left);
                let right = self.type_expression(*right);

                let left_type = left.type_.as_ref().expect("expression should have been typed");
                let right_type = right.type_.as_ref().expect("expression should have been typed");

                assert!(
                    left_type.data.is_subtype_of(&ED::BuiltinInt, self),
                    "type error: {} is not a subtype of Int",
                    left_type.data
                );

                assert!(
                    right_type.data.is_subtype_of(&ED::BuiltinInt, self),
                    "type error: {} is not a subtype of Int",
                    right_type.data
                );

                Expression {
                    data: ED::Add(Box::new(left), Box::new(right)),
                    type_: Some(Box::new(ED::BuiltinInt.untyped())),
                }
            }
            ED::Equal(left, right) => {
                let left = self.type_expression(*left);
                let right = self.type_expression(*right);

                Expression {
                    data: ED::Equal(Box::new(left), Box::new(right)),
                    type_: Some(Box::new(ED::Identifier("Bool".to_string()).untyped()))
                }
            },
            ED::Match { value, branches } => {
                let value = self.type_expression(*value);
                
                let mut typed_branches = Vec::with_capacity(branches.len());

                let mut main_type = match branches.get(0) {
                    None => {
                        return Expression {
                            type_: Some(Box::new(ED::unit().untyped())),
                            data: ED::Match { value: Box::new(value), branches: vec![] }
                        };
                    },
                    Some((pat, expr)) => {
                        self.inherit_const_scope();
                        self.inherit_scope();

                        let bindings = self.types_of_pattern(
                                pat, 
                                value.type_.as_ref().expect("typed value should be typed")
                            );

                        self.const_state
                            .as_mut()
                            .expect("const method called at runtime")
                            .scopes
                            .last_mut()
                            .expect("current const scope should exist")
                            .extend(bindings);

                        let expr = self.type_expression(expr.clone());
                        typed_branches.push((pat.clone(), expr.clone()));

                        self.const_state
                            .as_mut()
                            .expect("const method called at runtime")
                            .scopes
                            .pop();
                        self.scopes.pop();
                        
                        *expr.type_.unwrap()
                    }
                };

                let mut branches = branches.into_iter();
                let _ = branches.next();

                for (pat, expr) in branches {
                    self.inherit_const_scope();
                    self.inherit_scope();

                    let bindings = self.types_of_pattern(
                            &pat, 
                            value.type_.as_ref().expect("typed value should be typed")
                        );

                    self.const_state
                        .as_mut()
                        .expect("const method called at runtime")
                        .scopes
                        .last_mut()
                        .expect("current const scope should exist")
                        .extend(bindings);

                    let expr = self.type_expression(expr.clone());

                    main_type = self.merge_types(main_type, *expr.type_.clone().unwrap());

                    typed_branches.push((pat.clone(), expr.clone()));

                    self.const_state
                        .as_mut()
                        .expect("const method called at runtime")
                        .scopes
                        .pop();
                    self.scopes.pop();
                }


                Expression {
                    data: ED::Match { value: Box::new(value), branches: typed_branches },
                    type_: Some(Box::new(main_type))
                }

            }
            ED::Fun {
                args,
                return_type,
                body,
                context,
            } => {
                let ED::Block { statements, flatten } = body.data else {
                    panic!("the parser guarantees that the function body is a block")
                };
                let args: Vec<_> = args
                    .into_iter()
                    .map(|(name, type_)| {
                        let type_ = self.evaluate(type_);
                        assert!(type_.data.is_type(self));

                        (name, type_)
                    })
                    .collect();
                let arg_types: Vec<_> = args.iter().map(|(_, type_)| type_.clone()).collect();
                
                self.inherit_const_scope();
                self.inherit_scope();
                
                let current_scope = self
                    .const_state
                    .as_mut()
                    .expect("const method called at runtime")
                    .scopes
                    .last_mut()
                    .expect("current scope should exist");

                for (name, type_) in args.iter() {
                    current_scope.insert(name.plain_ref().clone(), type_.clone());
                }

                let mut typed_statements = Vec::new();
                let mut last_type = None;
                for stmt in statements {
                    if let Some(ty) = self.type_statement(stmt, &mut typed_statements) {
                        last_type = ty;
                    }
                }

                self.const_state
                    .as_mut()
                    .expect("const method called at runtime")
                    .scopes
                    .pop();
                self.scopes.pop();

                let found_return_type =
                    last_type.unwrap_or_else(|| ED::unit().untyped());

                let return_type =
                    *return_type.unwrap_or_else(|| Box::new(ED::unit().untyped()));
                let return_type = self.evaluate(return_type);

                assert!(found_return_type.data.is_subtype_of(&return_type.data, self));

                let body = Expression {
                    data: ED::Block {
                        statements: typed_statements,
                        flatten,
                    },
                    type_: Some(Box::new(return_type.clone())),
                };

                Expression {
                    data: ED::Fun {
                        args,
                        return_type: Some(Box::new(return_type.clone())),
                        body: Box::new(body),
                        context,
                    },
                    type_: Some(Box::new(
                        ED::FunType {
                            args: arg_types,
                            return_type: Some(Box::new(return_type)),
                        }
                        .untyped(),
                    )),
                }
            },
            ED::Call { func, args: parameters } => {
                if let ED::Identifier(func) = &func.data {
                    if let Some(func) = self.scopes.last().expect("current scope should exist").get(func) {
                        let result = self.evaluate(
                            ED::Call {
                                func: Box::new(func.clone()),
                                args: parameters,
                            }
                            .untyped()
                        );

                        let result = self.escape(result.clone()).unwrap_or_else(|| {
                            panic!("type error: {} cannot escape const time", result.data);
                        });

                        return self.type_expression(result);
                    }

                }
                
                let func = self.type_expression(*func);

                let (args, return_type) = match &func.type_.as_ref().expect("func should have been typed").data {
                    ED::FunType { args, return_type } => (args, return_type),
                    other => panic!("type error: cannot call {other:?}"),
                };

                let parameters: Vec<_> = parameters
                    .into_iter()
                    .map(|arg| self.type_expression(arg))
                    .collect();

                assert_eq!(args.len(), parameters.len(), "type error: invalid argument count");

                for (arg, param) in iter::zip(args, parameters.iter()) {
                    let param_type = &param
                        .type_
                        .as_ref()
                        .expect("parameters should be typed")
                        .data;

                    assert!(
                        param_type.is_subtype_of(&arg.data, self),
                        "type error: {} is not a subtype of {}",
                        param_type,
                        arg.data
                    );
                }

                Expression {
                    type_: Some(
                        return_type
                            .as_ref()
                            .expect("function should have return type")
                            .clone(),
                    ),
                    data: ExpressionData::Call {
                        func: Box::new(func),
                        args: parameters,
                    },
                }
            }
            ED::Block { statements, flatten } => {
                self.inherit_const_scope();
                self.inherit_scope();

                let mut typed_statements = Vec::new();
                let mut last_type = None;
                for stmt in statements {
                    if let Some(ty) = self.type_statement(stmt, &mut typed_statements) {
                        last_type = ty;
                    }
                }

                self.const_state
                    .as_mut()
                    .expect("const method called at runtime")
                    .scopes
                    .pop();
                self.scopes.pop();

                let return_type = last_type.unwrap_or_else(|| ED::unit().untyped());

                Expression {
                    data: ED::Block {
                        statements: typed_statements,
                        flatten,
                    },
                    type_: Some(Box::new(return_type)),
                }
            }
            ED::Const(inner) => {
                let inner = self.evaluate(*inner);
                
                let inner = self.escape(inner.clone()).unwrap_or_else(|| {
                    panic!("type error: {} cannot escape const time", inner.data);
                });

                self.type_expression(inner)
            }
            ED::Quote(inner) => {
                Expression {
                    data: ExpressionData::Quote(inner),
                    type_: Some(Box::new(ED::BuiltinQuote.untyped())),
                }
            },
            ED::Closure{..} => unreachable!("closure should never occur at this stage"),
            ED::Thunk(_) => unreachable!("thunk should never occur at this stage"),
            ED::Splice(name) => panic!("type error: cannot type splice ${name}"),
            data @
            ( ED::BuiltinInt
            | ED::BuiltinType
            | ED::BuiltinQuote
            | ED::BuiltinString
            | ED::FunType { .. }
            | ED::SumType(_, _)
            | ED::SumTypeValue(_)) => Expression {
                data,
                type_: Some(Box::new(ED::BuiltinType.untyped())),
            },
            data @ ED::BuiltinFunction { .. } => {
                assert!(expr
                    .type_
                    .as_ref()
                    .is_some_and(|type_| type_.data.is_type(self)));
                Expression {
                    data,
                    type_: expr.type_,
                }
            }
        }
    }

    fn get_type_of_variable(&self, name: &String) -> Expression {
        self.const_state
            .as_ref()
            .expect("const method called at runtime")
            .scopes
            .last()
            .expect("current scope should exist")
            .get(name)
            .unwrap_or_else(|| panic!("type error: unknown variable {name}"))
            .clone()
    }

    fn inherit_const_scope(&mut self) {
        let last_const_scope = self
            .const_state
            .as_ref()
            .expect("const method called at runtime")
            .scopes
            .last()
            .cloned()
            .unwrap_or_default();

        self.const_state
            .as_mut()
            .expect("const method called at runtime")
            .scopes
            .push(last_const_scope);
    }
    
    fn types_of_pattern(&mut self, pat: &Pattern, type_: &Expression) -> HashMap<String, Expression> {
        use ExpressionData as ED;

        let type_ = self.evaluate(type_.clone());

        match (pat, type_.data.clone()) {
            (Pattern::Binding(name), _) => HashMap::from([(name.plain_ref().clone(), type_.clone())]),
            (Pattern::IntLiteral(_), ED::BuiltinInt) => HashMap::new(),
            (Pattern::StringLiteral(_), ED::BuiltinString) => HashMap::new(),
            (Pattern::Constructor { name: pname, data: pdata }, ED::Constructor { name: tname, data: tdata }) => {
                assert_eq!(*pname, tname, "mismatched types between constructor {pname:?} et {tname:?}");

                let mut out = HashMap::new();

                for (p, t) in iter::zip(pdata, tdata) {
                    out.extend(self.types_of_pattern(p, &t).into_iter());
                }

                out
            },
            (Pattern::Constructor { name: pname, data: pdata }, ED::SumTypeValue(variants)) => {
                let pname = match pname {
                    None => String::new(),
                    Some(name) => name.plain_ref().to_string()
                };

                let tdata = &variants[&pname];

                let mut out = HashMap::new();

                for (p, t) in iter::zip(pdata, tdata) {
                    out.extend(self.types_of_pattern(p, &t).into_iter());
                }

                out
            }
            _ => panic!("could not type check pattern matching")
        }
    }

    fn merge_types(&mut self, t1: Expression, t2: Expression) -> Expression {
        match (&t1.data, &t2.data) {
            (x, y) if x == y => t1.clone(),
            _ => self.evaluate(ExpressionData::SumType(Box::new(t1), Box::new(t2)).untyped())
        }
    }

    fn type_statement(&mut self, stmt: Statement, typed_statements: &mut Vec<Statement>) -> Option<Option<Expression>> {
        match stmt {
            Statement::Expression(expr) => {
                let expr = self.type_expression(expr);
                let type_ = expr
                    .type_
                    .clone()
                    .expect("expression should have been typed");

                if let ExpressionData::Block { flatten: true, statements } = expr.data {
                    let mut last_type = None;
                    for stmt in statements {
                        if let Some(ty) = self.type_statement(stmt, typed_statements) {
                            last_type = ty;
                        }
                    }
                    Some(last_type)
                } else {
                    typed_statements.push(Statement::Expression(expr));
                    Some(Some(*type_))
                }

            }
            Statement::Binding { kind: BindingKind::Let, recursive, variable, annotation, value } => {
                let annotation = annotation.map(|a| self.evaluate(a));

                let value =
                    if recursive {
                        self.inherit_const_scope();
                        let state = self.const_state.as_mut().expect("const method called at runtime");

                        let last_scope = state.scopes.last_mut().expect("current scope should exist");

                        match &annotation {
                            Some(a) => {
                                last_scope.insert(variable.plain_ref().clone(), a.clone());
                            },
                            None => {
                                match &value.data {
                                    ExpressionData::Fun { args, return_type, .. } => {
                                        last_scope.insert(
                                            variable.plain_ref().clone(),
                                            ExpressionData::FunType {
                                                args: args.iter().map(|(_, ty)| ty.clone()).collect(),
                                                return_type: Some(return_type.clone().unwrap_or_else(|| Box::new(Expression::unit_typed())))
                                            }.untyped()
                                        );
                                    },
                                    _ => panic!("unannotated recursive binding {}", variable.plain_ref())
                                }
                            }
                        };

                        let value = self.type_expression(value);

                        self.const_state.as_mut().expect("const method called at runtime").scopes.pop();

                        value
                    } else {
                        self.type_expression(value)
                    };
                
                let type_ = value.type_.clone().expect("value should have been typed");

                if let Some(a) = &annotation {
                    assert!(type_.data.is_subtype_of(&a.data, self));
                }

                self.const_state
                    .as_mut()
                    .expect("const method called at runtime")
                    .scopes
                    .last_mut()
                    .expect("current scope should exist")
                    .insert(variable.plain_ref().clone(), *type_);

                typed_statements.push(Statement::Binding {
                    kind: BindingKind::Let,
                    recursive,
                    variable,
                    annotation,
                    value,
                });

                Some(None)
            },

            Statement::Binding { kind: BindingKind::Const, recursive, variable, annotation, value, } => {
                if recursive {
                    let id = self.new_rec(variable.plain_ref().clone());
                    let mut value = self.evaluate(value);

                    annotation.map(|annotation| {
                        let annotation = self.evaluate(annotation);
                        // type check if the programmer added an annotation
                        value = self.type_expression(value.clone());

                        assert!(
                            value.type_.as_ref().expect("value should have been typed").data.is_subtype_of(&annotation.data, self),
                            "type error: {} is not a subtype of {}",
                            value.type_.as_ref().expect("value should have been typed").data,
                            annotation.data
                        );
                    });

                    self.thunks[id] = Thunk::Value(value);
                } else {
                    let mut value = self.evaluate(value);

                    annotation.map(|annotation| {
                        let annotation = self.evaluate(annotation);
                        // type check if the programmer added an annotation
                        value = self.type_expression(value.clone());

                        assert!(
                            value.type_.as_ref().expect("value should have been typed").data.is_subtype_of(&annotation.data, self),
                            "type error: {} is not a subtype of {}",
                            value.type_.as_ref().expect("value should have been typed").data,
                            annotation.data
                        );
                    });

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), value.clone());
                }
            
                None
            },
        }
    }

    fn interpolate_ident(&self, ident: Ident) -> Ident {
        if TRACE { eprintln!("interpolating {ident:?}"); }

        Ident::Plain(match ident {
            Ident::Plain(var) => var,
            Ident::Splice(name) => match self.get_variable(&name) {
                Expression { data: ExpressionData::StringLiteral(value), .. } => value,
                other => panic!("type error: {} cannot be used to interpolate ${name} in variable name", other.data),
            }
        })
    }

    pub fn interpolate_statement(&self, stmt: Statement) -> Statement {
        match stmt {
            Statement::Binding { kind, recursive, variable, annotation, value } => {
                Statement::Binding {
                    kind,
                    recursive,
                    variable: self.interpolate_ident(variable),
                    annotation: annotation.map(|a| self.interpolate_expression(a)),
                    value: self.interpolate_expression(value),
                }
            },
            Statement::Expression(expr) => Statement::Expression(self.interpolate_expression(expr)),
        }
    }

    fn interpolate_expression(&self, expr: Expression) -> Expression {
        if TRACE { eprintln!("interpolating {expr:?}"); }

        use ExpressionData as ED;

        match expr.data {
            ED::Splice(name) => {
                let value = self.get_variable(&name);
                // TODO: escape value
                value
            },
            ED::Constructor { name, data } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Constructor {
                        name: name.map(|n| self.interpolate_ident(n)),
                        data: data.into_iter().map(|arg| self.interpolate_expression(arg)).collect()
                    }
                }
            },

            ED::SumTypeValue(variants) => {
                Expression {
                    type_: expr.type_,
                    data: ED::SumTypeValue(
                        variants
                            .into_iter()
                            .map(|(name, types)| (
                                name,
                                types.into_iter().map(|ty| self.interpolate_expression(ty)).collect()
                            ))
                            .collect()
                    )
                }
            }
            ED::Add(left, right) => {
                Expression {
                    type_: expr.type_,
                    data: ED::Add(
                        Box::new(self.interpolate_expression(*left)), 
                        Box::new(self.interpolate_expression(*right))
                    )
                }
            }
            ED::Equal(left, right) => {
                Expression {
                    type_: expr.type_,
                    data: ED::Equal(
                        Box::new(self.interpolate_expression(*left)), 
                        Box::new(self.interpolate_expression(*right))
                    )
                }
            }
            ED::SumType(left, right) => {
                Expression {
                    type_: expr.type_,
                    data: ED::SumType(
                        Box::new(self.interpolate_expression(*left)), 
                        Box::new(self.interpolate_expression(*right))
                    )
                }
            }
            ED::Fun { args, return_type, body, context } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Fun {
                        args: args.into_iter().map(|(name, ty)| (self.interpolate_ident(name), self.interpolate_expression(ty))).collect(),
                        return_type: return_type.map(|ty| Box::new(self.interpolate_expression(*ty))),
                        body: Box::new(self.interpolate_expression(*body)),
                        context, // context should not exist at this phase
                    }
                }
            },
            ED::Match { value, branches } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Match {
                        value: Box::new(self.interpolate_expression(*value)),
                        branches: branches.into_iter().map(|(p, v)| (self.interpolate_pattern(p), self.interpolate_expression(v))).collect()
                    }
                }
            }
            ED::Closure { value, context } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Closure {
                        value: Box::new(self.interpolate_expression(*value)),
                        context,
                    }
                }
            }
            ED::Call { func, args } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Call {
                        func: Box::new(self.interpolate_expression(*func)),
                        args: args.into_iter().map(|arg| self.interpolate_expression(arg)).collect(),
                    }
                }
            },
            ED::Block { statements, flatten } => {
                Expression {
                    type_: expr.type_,
                    data: ED::Block {
                        statements: statements.into_iter().map(|stmt| self.interpolate_statement(stmt)).collect(),
                        flatten,
                    }
                }
            },
            ED::Const(inner) => {
                Expression {
                    type_: expr.type_,
                    data: ED::Const(Box::new(self.interpolate_expression(*inner)))
                }
            },
            ED::Quote(statements) => {
                Expression {
                    type_: expr.type_,
                    data: ED::Quote(statements.into_iter().map(|stmt| self.interpolate_statement(stmt)).collect()),
                }
            },
            ED::FunType { args, return_type } => {
                Expression {
                    type_: expr.type_,
                    data: ED::FunType {
                        args: args.into_iter().map(|arg| self.interpolate_expression(arg)).collect(),
                        return_type: return_type.map(|ty| Box::new(self.interpolate_expression(*ty)))
                    }
                }
            },
            ED::Thunk(_) => unreachable!("thunks are only created on evaluation, they cannot appear in a quote block inner expression"),
            | ED::IntLiteral(_)
            | ED::StringLiteral(_)
            | ED::Identifier(_) 
            | ED::BuiltinInt 
            | ED::BuiltinString 
            | ED::BuiltinType 
            | ED::BuiltinQuote 
            | ED::BuiltinFunction { .. } => expr,
        }
    }

    fn interpolate_pattern(&self, pat: Pattern) -> Pattern {
        match pat {
            Pattern::Binding(name) => Pattern::Binding(self.interpolate_ident(name)),
            Pattern::IntLiteral(_) => pat,
            Pattern::StringLiteral(_) => pat,
            Pattern::Constructor { name, data } =>
                Pattern::Constructor {
                    name: name.map(|x| self.interpolate_ident(x)),
                    data: data.into_iter().map(|p| self.interpolate_pattern(p)).collect()
                },
            Pattern::FunType { args, return_type } => {
                Pattern::FunType {
                    args: args.into_iter().map(|p| self.interpolate_pattern(p)).collect(),
                    return_type: return_type.map(|x| Box::new(self.interpolate_pattern(*x)))
                }
            },
        }
    }

    fn escape(&self, expr: Expression) -> Option<Expression> {
        if TRACE { dbg!("can_escape", &expr); }
        
        // FIXME: should types be escaped?

        match expr.data {
            | ExpressionData::IntLiteral(_)
            | ExpressionData::Identifier(_) // TODO: escape before type checking
                                            // at this stage the expression was already
                                            // type-checked, so we know this variable is available
                                            // at runtime
            | ExpressionData::StringLiteral(_) => Some(expr),
            | ExpressionData::Const(_) 
            | ExpressionData::FunType { .. } 
            | ExpressionData::SumType(_, _)
            | ExpressionData::SumTypeValue(_)
            | ExpressionData::Splice(_)
            | ExpressionData::BuiltinInt 
            | ExpressionData::BuiltinString 
            | ExpressionData::BuiltinQuote
            | ExpressionData::BuiltinType => None,
            ExpressionData::Constructor { name, data } => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Constructor {
                        name,
                        data: {
                            let mut escaped = Vec::with_capacity(data.len());
                            for arg in data {
                                escaped.push(self.escape(arg)?);
                            }
                            escaped
                        }
                    }
                })
            },
            ExpressionData::Match { value, branches } => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Match {
                        value: Box::new(self.escape(*value)?),
                        branches: branches.into_iter().map(|(p, v)| Some((p, self.escape(v)?))).collect::<Option<Vec<_>>>()?
                    }
                })
            }
            ExpressionData::Add(left, right) => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Add(Box::new(self.escape(*left)?), Box::new(self.escape(*right)?))
                })
            }
            ExpressionData::Equal(left, right) => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Equal(Box::new(self.escape(*left)?), Box::new(self.escape(*right)?))
                })
            }
            ExpressionData::Fun { body, context, args, return_type } => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Fun {
                        body: Box::new(self.escape(*body)?),
                        context: {
                            let mut escaped = HashMap::with_capacity(context.len());

                            for (key, value) in context {
                                escaped.insert(key, self.escape(value)?);
                            }

                            escaped
                        },
                        args,
                        return_type,
                    }
                })
            }
            ExpressionData::Closure { value, context } => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Closure {
                        value: Box::new(self.escape(*value)?),
                        context: {
                            let mut escaped = HashMap::with_capacity(context.len());

                            for (key, value) in context {
                                escaped.insert(key, self.escape(value)?);
                            }

                            escaped
                        },
                    }
                })
            }
            ExpressionData::Call { func, args } => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Call {
                        func: Box::new(self.escape(*func)?),
                        args: {
                            let mut escaped = Vec::with_capacity(args.len());
                            for arg in args {
                                escaped.push(self.escape(arg)?);
                            }
                            escaped
                        }
                    }
                })
            }
            ExpressionData::Block { statements, flatten } => {
                Some(Expression {
                    type_: expr.type_,
                    data: ExpressionData::Block {
                        flatten,
                        statements: {
                            let mut escaped = Vec::with_capacity(statements.len());

                            for stmt in statements {
                                match stmt {
                                    Statement::Binding { kind, recursive, variable, annotation, value } => {
                                        escaped.push(Statement::Binding { kind, recursive, variable, annotation, value: self.escape(value)?, })
                                    },
                                    Statement::Expression(expr) =>
                                        escaped.push(Statement::Expression(self.escape(expr)?)),
                                }
                            }
                            escaped
                        }
                    }
                })
            },
            ExpressionData::Quote(inner) => {
                Some(Expression {
                    type_: None,
                    data: ExpressionData::Block {
                        statements: inner,
                        flatten: true,
                    }
                })
            },
            data @ ExpressionData::BuiltinFunction { runtime_available, .. } =>
                if runtime_available {
                    Some(Expression { data, type_: expr.type_ })
                } else {
                    None
                },
            ExpressionData::Thunk(id) => todo!("thunk {id} escaping"),
        }
    }

}

impl ExpressionData {
    fn is_subtype_of(&self, other: &ExpressionData, rt: &mut Runtime) -> bool {
        use ExpressionData as ED;

        match (self, other) {
            (ED::Identifier(_) | ED::Call { .. } | ED::Block { .. } | ED::Closure{..} | ED::Match{..}
            | ED::Splice(_) | ED::Equal(_, _) | ED::Add(_, _) | ED::SumType(_, _) | ED::Const(_), _)
                => rt.evaluate(self.clone().untyped()).data.is_subtype_of(other, rt),

            (_, ED::Identifier(_) | ED::Call { .. } | ED::Block { .. } | ED::Closure{..}
            | ED::Splice(_) | ED::Equal(_, _) | ED::Add(_, _) | ED::SumType(_, _) | ED::Const(_))
                => self.is_subtype_of(&rt.evaluate(other.clone().untyped()).data, rt),

            | (ED::IntLiteral(_), _) | (_, ED::IntLiteral(_))
            | (ED::StringLiteral(_), _) | (_, ED::StringLiteral(_))
            | (ED::Quote(_), _) | (_, ED::Quote(_))
            | (ED::BuiltinFunction { .. }, _) | (_, ED::BuiltinFunction { .. })
            | (ED::Fun { .. }, _) | (_, ED::Fun { .. }) => panic!("type error: not a type"),

            | (ED::BuiltinInt, ED::BuiltinInt)
            | (ED::BuiltinString, ED::BuiltinString)
            | (ED::BuiltinQuote, ED::BuiltinQuote) => true,
            (_, ED::BuiltinType) if self.is_type(rt) => true,
            (ED::Constructor { name: self_name, data: self_data, },
             ED::Constructor { name: other_name, data: other_data }) => {
                self_name == other_name
                    && self_data.len() == other_data.len()
                    && iter::zip(self_data, other_data).all(|(self_field, other_field)| {
                        // constructors are covariant w.r.t their fields
                        self_field.data.is_subtype_of(&other_field.data, rt)
                    })
            },

            (ED::FunType { args: self_args, return_type: self_ret },
             ED::FunType { args: other_args, return_type: other_ret }) => {
                self_args.len() == other_args.len()
                && iter::zip(self_args, other_args).all(|(self_arg, other_arg)| {
                    // functions are contravariant w.r.t their arguments
                    other_arg.data.is_subtype_of(&self_arg.data, rt)
                })
                // functions are covariant w.r.t their return type
                && self_ret.as_ref().expect("self should have return type").data
                    .is_subtype_of(&other_ret.as_ref().expect("self should have return type").data, rt)
            }

            (ED::SumTypeValue(variants1), ED::SumTypeValue(variants2)) => {
                variants1.iter().all( |(name, types1)| {
                    variants2.get(name).is_some_and( |types2|
                        iter::zip(types1, types2).all(|(t1, t2)| t1.data.is_subtype_of(&t2.data, rt))
                    )
                })
            }

            (ED::Constructor { name, data }, ED::SumTypeValue(variants)) => {
                let name = match name {
                    None => "",
                    Some(n) => n.plain_ref(),
                };

                variants.get(name).is_some_and( |types|
                    iter::zip(data, types).all(|(t1, t2)| t1.data.is_subtype_of(&t2.data, rt))
                )
            }
            
            (ED::SumTypeValue(variants), ED::Constructor { name: name2, data: data2 }) =>
                variants.len() == 1
                && variants.iter().next().is_some_and(|(name1, data1)| {
                    (match name2 {
                        None => name1 == "",
                        Some(name2) => name1 == name2.plain_ref(),
                    }) && iter::zip(data1, data2).all(|(t1, t2)| t1.data.is_subtype_of(&t2.data, rt))
               })
            ,
            (ED::BuiltinType | ED::Thunk(_) | ED::Constructor{..} | ED::FunType{..} | ED::SumTypeValue(_)
             | ED::BuiltinInt | ED::BuiltinString | ED::BuiltinQuote, _)
                => false

        }
    }

    pub fn is_type(&self, rt: &mut Runtime) -> bool {
        use ExpressionData as ED;

        match self {
            | ED::Identifier(_)
            | ED::Call { .. }
            | ED::Block { .. } 
            | ED::Const(_) 
            | ED::Splice(_)
            | ED::Add(_, _)
            | ED::SumType(_, _)
            | ED::Equal(_, _)
            | ED::Closure{..}
            | ED::Match{..}
            | ED::Thunk(_) => 
                rt.evaluate(self.clone().untyped()).data.is_type(rt),
            | ED::IntLiteral(_)
            | ED::StringLiteral(_)
            | ED::BuiltinFunction { .. }
            | ED::Quote(_)
            | ED::Fun { .. } => false,
            | ED::BuiltinInt
            | ED::BuiltinString 
            | ED::BuiltinQuote
            | ED::BuiltinType => true,
            ED::Constructor { name: _, data } => data
                .iter()
                .all(|field| ExpressionData::is_type(&field.data, rt)),
            ED::FunType {
                args,
                return_type,
            } => {
                args.iter().all(|arg| arg.data.is_type(rt))
                    && return_type
                        .as_ref()
                        .expect("self should have return type")
                        .data
                        .is_type(rt)
            },
            ED::SumTypeValue(_) => true,
        }
    }
}
