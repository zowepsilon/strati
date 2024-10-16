use std::iter;
use std::collections::HashMap;

use crate::ast::{BindingKind, Expression, ExpressionData, Ident, Statement};
use crate::interpreter::{Runtime, ThunkKind, TRACE};

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
                        let type_ = self.evaluate(type_, None);
                        assert!(type_.data.is_type(self));

                        (name, type_)
                    })
                    .collect();
                let arg_types: Vec<_> = args.iter().map(|(_, type_)| type_.clone()).collect();

                let last_typing_scope = self
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
                    .push(last_typing_scope);

                // code in non-const function bodies executed at const time have block-like scoping
                self.scopes
                    .push(self.scopes.last().cloned().unwrap_or_default());

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
                let return_type = self.evaluate(return_type, None);

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
                            .untyped(),
                            None,
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
                        "{} is not a subtype of {}",
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
                let context = self
                    .const_state
                    .as_mut()
                    .expect("const method called at runtime")
                    .scopes
                    .last()
                    .expect("current scope should exist")
                    .clone();

                self.const_state
                    .as_mut()
                    .expect("const method called at runtime")
                    .scopes
                    .push(context);

                // code in non-const function bodies executed at const time have block-like scoping
                self.scopes
                    .push(self.scopes.last().cloned().unwrap_or_default());

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
                let inner = self.evaluate(*inner, None);
                
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
            ED::Thunk(id) => Expression {
                data: ED::Thunk(id),
                type_: self.thunks[id].value.type_.clone(),
            },
            ED::Splice(name) => panic!("cannot type splice ${name}"),
            data @
            ( ED::BuiltinInt
            | ED::BuiltinType
            | ED::BuiltinQuote
            | ED::BuiltinString
            | ED::FunType { .. } ) => Expression {
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
            Statement::Binding { kind: BindingKind::Let, variable, annotation, value } => match annotation {
                Some(annotation) => {
                    let annotation = self.evaluate(annotation, None);
                    let value = self.type_expression(value);

                    assert!(value
                        .type_
                        .as_ref()
                        .expect("value should have been typed")
                        .data
                        .is_subtype_of(&annotation.data, self));

                    self.const_state
                        .as_mut()
                        .expect("const method called at runtime")
                        .scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), annotation.clone());

                    typed_statements.push(Statement::Binding {
                        kind: BindingKind::Let,
                        variable,
                        annotation: Some(annotation.clone()),
                        value,
                    });

                    Some(None)
                }
                None => {
                    let value = self.type_expression(value);
                    let type_ = value.type_.clone().expect("value should have been typed");

                    self.const_state
                        .as_mut()
                        .expect("const method called at runtime")
                        .scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), *type_);

                    typed_statements.push(Statement::Binding {
                        kind: BindingKind::Let,
                        variable,
                        annotation: None,
                        value,
                    });

                    Some(None)
                }
            },

            Statement::Binding {
                kind: BindingKind::Const, variable, annotation, value,
            } => match annotation {
                Some(annotation) => {
                    let annotation = self.evaluate(annotation, None);
                    let value = self.evaluate(value, Some(variable.to_string()));
                    // type check if the programmer added an annotation
                    let value = self.type_expression(value);

                    assert!(
                        value.type_.as_ref().expect("value should have been typed").data.is_subtype_of(&annotation.data, self),
                        "{} is not a subtype of {}",
                        value.type_.expect("value should have been typed").data,
                        annotation.data
                    );

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), value.clone());

                    None
                }
                None => {
                    let value = self.evaluate(value, Some(variable.to_string()));
                    // note: const value should not be typed

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), value.clone());

                    None
                }
            },
        }
    }

    fn interpolate_ident(&self, ident: Ident) -> Ident {
        if TRACE { eprintln!("interpolating {ident:?}"); }

        Ident::Plain(match ident {
            Ident::Plain(var) => var,
            Ident::Splice(name) => match self.get_variable(&name) {
                Expression { data: ExpressionData::StringLiteral(value), .. } => value,
                other => panic!("{} cannot be used to interpolate ${name} in variable name", other.data),
            }
        })
    }

    pub fn interpolate_statement(&self, stmt: Statement) -> Statement {
        match stmt {
            Statement::Binding { kind, variable, annotation, value } => {
                Statement::Binding {
                    kind,
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
                                    Statement::Binding { kind, variable, annotation, value } => {
                                        escaped.push(Statement::Binding { kind, variable, annotation, value: self.escape(value)?, })
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
    fn is_subtype_of(&self, other: &ExpressionData, rt: &Runtime) -> bool {
        use ExpressionData as ED;

        match (self, other) {
            (ED::Identifier(_), _)
            | (_, ED::Identifier(_))
            | (ED::Call { .. }, _)
            | (_, ED::Call { .. })
            | (ED::Block { .. }, _)
            | (_, ED::Block { .. })
            | (ED::Const(_), _)
            | (_, ED::Const(_)) => panic!("unevaluated expression while checking subtyping"),
            (ED::IntLiteral(_), _)
            | (_, ED::IntLiteral(_))
            | (ED::StringLiteral(_), _)
            | (_, ED::StringLiteral(_))
            | (ED::Quote(_), _)
            | (_, ED::Quote(_))
            | (ED::BuiltinFunction { .. }, _)
            | (_, ED::BuiltinFunction { .. })
            | (ED::Fun { .. }, _)
            | (_, ED::Fun { .. }) => panic!("type error: not a type"),
            (_, ED::BuiltinType) if self.is_type(rt) => true,
            (
                ED::Constructor {
                    name: self_name,
                    data: self_data,
                },
                ED::Constructor {
                    name: other_name,
                    data: other_data,
                },
            ) => {
                self_name == other_name
                    && self_data.len() == other_data.len()
                    && iter::zip(self_data, other_data).all(|(self_field, other_field)| {
                        // constructors are covariant w.r.t their fields
                        self_field.data.is_subtype_of(&other_field.data, rt)
                    })
            }
            (
                ED::FunType {
                    args: self_args,
                    return_type: self_ret,
                },
                ED::FunType {
                    args: other_args,
                    return_type: other_ret,
                },
            ) => {
                self_args.len() == other_args.len()
                && iter::zip(self_args, other_args).all(|(self_arg, other_arg)| {
                    // functions are contravariant w.r.t their arguments
                    other_arg.data.is_subtype_of(&self_arg.data, rt)
                })
                // functions are covariant w.r.t their return type
                && self_ret.as_ref().expect("self should have return type").data
                    .is_subtype_of(&other_ret.as_ref().expect("self should have return type").data, rt)
            }
            (ED::BuiltinInt, ED::BuiltinInt) => true,
            (ED::BuiltinString, ED::BuiltinString) => true,
            (ED::BuiltinQuote, ED::BuiltinQuote) => true,
            _ => false,
        }
    }

    fn is_type(&self, rt: &Runtime) -> bool {
        use ExpressionData as ED;

        match self {
            ED::Identifier(_) | ED::Call { .. } | ED::Block { .. } | ED::Const(_) | ED::Splice(_) => {
                panic!("unevaluated expression while checking subtyping")
            }
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
            ED::Thunk(id) =>
                match rt.thunks[*id].kind {
                    ThunkKind::Function => false,
                    ThunkKind::Type => true,
                }
        }
    }
}

