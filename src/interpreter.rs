use std::{
    collections::{HashMap, HashSet},
    iter,
};

use crate::ast::{BindingKind, Expression, ExpressionData, Program, Statement};

#[derive(Debug)]
struct ConstState {
    scopes: Vec<HashMap<String, Expression>>,
}

#[derive(Debug)]
pub struct Runtime {
    // a Runtime should only modify its bottom (current) scope
    // here scopes are lexical
    scopes: Vec<HashMap<String, Expression>>,
    const_state: Option<ConstState>,
}

const TRACE: bool = false;

impl Runtime {
    fn new(is_const: bool) -> Runtime {
        Runtime {
            scopes: vec![HashMap::new()],
            const_state: if is_const {
                Some(ConstState {
                    scopes: vec![HashMap::new()],
                })
            } else {
                None
            },
        }
    }
}

// runtime/common methods
impl Runtime {
    fn evaluate(&mut self, expr: Expression) -> Expression {
        if TRACE { eprintln!("evaluate: {}", expr.data) };

        use ExpressionData as ED;

        match expr.data {
            ED::BuiltinFunction { .. }
            | ED::IntLiteral(_)
            | ED::StringLiteral(_) => expr,
            ED::Identifier(var) => self.get_variable(&var),
            ED::Constructor { name, data } => {
                let data = data.into_iter().map(|e| self.evaluate(e)).collect();

                Expression {
                    data: ED::Constructor { name, data },
                    type_: expr.type_,
                }
            }
            ED::Fun { mut args, return_type, body, context: _, } => {
                let to_bind =
                    find_unbound_variables(&body, args.iter().map(|(name, _)| name.plain_ref()).collect());
                let context = to_bind
                    .into_iter()
                    .map(|name| (name.clone(), self.get_variable(name)))
                    .collect();


                let mut return_type = *return_type.unwrap_or_else(|| Box::new(ED::unit().untyped()));
                if self.const_state.is_some() {
                    args =
                        args.into_iter()
                            .map(|(name, type_)| (name, self.evaluate(type_)))
                            .collect();

                    return_type = self.evaluate(return_type);
                }

                Expression {
                    data: ED::Fun {
                        args,
                        return_type: Some(Box::new(return_type)),
                        body,
                        context,
                    },
                    type_: expr.type_,
                }
            }
            ED::Call { func, args: parameters } => match self.evaluate(*func).data {
                ED::Fun {
                    args,
                    return_type: _,
                    body,
                    context,
                } => {
                    let ED::Block { statements } = body.data else {
                        panic!("the parser guarantees that the function body is a block")
                    };

                    let parameters: Vec<_> =
                        parameters.into_iter().map(|p| self.evaluate(p)).collect();

                    self.scopes.push(context);

                    assert_eq!(args.len(), parameters.len(), "invalid argument count");

                    let current_scope = self.scopes.last_mut().expect("current scope should exist");
                    for ((arg_name, _), value) in iter::zip(args, parameters) {
                        current_scope.insert(arg_name.plain(), value);
                    }

                    let mut last_value = None;
                    for stmt in statements {
                        last_value = self.run_statement(stmt);
                    }

                    self.scopes.pop();

                    last_value.unwrap_or_else(|| ED::unit().untyped())
                }
                ED::BuiltinFunction { handler, .. } => {
                    let parameters: Vec<_> =
                        parameters.into_iter().map(|p| self.evaluate(p)).collect();

                    handler(self, parameters)
                }
                _ => panic!("type error: expected closure value"),
            },
            ED::Block { statements } => {
                self.scopes
                    .push(self.scopes.last().cloned().unwrap_or_default());

                let mut last_value = None;
                for stmt in statements {
                    last_value = self.run_statement(stmt.clone());
                }

                self.scopes.pop();

                last_value.unwrap_or_else(|| ED::unit().untyped())
            }
            ED::FunType { args, return_type } => {
                if self.const_state.is_some() {
                    let args = args.into_iter().map(|arg| self.evaluate(arg)).collect();
                    let return_type =
                        return_type.unwrap_or_else(|| Box::new(ExpressionData::unit().untyped()));
                    let return_type = Some(Box::new(self.evaluate(*return_type)));

                    Expression {
                        data: ExpressionData::FunType {
                            args,
                            return_type,
                        },
                        type_: expr.type_,
                    }
                } else {
                    panic!("fun type {} cannot be evaluated at runtime", ED::FunType{args, return_type})
                }
            }
            ED::Const(inner) => {
                if self.const_state.is_some() {
                    self.evaluate(*inner)
                } else {
                    panic!("const expression const {} cannot be evaluated at runtime", inner.data)
                }
            },
            ED::Quote(inner) => {
                if self.const_state.is_some() {
                    todo!("quote block evaluation")
                } else {
                    panic!("quote expression {} cannot be evaluated at runtime", ExpressionData::Quote(inner))
                }
            },
            ED::Splice(name) => panic!("cannot evaluate splice ${name}"),
            | ED::BuiltinInt
            | ED::BuiltinType
            | ED::BuiltinQuote
            | ED::BuiltinString => {
                if self.const_state.is_some() {
                    expr
                } else {
                    panic!("types cannot be evaluated at runtime")
                }
            },
        }
    }

    fn run_statement(&mut self, stmt: Statement) -> Option<Expression> {
        match stmt {
            Statement::Expression(expr) => Some(self.evaluate(expr)),
            Statement::Binding {
                kind: _,
                variable,
                annotation: _,
                value,
            } => {
                let value = self.evaluate(value);

                self.scopes
                    .last_mut()
                    .expect("current scope should exist")
                    .insert(variable.plain(), value);

                None
            }
        }
    }

    fn get_variable(&self, var: &str) -> Expression {
        self.scopes
            .last()
            .expect("current scope should exist")
            .get(var)
            .unwrap_or_else(|| panic!("unknown variable {var} at {} time", if self.const_state.is_some() {"const"} else {"run"}))
            .clone()
    }

    #[allow(unused)] // TODO: remove this
    fn add_builtin_function(
        &mut self,
        name: &'static str,
        handler: fn(&mut Runtime, Vec<Expression>) -> Expression,
        type_: Expression,
        runtime_available: bool,
    ) {
        self.scopes
            .last_mut()
            .expect("current scope should exist")
            .insert(
                name.to_string(),
                Expression {
                    data: ExpressionData::BuiltinFunction { name, handler, runtime_available },
                    type_: Some(Box::new(type_)),
                },
            );
    }
}

// const time methods
impl Runtime {
    fn type_expression(&mut self, expr: Expression) -> Expression {
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
                let ED::Block { statements } = body.data else {
                    panic!("the parser guarantees that the function body is a block")
                };

                let args: Vec<_> = args
                    .into_iter()
                    .map(|(name, type_)| {
                        let type_ = self.evaluate(type_);
                        assert!(type_.data.is_type());

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
                    let (stmt, type_) = self.type_statement(stmt);

                    typed_statements.push(stmt);
                    last_type = type_;
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

                assert!(found_return_type.data.is_subtype_of(&return_type.data));

                let body = Expression {
                    data: ED::Block {
                        statements: typed_statements,
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
            /*
                ExpressionData::Fun {
                    is_const: true,
                    args,
                    return_type,
                    body,
                    context,
                } => {
                    let ExpressionData::Block { statements } = body.data else {
                        panic!("the parser guarantees that the function body is a block")
                    };

                    let args: Vec<_> = args
                        .into_iter()
                        .map(|(name, type_)| {
                            let type_ = self.evaluate(type_);
                            assert!(type_.data.is_type());

                            (name, type_)
                        })
                        .collect();
                    let arg_types: Vec<_> = args.iter().map(|(_, type_)| type_.clone()).collect();

                    // code in non-const function bodies executed at const time have block-like scoping
                    let body = Expression {
                        data: ExpressionData::Block {
                            statements,
                        },
                        type_: return_type.clone(),
                    };

                    Expression {
                        data: ExpressionData::Fun {
                            is_const: false,
                            args,
                            return_type: return_type.clone(),
                            body: Box::new(body),
                            context,
                        },
                        type_: Some(Box::new(
                            ExpressionData::FunType {
                                is_const: true,
                                args: arg_types,
                                return_type,
                            }
                            .untyped(),
                        )),
                    }
                },
            */
            ED::Call { func, args: parameters } => {
                if let ED::Identifier(func) = &func.data {
                    if let Some(func) = self.scopes.last().expect("current scope should exist").get(func) {
                        let result = self.evaluate(
                            ED::Call {
                                func: Box::new(func.clone()),
                                args: parameters,
                            }
                            .untyped(),
                        );

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

                assert_eq!(args.len(), parameters.len(), "invalid argument count");

                for (arg, param) in iter::zip(args, parameters.iter()) {
                    let param_type = &param
                        .type_
                        .as_ref()
                        .expect("parameters should be typed")
                        .data;

                    assert!(
                        param_type.is_subtype_of(&arg.data),
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
            ED::Block { statements } => {
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
                    let (stmt, type_) = self.type_statement(stmt);

                    typed_statements.push(stmt);
                    last_type = type_;
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
                    },
                    type_: Some(Box::new(return_type)),
                }
            }
            ED::Const(inner) => {
                let inner = self.evaluate(*inner);

                let inner = self.type_expression(inner);
                
                if !self.can_escape(&inner) {
                    panic!("type error: {} cannot escape const time", inner.data);
                }

                inner
            }
            ED::Quote(inner) => {
                Expression {
                    data: ExpressionData::Quote(inner),
                    type_: Some(Box::new(ED::BuiltinQuote.untyped())),
                }
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
                    .is_some_and(|type_| type_.data.is_type()));
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

    fn type_statement(&mut self, stmt: Statement) -> (Statement, Option<Expression>) {
        match stmt {
            Statement::Expression(expr) => {
                let expr = self.type_expression(expr);
                let type_ = expr
                    .type_
                    .clone()
                    .expect("expression should have been typed");

                (Statement::Expression(expr), Some(*type_))
            }
            Statement::Binding { kind: BindingKind::Let, variable, annotation, value } => match annotation {
                Some(annotation) => {
                    let annotation = self.evaluate(annotation);
                    let value = self.type_expression(value);

                    assert!(value
                        .type_
                        .as_ref()
                        .expect("value should have been typed")
                        .data
                        .is_subtype_of(&annotation.data));

                    self.const_state
                        .as_mut()
                        .expect("const method called at runtime")
                        .scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), annotation.clone());

                    let stmt = Statement::Binding {
                        kind: BindingKind::Let,
                        variable,
                        annotation: Some(annotation.clone()),
                        value,
                    };

                    (stmt, None)
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

                    let stmt = Statement::Binding {
                        kind: BindingKind::Let,
                        variable,
                        annotation: None,
                        value,
                    };

                    (stmt, None)
                }
            },

            Statement::Binding {
                kind: BindingKind::Const, variable, annotation, value,
            } => match annotation {
                Some(annotation) => {
                    let annotation = self.evaluate(annotation);
                    let value = self.evaluate(value);
                    // type check if the programmer added an annotation
                    let value = self.type_expression(value);

                    assert!(
                        value.type_.as_ref().expect("value should have been typed").data.is_subtype_of(&annotation.data),
                        "{} is not a subtype of {}",
                        value.type_.expect("value should have been typed").data,
                        annotation.data
                    );

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), value.clone());

                    let stmt = Statement::Binding {
                        kind: BindingKind::Const,
                        variable,
                        annotation: Some(annotation.clone()),
                        value,
                    };

                    (stmt, None)
                }
                None => {
                    let value = self.evaluate(value);
                    // note: const value should not be typed

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.plain_ref().clone(), value.clone());

                    (Statement::Expression(ExpressionData::unit().untyped()), None)
                }
            },
        }
    }

    fn can_escape(&self, expr: &Expression) -> bool {
        if TRACE { dbg!("can_escape", expr); }
        match &expr.data {
            | ExpressionData::IntLiteral(_)
            | ExpressionData::Identifier(_) // TODO: escape before type checking
                                            // at this stage the expression was already
                                            // type-checked, so we know this variable is available
                                            // at runtime
            | ExpressionData::StringLiteral(_) => true,
            | ExpressionData::Const(_) 
            | ExpressionData::FunType { .. } 
            | ExpressionData::Splice(_)
            | ExpressionData::BuiltinInt 
            | ExpressionData::BuiltinString 
            | ExpressionData::BuiltinQuote
            | ExpressionData::BuiltinType => false,
            ExpressionData::Constructor { name: _, data } => data.iter().all(|arg| self.can_escape(arg)),
            ExpressionData::Fun { body, context, .. } => 
                context.values().all(|val| self.can_escape(val))
                || self.can_escape(body),
            ExpressionData::Call { func, args } =>
                self.can_escape(func)
                || args.iter().all(|arg| self.can_escape(arg)),
            ExpressionData::Block { statements } => {
                statements.iter().all(|stmt| {
                    match stmt {
                        | Statement::Binding { value, .. }
                        | Statement::Expression(value) => self.can_escape(value),
                    }
                })
            },
            ExpressionData::Quote(_) => todo!("turn can_escape into escape"),
            ExpressionData::BuiltinFunction { runtime_available, .. } => *runtime_available,
        }
    }
}

impl Program {
    pub fn interpret(self) {
        let root = ExpressionData::Block {
            statements: self.root,
        }
        .untyped();

        let mut meta_rt = Runtime::new(true);

        meta_rt
            .scopes
            .last_mut()
            .expect("root scope should exist")
            .insert("Int".to_string(), ExpressionData::BuiltinInt.untyped());
        meta_rt
            .scopes
            .last_mut()
            .expect("root scope should exist")
            .insert(
                "String".to_string(),
                ExpressionData::BuiltinString.untyped(),
            );
        meta_rt
            .scopes
            .last_mut()
            .expect("root scope should exist")
            .insert("Type".to_string(), ExpressionData::BuiltinType.untyped());

        let root = meta_rt.type_expression(root);

        println!("{}", root.data);

        let mut rt = Runtime::new(false);
        let ret = rt.evaluate(root);

        println!("Returned {}", ret.data);
    }
}

impl ExpressionData {
    fn is_subtype_of(&self, other: &ExpressionData) -> bool {
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
            (_, ED::BuiltinType) if self.is_type() => true,
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
                        self_field.data.is_subtype_of(&other_field.data)
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
                    other_arg.data.is_subtype_of(&self_arg.data)
                })
                // functions are covariant w.r.t their return type
                && self_ret.as_ref().expect("self should have return type").data
                    .is_subtype_of(&other_ret.as_ref().expect("self should have return type").data)
            }
            (ED::BuiltinInt, ED::BuiltinInt) => true,
            (ED::BuiltinString, ED::BuiltinString) => true,
            (ED::BuiltinQuote, ED::BuiltinQuote) => true,
            _ => false,
        }
    }

    fn is_type(&self) -> bool {
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
                .all(|field| ExpressionData::is_type(&field.data)),
            ED::FunType {
                args,
                return_type,
            } => {
                args.iter().all(|arg| ExpressionData::is_type(&arg.data))
                    && return_type
                        .as_ref()
                        .expect("self should have return type")
                        .data
                        .is_type()
            }
        }
    }
}

fn find_unbound_variables<'a>(
    expr: &'a Expression,
    bound: HashSet<&'a String>,
) -> HashSet<&'a String> {
    use ExpressionData as ED;

    match &expr.data {
        | ED::IntLiteral(_)
        | ED::StringLiteral(_)
        | ED::BuiltinFunction { .. }
        | ED::BuiltinInt
        | ED::BuiltinString
        | ED::BuiltinQuote
        | ED::BuiltinType => HashSet::new(),
        ED::Splice(name) => unreachable!("find_bound_variables on splice ${name}"),
        ED::Identifier(name) => {
            if bound.contains(&name) {
                HashSet::new()
            } else {
                HashSet::from([name])
            }
        }
        ED::Constructor { name: _, data } => {
            let mut found = HashSet::new();

            for field in data {
                let subfound = find_unbound_variables(field, bound.clone());

                found.extend(subfound.into_iter());
            }

            found
        }
        ED::Call { func, args } => {
            let mut found = find_unbound_variables(func, bound.clone());

            for field in args {
                let subfound = find_unbound_variables(field, bound.clone());

                found.extend(subfound.into_iter());
            }

            found
        }
        ED::Fun {
            args,
            return_type: _,
            body,
            context: _,
        } => {
            let mut subbound = bound.clone();
            for (arg_name, _) in args {
                subbound.insert(arg_name.plain_ref());
            }

            find_unbound_variables(body, subbound)
        }
        ED::Block { statements } => {
            let mut subbound = bound.clone();
            let mut found = HashSet::new();

            for stmt in statements {
                match stmt {
                    Statement::Binding {
                        kind: _,
                        variable,
                        annotation: _,
                        value,
                    } => {
                        found.extend(find_unbound_variables(value, subbound.clone()));
                        subbound.insert(variable.plain_ref());
                    }
                    Statement::Expression(value) => {
                        found.extend(find_unbound_variables(value, subbound.clone()));
                    }
                }
            }

            found
        }
        ED::Const(inner) => find_unbound_variables(inner, bound),
        ED::Quote(_) => todo!("find unbound variables in unquotes"),
        ED::FunType {
            args,
            return_type,
        } => {
            let mut found = HashSet::new();

            for field in args {
                let subfound = find_unbound_variables(field, bound.clone());

                found.extend(subfound.into_iter());
            }

            found.extend(find_unbound_variables(
                return_type
                    .as_ref()
                    .expect("expression should have return type"),
                bound,
            ));
            found
        }
    }
}
