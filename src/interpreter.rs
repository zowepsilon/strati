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
        match expr.data {
            ExpressionData::BuiltinFunction { .. }
            | ExpressionData::IntLiteral(_)
            | ExpressionData::StringLiteral(_) => expr,
            ExpressionData::Identifier(var) => self.get_variable(&var),
            ExpressionData::Constructor { name, data } => {
                let data = data.into_iter().map(|e| self.evaluate(e)).collect();

                Expression {
                    data: ExpressionData::Constructor { name, data },
                    type_: expr.type_,
                }
            }
            ExpressionData::Fun {
                is_const,
                args,
                return_type,
                body,
                context: _,
            } => {
                if is_const && self.const_state.is_none() {
                    ExpressionData::BuiltinFunction {
                        name: "const_only",
                        handler: |_rt, _args| {
                            panic!("attempted to call const-only function at runtime")
                        },
                    }
                    .untyped()
                } else {
                    let to_bind =
                        find_unbound_variables(&body, args.iter().map(|(name, _)| name).collect());
                    let context = to_bind
                        .into_iter()
                        .map(|name| (name.clone(), self.get_variable(name)))
                        .collect();

                    Expression {
                        data: ExpressionData::Fun {
                            is_const,
                            args,
                            return_type,
                            body,
                            context,
                        },
                        type_: expr.type_,
                    }
                }
            }
            ExpressionData::Call {
                func,
                args: parameters,
            } => match self.evaluate(*func).data {
                ExpressionData::Fun {
                    is_const,
                    args,
                    return_type: _,
                    body,
                    context,
                } => {
                    assert!(
                        self.const_state.is_some() || !is_const,
                        "cannot call const-only function at runtime"
                    );

                    let ExpressionData::Block { statements } = body.data else {
                        panic!("the parser guarantees that the function body is a block")
                    };

                    let parameters: Vec<_> =
                        parameters.into_iter().map(|p| self.evaluate(p)).collect();

                    self.scopes.push(context);

                    assert_eq!(args.len(), parameters.len(), "invalid argument count");

                    let current_scope = self.scopes.last_mut().expect("current scope should exist");
                    for ((arg_name, _), value) in iter::zip(args, parameters) {
                        current_scope.insert(arg_name, value);
                    }

                    let mut last_value = None;
                    for stmt in statements {
                        last_value = self.run_statement(stmt);
                    }

                    self.scopes.pop();

                    last_value.unwrap_or_else(|| ExpressionData::unit().untyped())
                }
                ExpressionData::BuiltinFunction { handler, .. } => {
                    let parameters: Vec<_> =
                        parameters.into_iter().map(|p| self.evaluate(p)).collect();

                    handler(self, parameters)
                }
                _ => panic!("type error: expected closure value"),
            },
            ExpressionData::Block { statements } => {
                self.scopes
                    .push(self.scopes.last().cloned().unwrap_or_default());

                let mut last_value = None;
                for stmt in statements {
                    last_value = self.run_statement(stmt.clone());
                }

                self.scopes.pop();

                last_value.unwrap_or_else(|| ExpressionData::unit().untyped())
            }
            ExpressionData::Const(inner) if self.const_state.is_some() => self.evaluate(*inner),
            ExpressionData::FunType {
                is_const,
                args,
                return_type,
            } => {
                let args = args.into_iter().map(|arg| self.evaluate(arg)).collect();
                let return_type =
                    return_type.unwrap_or_else(|| Box::new(ExpressionData::unit().untyped()));
                let return_type = Some(Box::new(self.evaluate(*return_type)));

                Expression {
                    data: ExpressionData::FunType {
                        is_const,
                        args,
                        return_type,
                    },
                    type_: expr.type_,
                }
            }
            ExpressionData::Const(inner) => panic!(
                "const expression const {} cannot be evaluated at runtime",
                inner.data
            ),
            ExpressionData::BuiltinInt
            | ExpressionData::BuiltinType
            | ExpressionData::BuiltinString => panic!("types cannot be evaluated at runtime"),
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
                    .insert(variable, value);

                None
            }
        }
    }

    fn get_variable(&self, var: &str) -> Expression {
        self.scopes
            .last()
            .expect("current scope should exist")
            .get(var)
            .unwrap_or_else(|| panic!("unknown variable {var}"))
            .clone()
    }

    #[allow(unused)] // TODO: remove this
    fn add_builtin_function(
        &mut self,
        name: &'static str,
        handler: fn(&mut Runtime, Vec<Expression>) -> Expression,
        type_: Expression,
    ) {
        self.scopes
            .last_mut()
            .expect("current scope should exist")
            .insert(
                name.to_string(),
                Expression {
                    data: ExpressionData::BuiltinFunction { name, handler },
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

        match expr.data {
            data @ ExpressionData::IntLiteral(_) => Expression {
                data,
                type_: Some(Box::new(ExpressionData::BuiltinInt.untyped())),
            },
            data @ ExpressionData::StringLiteral(_) => Expression {
                data,
                type_: Some(Box::new(ExpressionData::BuiltinString.untyped())),
            },
            ExpressionData::Identifier(name) => Expression {
                type_: Some(Box::new(self.get_type_of_variable(&name))),
                data: ExpressionData::Identifier(name),
            },
            ExpressionData::Constructor { name, data } => {
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
                    data: ExpressionData::Constructor {
                        name: name.clone(),
                        data,
                    },
                    type_: Some(Box::new(
                        ExpressionData::Constructor {
                            name,
                            data: field_types,
                        }
                        .untyped(),
                    )),
                }
            }
            ExpressionData::Fun {
                is_const,
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
                    current_scope.insert(name.clone(), type_.clone());
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
                    last_type.unwrap_or_else(|| ExpressionData::unit().untyped());

                let return_type =
                    *return_type.unwrap_or_else(|| Box::new(ExpressionData::unit().untyped()));
                let return_type = self.evaluate(return_type);

                assert!(found_return_type.data.is_subtype_of(&return_type.data));

                let body = Expression {
                    data: ExpressionData::Block {
                        statements: typed_statements,
                    },
                    type_: Some(Box::new(return_type.clone())),
                };

                Expression {
                    data: ExpressionData::Fun {
                        is_const,
                        args,
                        return_type: Some(Box::new(return_type.clone())),
                        body: Box::new(body),
                        context,
                    },
                    type_: Some(Box::new(
                        ExpressionData::FunType {
                            is_const,
                            args: arg_types,
                            return_type: Some(Box::new(return_type)),
                        }
                        .untyped(),
                    )),
                }
            }
            ExpressionData::Call {
                func,
                args: parameters,
            } => {
                let func = self.type_expression(*func);

                match &func
                    .type_
                    .as_ref()
                    .expect("func should have been typed")
                    .data
                {
                    ExpressionData::FunType {
                        is_const: false,
                        args,
                        return_type,
                    } => {
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
                                "{:?} is not a subtype of {:?}",
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
                    ExpressionData::FunType { is_const: true, .. } => {
                        let result = self.evaluate(
                            ExpressionData::Call {
                                func: Box::new(func),
                                args: parameters,
                            }
                            .untyped(),
                        );

                        self.type_expression(result)
                    }
                    other => panic!("type error: cannot call {other:?}"),
                }
            }
            ExpressionData::Block { statements } => {
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

                let return_type = last_type.unwrap_or_else(|| ExpressionData::unit().untyped());

                Expression {
                    data: ExpressionData::Block {
                        statements: typed_statements,
                    },
                    type_: Some(Box::new(return_type)),
                }
            }
            ExpressionData::Const(inner) => {
                let inner = self.evaluate(*inner);

                self.type_expression(inner)
            }
            data @ (ExpressionData::BuiltinInt
            | ExpressionData::BuiltinType
            | ExpressionData::BuiltinString
            | ExpressionData::FunType { .. }) => Expression {
                data,
                type_: Some(Box::new(ExpressionData::BuiltinType.untyped())),
            },
            data @ ExpressionData::BuiltinFunction { .. } => {
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
            .unwrap_or_else(|| panic!("unknown variable {name}"))
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
            Statement::Binding {
                kind: BindingKind::Let,
                variable,
                annotation,
                value,
            } => match annotation {
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
                        .insert(variable.clone(), annotation.clone());

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
                        .insert(variable.clone(), *type_);

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
                kind: BindingKind::Const,
                variable,
                annotation,
                value,
            } => match annotation {
                Some(annotation) => {
                    let annotation = self.evaluate(annotation);
                    let value = self.evaluate(value);
                    let value = self.type_expression(value);

                    self.const_state
                        .as_mut()
                        .expect("const method called at runtime")
                        .scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.clone(), annotation.clone());

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.clone(), value.clone());

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
                    let value = self.type_expression(value);
                    let type_ = value.type_.clone().expect("value should have been typed");

                    self.const_state
                        .as_mut()
                        .expect("const method called at runtime")
                        .scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.clone(), *type_);

                    self.scopes
                        .last_mut()
                        .expect("current scope should exist")
                        .insert(variable.clone(), value.clone());

                    let stmt = Statement::Binding {
                        kind: BindingKind::Const,
                        variable,
                        annotation: None,
                        value,
                    };

                    (stmt, None)
                }
            },
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
                    is_const: self_is_const,
                    args: self_args,
                    return_type: self_ret,
                },
                ED::FunType {
                    is_const: other_is_const,
                    args: other_args,
                    return_type: other_ret,
                },
            ) => {
                (!self_is_const || *other_is_const)  // non const-only functions can also be used at const time
                                                        // i.e. fn(T) -> U <: const fn(T) -> U
                                                        // this works because there are no runtime-only features
                && self_args.len() == other_args.len()
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
            _ => false,
        }
    }

    fn is_type(&self) -> bool {
        use ExpressionData as ED;

        match self {
            ED::Identifier(_) | ED::Call { .. } | ED::Block { .. } | ED::Const(_) => {
                panic!("unevaluated expression while checking subtyping")
            }
            ED::IntLiteral(_)
            | ED::StringLiteral(_)
            | ED::BuiltinFunction { .. }
            | ED::Fun { .. } => false,
            ED::BuiltinInt | ED::BuiltinString | ED::BuiltinType => true,
            ED::Constructor { name: _, data } => data
                .iter()
                .all(|field| ExpressionData::is_type(&field.data)),
            ED::FunType {
                is_const: _,
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
    match &expr.data {
        ExpressionData::IntLiteral(_)
        | ExpressionData::StringLiteral(_)
        | ExpressionData::BuiltinFunction { .. }
        | ExpressionData::BuiltinInt
        | ExpressionData::BuiltinString
        | ExpressionData::BuiltinType => HashSet::new(),
        ExpressionData::Identifier(name) => {
            if bound.contains(&name) {
                HashSet::new()
            } else {
                HashSet::from([name])
            }
        }
        ExpressionData::Constructor { name: _, data } => {
            let mut found = HashSet::new();

            for field in data {
                let subfound = find_unbound_variables(field, bound.clone());

                found.extend(subfound.into_iter());
            }

            found
        }
        ExpressionData::Call { func, args } => {
            let mut found = find_unbound_variables(func, bound.clone());

            for field in args {
                let subfound = find_unbound_variables(field, bound.clone());

                found.extend(subfound.into_iter());
            }

            found
        }
        ExpressionData::Fun {
            is_const: _,
            args,
            return_type: _,
            body,
            context: _,
        } => {
            let mut subbound = bound.clone();
            for (arg_name, _) in args {
                subbound.insert(arg_name);
            }

            find_unbound_variables(body, subbound)
        }
        ExpressionData::Block { statements } => {
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
                        subbound.insert(variable);
                    }
                    Statement::Expression(value) => {
                        found.extend(find_unbound_variables(value, subbound.clone()));
                    }
                }
            }

            found
        }
        ExpressionData::Const(inner) => find_unbound_variables(inner, bound),
        ExpressionData::FunType {
            is_const: _,
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
