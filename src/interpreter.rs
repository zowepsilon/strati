use std::{
    collections::{HashMap, HashSet},
    iter,
};

use crate::ast::{Expression, ExpressionData, Ident, Program, Statement};
use crate::stage1::ConstState;


#[derive(Debug, Clone)]
pub enum ThunkKind { Function, Type }

#[derive(Debug, Clone)]
pub struct Thunk {
    pub value: Expression,
    pub kind: ThunkKind,
}

#[derive(Debug)]
pub struct Runtime {
    // a Runtime should only modify its bottom (current) scope
    // here scopes are lexical
    pub scopes: Vec<HashMap<String, Expression>>,
    pub const_state: Option<ConstState>,
    pub thunks: Vec<Thunk>,
}

pub const TRACE: bool = false;

impl Runtime {
    fn new(is_const: bool) -> Runtime {
        Runtime {
            scopes: vec![HashMap::new()],
            const_state:
                if is_const {
                    Some(ConstState {
                        scopes: vec![HashMap::new()],
                    })
                } else {
                    None
                },
            thunks: Vec::new(),
        }
    }
}

// runtime/common methods
impl Runtime {
    #[expect(unused)]
    pub fn evaluate(&mut self, expr: Expression, binding: Option<String>) -> Expression {
        if TRACE { eprintln!("evaluate: {}", expr.data) };

        use ExpressionData as ED;

        match expr.data {
            ED::BuiltinFunction { .. }
            | ED::IntLiteral(_)
            | ED::StringLiteral(_) => expr,
            ED::Identifier(var) => self.get_variable(&var),
            ED::Constructor { name, data } => {
                let data = data.into_iter().map(|e| self.evaluate(e, None)).collect();

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
                            .map(|(name, type_)| (name, self.evaluate(type_, None)))
                            .collect();

                    return_type = self.evaluate(return_type, None);
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
            ED::Call { func, args: parameters } => match self.evaluate(*func, None).data {
                ED::Fun {
                    args,
                    return_type: _,
                    body,
                    context,
                } => {
                    let ED::Block { statements, flatten: _ } = body.data else {
                        panic!("the parser guarantees that the function body is a block")
                    };

                    let parameters: Vec<_> =
                        parameters.into_iter().map(|p| self.evaluate(p, None)).collect();

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
                        parameters.into_iter().map(|p| self.evaluate(p, None)).collect();

                    handler(self, parameters)
                }
                _ => panic!("type error: expected closure value"),
            },
            ED::Block { statements, flatten: _ } => {
                self.scopes
                    .push(self.scopes.last().cloned().unwrap_or_default());

                let mut last_value = None;
                for stmt in statements {
                    last_value = self.run_statement(stmt.clone());
                }

                self.scopes.pop();

                last_value.unwrap_or_else(|| Expression::unit_typed())
            }
            ED::FunType { args, return_type } => {
                if self.const_state.is_some() {
                    let args = args.into_iter().map(|arg| self.evaluate(arg, None)).collect();
                    let return_type =
                        return_type.unwrap_or_else(|| Box::new(ExpressionData::unit().untyped()));
                    let return_type = Some(Box::new(self.evaluate(*return_type, None)));

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
            ED::Thunk(id) => self.evaluate(self.thunks[id].value.clone(), None),
            ED::Const(inner) => {
                if self.const_state.is_some() {
                    self.evaluate(*inner, None)
                } else {
                    panic!("const expression const {} cannot be evaluated at runtime", inner.data)
                }
            },
            ED::Quote(inner) => {
                if self.const_state.is_some() {
                    Expression {
                        data: ED::Quote(inner.into_iter().map(|stmt| self.interpolate_statement(stmt)).collect()),
                        type_: expr.type_,
                    }
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

    pub fn run_statement(&mut self, stmt: Statement) -> Option<Expression> {
        match stmt {
            Statement::Expression(expr) => Some(self.evaluate(expr, None)),
            Statement::Binding {
                kind: _,
                variable,
                annotation: _,
                value,
            } => {
                let value = self.evaluate(value, Some(variable.to_string()));

                self.scopes
                    .last_mut()
                    .expect("current scope should exist")
                    .insert(variable.plain(), value);

                None
            }
        }
    }

    pub fn get_variable(&self, var: &str) -> Expression {
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

impl Program {
    pub fn interpret(self) -> Expression {
        let root = ExpressionData::Block {
            statements: self.root,
            flatten: false,
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

        if TRACE { println!("{}", root.data); }

        let mut rt = Runtime::new(false);
        rt.evaluate(root, None)
    }
}

pub fn find_unbound_variables<'a>(
    expr: &'a Expression,
    bound: HashSet<&'a String>,
) -> HashSet<&'a String> {
    use ExpressionData as ED;


    match &expr.data {
        | ED::IntLiteral(_)
        | ED::StringLiteral(_)
        | ED::BuiltinFunction { .. }
        | ED::Thunk(_)
        | ED::BuiltinInt
        | ED::BuiltinString
        | ED::BuiltinQuote
        | ED::BuiltinType => HashSet::new(),
        ED::Identifier(name) | ED::Splice(name) => {
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
        ED::Block { statements, flatten: _ } => {
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
        ED::Quote(_) => unbound_in_quote(expr, bound),
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

fn unbound_in_quote<'a>(expr: &'a Expression, bound: HashSet<&'a String>) -> HashSet<&'a String> {
    use ExpressionData as ED;

    match &expr.data {
        | ED::IntLiteral(_)
        | ED::StringLiteral(_)
        | ED::BuiltinFunction { .. }
        | ED::Thunk(_)
        | ED::BuiltinInt
        | ED::BuiltinString
        | ED::BuiltinQuote
        | ED::BuiltinType
        | ED::Identifier(_)
            => HashSet::new(),
        ED::Splice(name) => {
            if bound.contains(&name) {
                HashSet::new()
            } else {
                HashSet::from([name])
            }
        }
        ED::Constructor { name: _, data } => {
            let mut found = HashSet::new();

            for field in data {
                let subfound = unbound_in_quote(field, bound.clone());
                found.extend(subfound.into_iter());
            }

            found
        }
        ED::Call { func, args } => {
            let mut found = unbound_in_quote(func, bound.clone());

            for field in args {
                let subfound = unbound_in_quote(field, bound.clone());
                found.extend(subfound.into_iter());
            }

            found
        }
        ED::Fun {
            args,
            return_type,
            body,
            context: _,
        } => {
            let mut subbound = bound.clone();
            for (name, ty) in args {
                match name {
                    Ident::Plain(_) => (),
                    Ident::Splice(name) => { subbound.insert(name); },
                };

                subbound.extend(unbound_in_quote(ty, subbound.clone()));
            }

            if let Some(return_type) = return_type {
                subbound.extend(unbound_in_quote(return_type, subbound.clone()));
            }

            unbound_in_quote(body, subbound)
        }
        ED::Block { statements, flatten: _ } => {
            let mut subbound = bound.clone();
            let mut found = HashSet::new();

            for stmt in statements {
                match stmt {
                    Statement::Binding {
                        kind: _,
                        variable,
                        annotation,
                        value,
                    } => {
                        match variable {
                            Ident::Plain(_) => (),
                            Ident::Splice(name) => { subbound.insert(&name); },
                        };

                        if let Some(annotation) = annotation {
                            subbound.extend(unbound_in_quote(annotation, subbound.clone()));
                        }

                        subbound.extend(unbound_in_quote(value, subbound.clone()))
                    }
                    Statement::Expression(value) => {
                        found.extend(unbound_in_quote(value, subbound.clone()));
                    }
                }
            }

            found
        }
        ED::Const(inner) => unbound_in_quote(inner, bound),
        ED::Quote(statements) => {
            let mut subbound = bound.clone();
            let mut found = HashSet::new();

            for stmt in statements {
                match stmt {
                    Statement::Binding {
                        kind: _,
                        variable,
                        annotation,
                        value,
                    } => {
                        match variable {
                            Ident::Plain(_) => (),
                            Ident::Splice(name) => { subbound.insert(&name); },
                        };

                        if let Some(annotation) = annotation {
                            subbound.extend(unbound_in_quote(annotation, subbound.clone()));
                        }

                        subbound.extend(unbound_in_quote(value, subbound.clone()))
                    }
                    Statement::Expression(value) => {
                        found.extend(unbound_in_quote(value, subbound.clone()));
                    }
                }
            }

            found
        }
        ED::FunType {
            args,
            return_type,
        } => {
            let mut found = HashSet::new();

            for field in args {
                let subfound = unbound_in_quote(field, bound.clone());

                found.extend(subfound.into_iter());
            }

            found.extend(unbound_in_quote(
                return_type
                    .as_ref()
                    .expect("expression should have return type"),
                bound,
            ));
            found
        }
    }
}
