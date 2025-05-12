use std::{
    collections::{HashMap, HashSet},
    iter,
};

use crate::ast::{Expression, ExpressionData, Ident, Program, Statement};
use crate::stage1::ConstState;


#[derive(Debug, Clone)]
pub enum Thunk {
    Empty,
    Value(Expression)
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

// runtime/common methods
impl Runtime {
    pub fn evaluate(&mut self, expr: Expression) -> Expression {
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
                    .map(|name| (name.clone(), self.get_raw_variable(name)))
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
                    let ED::Block { statements, flatten: _ } = body.data else {
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
                },
                _ => panic!("type error: expected closure value"),
            },
            ED::Closure { value, context } => {
                self.scopes.push(context);
                let value = self.evaluate(*value);
                self.scopes.pop();

                value
            }
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
            ED::Add(left, right) => {
                let left = self.evaluate(*left);
                let right = self.evaluate(*right);

                match (left.data, right.data) {
                    (ED::IntLiteral(x), ED::IntLiteral(y)) => {
                        let x = x.parse::<i64>().unwrap();
                        let y = y.parse::<i64>().unwrap();

                        Expression {
                            data: ED::IntLiteral((x+y).to_string()),
                            type_: Some(Box::new(ED::BuiltinInt.untyped()))
                        }
                    }
                    (l, r) => panic!("tried adding {l} and {r}"),
                }
            }
            ED::Equal(left, right) => {
                let left = self.evaluate(*left);
                let right = self.evaluate(*right);
                
                // TODO: equality
                Expression::unit_typed()
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
            ED::Thunk(id) => self.get_thunk(id),
            ED::SumType(left, right) => {

                if self.const_state.is_some() {
                    let mut left = *left;
                    let mut right = *right;

                    if let ED::SumType(_, _) = left.data {
                        left = self.evaluate(left);
                    }

                    if let ED::SumType(_, _) = right.data {
                        right = self.evaluate(right);
                    }

                    match (left.data, right.data) {
                        (ED::Constructor {name: name1, data: data1}, ED::Constructor {name: name2, data: data2}) => {
                            assert_ne!(name1, name2, "tried adding non-disjoint constructor types");

                            ED::SumTypeValue([
                                (name1.map_or_else(String::new, Ident::plain), data1.into_iter().map(|t| self.new_closure(t)).collect()),
                                (name2.map_or_else(String::new, Ident::plain), data2.into_iter().map(|t| self.new_closure(t)).collect()),
                            ].into()).untyped()
                        },
                        | (ED::Constructor { name, data }, ED::SumTypeValue(mut variants))
                        | (ED::SumTypeValue(mut variants), ED::Constructor { name, data }) => {
                            let name = name.map_or_else(String::new, Ident::plain);

                            assert!(!variants.contains_key(&name), 
                                "tried adding non-disjoint constructor types {} and {}", 
                                ED::Constructor { name: Some(Ident::Plain(name)), data }, ED::SumTypeValue(variants)
                            );

                            variants.insert(name, data.into_iter().map(|t| self.new_closure(t)).collect());

                            ED::SumTypeValue(variants).untyped()
                        }

                        (ED::SumTypeValue(mut lvariants), ED::SumTypeValue(rvariants)) => {
                            assert!(!lvariants.keys().any(|name| rvariants.contains_key(name)),
                                "tried adding non-disjoint constructor types {} and {}", 
                                ED::SumTypeValue(lvariants), ED::SumTypeValue(rvariants)
                            );

                            lvariants.extend(rvariants.into_iter());

                            ED::SumTypeValue(lvariants).untyped()
                        },
                        (l, r) => panic!("only constructor types may be summed, got {l} and {r}")
                    }

                } else {
                    panic!("sum type {} cannot be evaluated at runtime", ED::SumType(left, right))
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
            | ED::BuiltinString
            | ED::SumTypeValue(_) => {
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
            Statement::Expression(expr) => Some(self.evaluate(expr)),
            Statement::Binding {
                kind: _,
                recursive: false,
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
            },
            Statement::Binding {
                kind: _,
                recursive: true,
                variable,
                annotation: _,
                value,
            } => {
                let id = self.new_rec(variable.plain_ref().clone());
                let value = self.evaluate(value);

                self.thunks[id] = Thunk::Value(value);

                None
            }
        }
    }

    pub fn get_variable(&self, var: &str) -> Expression {
        let mut value = self.get_raw_variable(var);

        while let ExpressionData::Thunk(id) = &value.data {
            value = self.get_thunk(*id);
        }

        value
    }

    pub fn get_raw_variable(&self, var: &str) -> Expression {
        self.scopes
            .last()
            .expect("current scope should exist")
            .get(var)
            .unwrap_or_else(|| panic!(
                "unknown variable {var} at {} time", 
                if self.const_state.is_some() {"const"} else {"run"}
            ))
            .clone()
    }
    
    pub fn new_rec(&mut self, name: String) -> usize {
        let id = self.new_thunk();

        self.scopes
            .last_mut()
            .expect("current scope should exist")
            .insert(name, ExpressionData::Thunk(id).untyped());
        id
    }

    fn new_thunk(&mut self) -> usize {
        let id = self.thunks.len();
        self.thunks.push(Thunk::Empty);

        id
    }

    fn get_thunk(&self, id: usize) -> Expression {
        match &self.thunks[id] {
            Thunk::Empty => panic!("trying to access a thunk while it is not evaluated yet"),
            Thunk::Value(v) => v.clone()
        }
    }

    fn new_closure(&self, expr: Expression) -> Expression {
        Expression {
            type_: expr.type_.clone(),
            data: ExpressionData::Closure {
                value: Box::new(expr),
                context: self.scopes.last().expect("current scope should exist").clone()
            }
        }
    }
 
    pub fn inherit_scope(&mut self) {
        self.scopes.push(self.scopes.last().cloned().unwrap_or_default());
    }
}

impl Program {
    pub fn interpret(self) -> Expression {
        use ExpressionData as ED;

        let mut meta_rt = Runtime {
            scopes: vec![HashMap::from([
                ("Int"   .to_string(), ED::BuiltinInt   .untyped()),
                ("String".to_string(), ED::BuiltinString.untyped()),
                ("Type"  .to_string(), ED::BuiltinType  .untyped()),
                ("dump"  .to_string(), ED::BuiltinFunction {
                    name: "dump",
                    runtime_available: false,
                    handler: |rt, args| {
                        assert_eq!(args.len(), 1, "dump: invalid argument count");
                        assert!(args[0].data.is_type(rt), "dump: argument must be a type");

                        Expression {
                            type_: Some(Box::new(ED::FunType { args: vec![args[0].clone()], return_type: Some(Box::new(ED::unit().untyped())) }.untyped())),
                            data: ED::BuiltinFunction {
                                name: "dump_impl",
                                runtime_available: true,
                                handler: |rt, args| {
                                    assert!(rt.const_state.is_none(), "cannot call dump at stage 1. no side effects are allowed at this stage");

                                    assert_eq!(args.len(), 1, "dump_impl: invalid argument count");

                                    println!("{}", args[0].data);

                                    Expression::unit_typed()
                                }
                            }

                        }
                    }
                }.untyped())
            ])],
            const_state: Some(ConstState { scopes: vec![ HashMap::new() ] }),
            thunks: Vec::new(),
        };


        let root = ED::Block {
            statements: self.root,
            flatten: false,
        }.untyped();

        let root = meta_rt.type_expression(root);

        if false { println!("{}", root.data); }

        let mut rt = Runtime {
            scopes: vec![ HashMap::new() ],
            const_state: None,
            thunks: Vec::new(),
        };

        rt.evaluate(root)
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
        ED::SumTypeValue(variants) => {
            let mut found = HashSet::new();
            
            for v in variants.values() {
                for field in v {
                    let subfound = find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }
            }

            found
        }
        | ED::Add(left, right)
        | ED::Equal(left, right)
        | ED::SumType(left, right) => {
            let mut found = find_unbound_variables(left, bound.clone());
            found.extend(find_unbound_variables(right, bound).into_iter());

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
        },
        ED::Closure { value, context: _ } => find_unbound_variables(value, bound),
        ED::Block { statements, flatten: _ } => {
            let mut subbound = bound.clone();
            let mut found = HashSet::new();

            for stmt in statements {
                match stmt {
                    Statement::Binding {
                        kind: _,
                        recursive,
                        variable,
                        annotation: _,
                        value,
                    } => {
                        let mut value_scope = subbound.clone();
                        if *recursive {
                            value_scope.insert(variable.plain_ref());
                        }

                        found.extend(find_unbound_variables(value, value_scope));
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
        ED::SumTypeValue(variants) => {
            let mut found = HashSet::new();
            
            for v in variants.values() {
                for field in v {
                    let subfound = unbound_in_quote(field, bound.clone());

                    found.extend(subfound.into_iter());
                }
            }

            found
        }
        | ED::Add(left, right)
        | ED::Equal(left, right)
        | ED::SumType(left, right) => {
            let mut found = unbound_in_quote(left, bound.clone());
            found.extend(unbound_in_quote(right, bound).into_iter());

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
                        recursive,
                        variable,
                        annotation,
                        value,
                    } => {
                        match variable {
                            Ident::Plain(_) => (),
                            Ident::Splice(name) => { subbound.insert(&name); },
                        };

                        if let Some(annotation) = annotation {
                            found.extend(unbound_in_quote(annotation, subbound.clone()));
                        }

                        let mut value_scope = subbound.clone();
                        if *recursive {
                            value_scope.insert(variable.plain_ref());
                        }

                        found.extend(unbound_in_quote(value, value_scope));
                    }
                    Statement::Expression(value) => {
                        found.extend(unbound_in_quote(value, subbound.clone()));
                    }
                }
            }

            found
        }
        ED::Closure { value, context: _ } => unbound_in_quote(value, bound),
        ED::Const(inner) => unbound_in_quote(inner, bound),
        ED::Quote(statements) => {
            let mut subbound = bound.clone();
            let mut found = HashSet::new();

            for stmt in statements {
                match stmt {
                    Statement::Binding {
                        kind: _,
                        recursive,
                        variable,
                        annotation,
                        value,
                    } => {
                        match variable {
                            Ident::Plain(_) => (),
                            Ident::Splice(name) => { subbound.insert(&name); },
                        };

                        if let Some(annotation) = annotation {
                            found.extend(unbound_in_quote(annotation, subbound.clone()));
                        }

                        let mut value_scope = subbound.clone();
                        if *recursive {
                            value_scope.insert(variable.plain_ref());
                        }

                        found.extend(unbound_in_quote(value, value_scope));

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
