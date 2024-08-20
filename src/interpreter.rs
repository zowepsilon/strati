use std::{
    collections::{HashMap, HashSet},
    iter,
};

use crate::ast::{Expression, ExpressionData, Program, Statement};

#[derive(Debug)]
struct MetaState {
    scopes: Vec<HashMap<String, Expression>>,
}

#[derive(Debug)]
pub struct Runtime {
    // a Runtime should only modify its bottom (current) scope
    // here scopes are lexical
    scopes: Vec<HashMap<String, Expression>>,
    meta_state: Option<MetaState>,
}

impl Runtime {
    fn new(meta: bool) -> Runtime {
        Runtime {
            scopes: vec![HashMap::new()],
            meta_state: if meta {
                Some(MetaState {
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
                args,
                return_type,
                body,
                context: _,
            } => {
                let to_bind =
                    self.find_unbound_variables(&body, args.iter().map(|(name, _)| name).collect());
                let context = to_bind
                    .into_iter()
                    .map(|name| (name.clone(), self.get_variable(name)))
                    .collect();

                Expression {
                    data: ExpressionData::Fun {
                        args,
                        return_type,
                        body,
                        context,
                    },
                    type_: expr.type_,
                }
            }
            ExpressionData::Call {
                func,
                args: parameters,
            } => match self.evaluate(*func).data {
                ExpressionData::Fun {
                    args,
                    return_type: _,
                    body,
                    context,
                } => {
                    let ExpressionData::Block { statements } = body.data
                        else { panic!("the parser guarantees that the function body is a block") };

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
            ExpressionData::Meta(inner) if self.meta_state.is_some() => self.evaluate(*inner),
            ExpressionData::FunType {args, return_type} => {
                let args = args.into_iter().map(|arg| self.evaluate(arg)).collect();
                let return_type = return_type.unwrap_or_else(|| Box::new(ExpressionData::unit().untyped()));
                let return_type = Some(Box::new(self.evaluate(*return_type)));

                Expression {
                    data: ExpressionData::FunType { args, return_type },
                    type_: expr.type_,
                }
            },
            ExpressionData::Meta(_) => panic!("meta expressions cannot be evaluated at runtime"),
            ExpressionData::BuiltinInt
            | ExpressionData::BuiltinType
            | ExpressionData::BuiltinString => panic!("types cannot be evaluated at runtime"),
        }
    }

    fn run_statement(&mut self, stmt: Statement) -> Option<Expression> {
        match stmt {
            Statement::Expression(expr) => Some(self.evaluate(expr)),
            Statement::Let {
                variable,
                annotation: _,
                value,
            } => {
                let value_id = self.evaluate(value);

                self.scopes
                    .last_mut()
                    .expect("current scope should exist")
                    .insert(variable, value_id);

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

    fn find_unbound_variables<'a, 'b>(
        &'a self,
        expr: &'b Expression,
        bound: HashSet<&'b String>,
    ) -> HashSet<&'b String> {
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
                    let subfound = self.find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }

                found
            }
            ExpressionData::Call { func, args } => {
                let mut found = self.find_unbound_variables(func, bound.clone());

                for field in args {
                    let subfound = self.find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }

                found
            }
            ExpressionData::Fun {
                args,
                return_type: _,
                body,
                context: _,
            } => {
                let mut subbound = bound.clone();
                for (arg_name, _) in args {
                    subbound.insert(arg_name);
                }

                self.find_unbound_variables(body, subbound)
            }
            ExpressionData::Block { statements } => {
                let mut subbound = bound.clone();
                let mut found = HashSet::new();

                for stmt in statements {
                    match stmt {
                        Statement::Let {
                            variable,
                            annotation: _,
                            value,
                        } => {
                            found.extend(self.find_unbound_variables(value, subbound.clone()));
                            subbound.insert(variable);
                        }
                        Statement::Expression(value) => {
                            found.extend(self.find_unbound_variables(value, subbound.clone()));
                        }
                    }
                }

                found
            }
            ExpressionData::Meta(inner) => self.find_unbound_variables(inner, bound),
            ExpressionData::FunType { args, return_type } => {
                let mut found = HashSet::new();

                for field in args {
                    let subfound = self.find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }
                
                found.extend(self.find_unbound_variables(return_type.as_ref().expect("expression should have return type"), bound));
                found
            },
        }
    }

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

// metatime methods
impl Runtime {
    fn meta_type(&mut self, expr: Expression) -> Expression {
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
                type_: Some(Box::new(self.meta_get_type(&name))),
                data: ExpressionData::Identifier(name),
            },
            ExpressionData::Constructor { name, data } => {
                let data: Vec<_> = data
                    .into_iter()
                    .map(|field| self.meta_type(field))
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
                args,
                return_type,
                body,
                context: _,
            } => {
                let ExpressionData::Block { statements } = body.data.clone()
                        else { panic!("the parser guarantees that the function body is a block") };

                let args: Vec<_> = args.into_iter()
                    .map(|(name, type_)| {
                        let type_ = self.evaluate(type_);
                        assert!(type_.data.is_type());

                        (name, type_)
                    })
                    .collect()
                ;
                let arg_types: Vec<_> = args.iter().map(|(_, type_)| type_.clone()).collect();

                let to_bind = self.find_unbound_variables(&body, args.iter().map(|(name, _)| name).collect());

                let context: HashMap<_, _> = to_bind.into_iter()
                    .map(|name| (
                        name.clone(),
                        self.meta_get_type(name)
                    ))
                    .collect()
                ;
                
                self.meta_state.as_mut().expect("meta method called at runtime")
                    .scopes
                    .push(context.clone());
                
                // code in non-meta function bodies executed at meta time have block-like scoping
                self.scopes
                    .push(self.scopes.last().cloned().unwrap_or_default());

                let current_scope = self.meta_state.as_mut().expect("meta method called at runtime")
                    .scopes
                    .last_mut().expect("current scope should exist")
                ;

                for (name, type_) in args.iter() {
                    current_scope.insert(name.clone(), type_.clone());
                }
                
                let mut typed_statements = Vec::new();
                let mut last_type = None;
                for stmt in statements {
                    let (stmt, type_) = self.meta_type_statement(stmt);

                    typed_statements.push(stmt);
                    last_type = type_;
                }

                self.meta_state.as_mut().expect("meta method called at runtime")
                    .scopes.pop();

                self.scopes.pop();
                
                let found_return_type = last_type.unwrap_or_else(|| ExpressionData::unit().untyped());

                let return_type = *return_type.unwrap_or_else(|| Box::new(ExpressionData::unit().untyped()));
                let return_type = self.evaluate(return_type);

                assert!(found_return_type.data.is_subtype_of(&return_type.data));

                Expression {
                    data: ExpressionData::Fun {
                        args,
                        return_type: None,
                        body,
                        context,
                    },
                    type_: Some(Box::new(ExpressionData::FunType {
                        args: arg_types,
                        return_type: Some(Box::new(return_type)),
                    }.untyped())),
                }
            },
            ExpressionData::Call { func, args: parameters } => {
                let func = self.meta_type(*func);
                let parameters: Vec<_> = parameters.into_iter().map(|arg| self.meta_type(arg)).collect();

                match &func.type_.as_ref().expect("func should have been typed").data {
                    ExpressionData::FunType { args, return_type } => {
                        assert_eq!(args.len(), parameters.len(), "invalid argument count");

                        for (arg, param) in iter::zip(args, parameters.iter()) {
                            let param_type = &param.type_.as_ref().expect("parameters should be typed").data;

                            assert!(param_type.is_subtype_of(&arg.data), "{:?} is not a subtype of {:?}", param_type, arg.data);
                        }

                        Expression {
                            type_: Some(return_type.as_ref().expect("function should have return type").clone()),
                            data: ExpressionData::Call {
                                func: Box::new(func),
                                args: parameters,
                            },
                        }
                    },
                    other => panic!("type error: cannot call {other:?}"),
                }
            },
            ExpressionData::Block { statements } => {
                let context =
                    self.meta_state.as_mut().expect("meta method called at runtime")
                        .scopes
                        .last().expect("current scope should exist")
                        .clone();

                self.meta_state.as_mut().expect("meta method called at runtime")
                    .scopes.push(context);
                
                // code in non-meta function bodies executed at meta time have block-like scoping
                self.scopes
                    .push(self.scopes.last().cloned().unwrap_or_default());

                let mut typed_statements = Vec::new();
                let mut last_type = None;
                for stmt in statements {
                    let (stmt, type_) = self.meta_type_statement(stmt);

                    typed_statements.push(stmt);
                    last_type = type_;
                }

                self.meta_state.as_mut().expect("meta method called at runtime").scopes.pop();
                self.scopes.pop();
                
                let return_type = last_type.unwrap_or_else(||
                    ExpressionData::unit().untyped()
                );

                Expression {
                    data: ExpressionData::Block { statements: typed_statements },
                    type_: Some(Box::new(return_type))
                }
            },
            ExpressionData::Meta(inner) => self.evaluate(*inner),
            data @ (
                | ExpressionData::BuiltinInt
                | ExpressionData::BuiltinType
                | ExpressionData::BuiltinString
                | ExpressionData::FunType{..}
            ) => Expression { data, type_: Some(Box::new(ExpressionData::BuiltinType.untyped())) },
            data @ ExpressionData::BuiltinFunction { .. } => {
                assert!(expr.type_.as_ref().is_some_and(|type_| type_.data.is_type()));
                Expression {
                    data,
                    type_: expr.type_
                }
            },
        }
    }

    fn meta_get_type(&self, name: &String) -> Expression {
        self.meta_state.as_ref().expect("meta method called at runtime")
            .scopes.last().expect("current scope should exist")
            .get(name).unwrap_or_else(|| panic!("unknown variable {name}"))
            .clone()
    }

    fn meta_type_statement(&mut self, stmt: Statement) -> (Statement, Option<Expression>) {
        match stmt {
            Statement::Expression(expr) => {
                let expr = self.meta_type(expr);
                let type_ = expr.type_.clone().expect("expression should have been typed");

                (Statement::Expression(expr), Some(*type_))
            }
            Statement::Let { variable, annotation, value } => {
                match annotation {
                    Some(annotation) => {

                        let annotation = self.evaluate(annotation);
                        let value = self.meta_type(value);

                        assert!(
                            value.type_.as_ref().expect("value should have been typed").data
                                .is_subtype_of(&annotation.data)
                        );

                        self.meta_state.as_mut().expect("meta method called at runtime")
                            .scopes.last_mut().expect("current scope should exist")
                            .insert(variable.clone(), annotation.clone());


                        let stmt = Statement::Let {
                            variable,
                            annotation: Some(annotation.clone()),
                            value,
                        };

                        (stmt, Some(annotation))

                    },
                    None => {
                        let value = self.meta_type(value);
                        let type_ = value.type_.clone().expect("value should have been typed");

                        self.meta_state.as_mut().expect("meta method called at runtime")
                            .scopes.last_mut().expect("current scope should exist")
                            .insert(variable.clone(), *type_.clone());

                        let stmt = Statement::Let {
                            variable,
                            annotation: None,
                            value
                        };

                        (stmt, Some(*type_))
                    }
                }
            },
        }
    }
}

impl Program {
    pub fn interpret(self) {
        let root = ExpressionData::Block { statements: self.root }.untyped();
        
        let mut meta_rt = Runtime::new(true);
        
        meta_rt.scopes.last_mut().expect("root scope should exist")
               .insert("Int".to_string(), ExpressionData::BuiltinInt.untyped());
        meta_rt.scopes.last_mut().expect("root scope should exist")
               .insert("String".to_string(), ExpressionData::BuiltinString.untyped());
        meta_rt.scopes.last_mut().expect("root scope should exist")
               .insert("Type".to_string(), ExpressionData::BuiltinType.untyped());

        let root = meta_rt.meta_type(root);
        
        let mut rt = Runtime::new(false);
        let ret = rt.evaluate(root);

        println!("Returned {}", ret.data);
    }
}

impl ExpressionData {
    fn is_subtype_of(&self, other: &ExpressionData) -> bool {
        use ExpressionData as ED;

        match (self, other) {
            | (ED::Identifier(_), _) | (_, ED::Identifier(_))
            | (ED::Call{..}, _)      | (_, ED::Call{..})
            | (ED::Block{..}, _)     | (_, ED::Block{..})
            | (ED::Meta(_), _)       | (_, ED::Meta(_))
                => panic!("unevaluated expression while checking subtyping"),
            | (ED::IntLiteral(_), _)       | (_, ED::IntLiteral(_))
            | (ED::StringLiteral(_), _)    | (_, ED::StringLiteral(_))
            | (ED::BuiltinFunction{..}, _) | (_, ED::BuiltinFunction{..})
            | (ED::Fun{..}, _)             | (_, ED::Fun{..})
                => panic!("type error: not a type"),
            (_, ED::BuiltinType) if self.is_type() => true,
            (ED::Constructor { name: self_name,  data: self_data },
             ED::Constructor { name: other_name, data: other_data })
                => self_name == other_name
                && self_data.len() == other_data.len()
                && iter::zip(self_data, other_data).all(|(self_field, other_field)| {
                    // constructors are covariant w.r.t their fields
                    self_field.data.is_subtype_of(&other_field.data)
                }),
            (ED::FunType { args: self_args,  return_type: self_ret  },
             ED::FunType { args: other_args, return_type: other_ret })
                => self_args.len() == other_args.len()
                && iter::zip(self_args, other_args).all(|(self_arg, other_arg)| {
                    // functions are contravariant w.r.t their arguments
                    other_arg.data.is_subtype_of(&self_arg.data)
                })
                // functions are covariant w.r.t their return type
                && self_ret.as_ref().expect("self should have return type").data
                    .is_subtype_of(&other_ret.as_ref().expect("self should have return type").data),
            (ED::BuiltinInt, ED::BuiltinInt) => true,
            (ED::BuiltinString, ED::BuiltinString) => true,
            _ => false
        }
    }

    fn is_type(&self) -> bool {
        use ExpressionData as ED;

        match self {
            | ED::Identifier(_)
            | ED::Call{..}
            | ED::Block{..}
            | ED::Meta(_)
                => panic!("unevaluated expression while checking subtyping"),
            | ED::IntLiteral(_)
            | ED::StringLiteral(_)
            | ED::BuiltinFunction{..}
            | ED::Fun{..}
                => false,
            | ED::BuiltinInt
            | ED::BuiltinString
            | ED::BuiltinType
                => true,
            ED::Constructor { name: _, data } =>
                data.iter().all(|field| ExpressionData::is_type(&field.data)),
            ED::FunType { args, return_type } =>
                args.iter().all(|arg| ExpressionData::is_type(&arg.data))
                && return_type.as_ref().expect("self should have return type").data.is_type(),
        }
    }
}
