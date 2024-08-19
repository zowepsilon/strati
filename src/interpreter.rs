use std::{collections::{HashMap, HashSet}, iter};

use crate::ast::{Expression, ExpressionData, Program, Statement};

type ObjectId = usize;

#[derive(Debug, Clone)]
enum Object {
    Int(i32),
    String(String),
    Constructor {
        name: Option<String>,
        data: Vec<ObjectId>,
    },
    Closure {
        context: HashMap<String, ObjectId>,
        args: Vec<String>,
        body: Expression,
    },
    BuiltinFunction {
        name: &'static str,
        handler: fn (&mut Runtime, Vec<ObjectId>) -> ObjectId,
    },
}

#[derive(Debug, Clone)]
struct ObjectCell {
    data: Object,
    #[allow(unused)]
    // TODO: reference counting
    count: usize,
}

#[derive(Debug)]
struct Runtime {
    // a Runtime should only modify its bottom (current) scope
    // here scopes are lexical
    scopes: Vec<HashMap<String, ObjectId>>,
    objects: HashMap<ObjectId, ObjectCell>,
    next_id: ObjectId,
    meta: bool,
}

impl Runtime {
    const UNIT: ObjectId = 0;

    fn new(meta: bool) -> Runtime {
        let mut this = Runtime {
            scopes: vec![HashMap::new()],
            objects: HashMap::new(),
            next_id: 0,
            meta,
        };

        this.new_object(Object::Constructor {
            name: None,
            data: Vec::new(),
        });

        this
    }
}


// runtime/common methods
impl Runtime {
    fn evaluate(&mut self, expr: Expression) -> ObjectId {
        match expr.data {
            ExpressionData::IntLiteral(i) => self.new_object( Object::Int(i.parse().expect("lexer should have validated string")) ),
            ExpressionData::StringLiteral(s) => self.new_object( Object::String(s) ),
            ExpressionData::Identifier(var) => self.get_variable(&var),
            ExpressionData::Constructor { name, data } => {
                let data = data.into_iter()
                               .map(|e| self.evaluate(e))
                               .collect();
            
                self.new_object(Object::Constructor {
                    name,
                    data,
                })
            },
            ExpressionData::Fun { args, return_type: _, body } => {
                let to_bind = self.find_unbound_variables(&body, args.iter().map(|(name, _)| name).collect());
                let context = to_bind.into_iter()
                    .map(|name| (
                        name.clone(), 
                        self.get_variable(name)
                    ))
                    .collect();

                let args = args.into_iter().map(|(name, _ty)| name).collect();

                self.new_object(Object::Closure {
                    context,
                    args,
                    body: *body,
                })
            },
            ExpressionData::Call { func, args: parameters } => {
                let func_id = self.evaluate(*func);
                match self.objects[&func_id].data.clone() {
                    Object::Closure { context, args, body } => {
                        let ExpressionData::Block { statements } = body.data
                            else { panic!("the parser guarantees that the function body is a block") };
                        
                        let parameters: Vec<_> = parameters.into_iter()
                            .map(|p| self.evaluate(p))
                            .collect();

                        self.scopes.push(context);

                        assert_eq!(args.len(), parameters.len(), "invalid argument count");


                        let current_scope = self.scopes.last_mut().expect("current scope should exist");
                        for (arg_name, value) in iter::zip(args, parameters) {
                            current_scope.insert(arg_name, value);
                        }

                        let mut last_value = None;
                        for stmt in statements {
                            last_value = self.run_statement(stmt);
                        }
                        
                        self.scopes.pop();
                        
                        last_value.unwrap_or(Runtime::UNIT)
                    },
                    Object::BuiltinFunction { handler, .. } => {
                        let parameters: Vec<_> = parameters.into_iter()
                            .map(|p| self.evaluate(p))
                            .collect();
                        
                        handler(self, parameters)
                    },
                    _ => panic!("type error: expected closure value")
                }
            },
            ExpressionData::Block { statements } => {
                self.scopes.push(self.scopes.last().cloned().unwrap_or_default());
                
                let mut last_value = None;
                for stmt in statements {
                    last_value = self.run_statement(stmt.clone());
                }
                
                self.scopes.pop();
                
                last_value.unwrap_or(Runtime::UNIT)
            },
            ExpressionData::Meta(inner) if self.meta => self.evaluate(*inner),
            ExpressionData::Meta(_) => panic!("meta expressions cannot be evaluated at runtime"),
        }
    }

    fn run_statement(&mut self, stmt: Statement) -> Option<ObjectId> {
        match stmt {
            Statement::Expression(expr) =>  Some(self.evaluate(expr)),
            Statement::Let { variable, annotation: _, value } => {
                let value_id = self.evaluate(*value);

                self.scopes.last_mut().expect("current scope should exist").insert(variable, value_id);

                None
            },
        }
    }

    fn get_variable(&self, var: &str) -> ObjectId {
        self.scopes
            .last().expect("current scope should exist")
            .get(var)
            .unwrap_or_else(|| panic!("unknown variable {var}"))
            .clone()
    }

    fn new_object(&mut self, data: Object) -> ObjectId {
        let id = self.next_id;
        self.next_id += 1;

        self.objects.insert(id, ObjectCell { data, count: 1 });

        id
    }

    fn find_unbound_variables<'a, 'b>(
        &'a self,
        expr: &'b Expression,
        bound: HashSet<&'b String>,
    ) -> HashSet<&'b String> {
        match &expr.data {
            | ExpressionData::IntLiteral(_)
            | ExpressionData::StringLiteral(_) => HashSet::new(),
            ExpressionData::Identifier(name) => {
                if bound.contains(&name) {
                    HashSet::new()
                } else {
                    HashSet::from([name])
                }
            },
            ExpressionData::Constructor { name: _, data } => {
                let mut found = HashSet::new();

                for field in data {
                    let subfound = self.find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }

                found
            },
            ExpressionData::Call { func, args } => {
                let mut found = self.find_unbound_variables(func, bound.clone());

                for field in args {
                    let subfound = self.find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }

                found
            },
            ExpressionData::Fun { args, return_type: _, body } => {
                let mut subbound = bound.clone();
                for (arg_name, _) in args {
                    subbound.insert(arg_name);
                }

                self.find_unbound_variables(body, subbound)
            },
            ExpressionData::Block { statements } => {
                let mut subbound = bound.clone();
                let mut found = HashSet::new();

                for stmt in statements {
                    match stmt {
                        Statement::Let { variable, annotation: _, value } => {
                            found.extend(self.find_unbound_variables(value, subbound.clone()));
                            subbound.insert(variable);
                        },
                        Statement::Expression(value) => {
                            found.extend(self.find_unbound_variables(value, subbound.clone()));
                        },
                    }
                }

                found
            },
            ExpressionData::Meta(inner) =>
                // is this is the right thing to do? 
                // TODO: change behaviour when implementing metatime closures
                self.find_unbound_variables(inner, bound)
        }
    }

    fn add_builtin_function(&mut self, name: &'static str, handler: fn(&mut Runtime, Vec<ObjectId>) -> ObjectId) {
        let id = self.new_object(Object::BuiltinFunction { name, handler });
        
        self.scopes
            .last_mut()
            .expect("current scope should exist")
            .insert(name.to_string(), id);
    }
}

// metatime methods
impl Runtime { }

impl Program {
    pub fn interpret(self) {
        let mut rt = Runtime::new(false);
        
        rt.add_builtin_function("dump", |rt, args| {
            print!("(");
            for id in args {
                let arg = &rt.objects[&id];
                print!("{}, ", arg.data.fmt(rt));
            }

            println!(")");
            
            Runtime::UNIT
        });
        rt.add_builtin_function("add", |rt, args| {
            if args.len() != 2 {
                panic!("invalid argument count while calling add");
            }

            let x = &rt.objects[&args[0]].data;
            let y = &rt.objects[&args[1]].data;

            match (x, y) {
                (Object::Int(x), Object::Int(y)) =>
                    rt.new_object(Object::Int(x+y)),
                _ => panic!("type error: expected ints")
            }
        });

        rt.add_builtin_function("neg", |rt, args| {
            if args.len() != 1 {
                panic!("invalid argument count while calling neg");
            }

            match &rt.objects[&args[0]].data {
                Object::Int(x) =>
                    rt.new_object(Object::Int(-x)),
                _ => panic!("type error: expected ints")
            }
        });

        let id = rt.evaluate(ExpressionData::Block {
            statements: self.root,
        }.untyped());

        println!("Returned {}", rt.objects[&id].data.fmt(&rt));
    }
}

impl Object {
    fn fmt(&self, rt: &Runtime) -> String {
        match self {
            Object::Int(i) => format!("{i}"),
            Object::String(s) => format!("{s}"),
            Object::Constructor { name, data } => {
                let mut fmt = match name {
                    None => format!("."),
                    Some(name) => format!(".{name}")
                };

                if !data.is_empty() {
                    fmt = format!("{fmt}(");
                    for id in data {
                        let field = &rt.objects[&id].data;
                        fmt = format!("{fmt}{}, ", field.fmt(rt));
                    }

                    fmt = format!("{fmt})");
                }
                fmt
            },
            Object::Closure { context, args, body: _ } => {
                let mut fmt = format!("closure of (");
                for arg in args {
                    fmt = format!("{fmt}{arg}, ")
                }
                fmt = format!("{fmt}) with context {{ ");
                for (name, id) in context {
                    let field = &rt.objects[&id].data;
                    fmt = format!("{fmt}{name} = {}, ", field.fmt(rt))
                }
                fmt = format!("{fmt}}}");
                
                fmt
            },
            Object::BuiltinFunction { name, .. } => format!("builtin function {name}")
        }
    }
}
