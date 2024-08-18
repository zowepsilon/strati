use std::{collections::{HashMap, HashSet}, iter};

use crate::ast::{Expression, Program, Statement};

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

struct Runtime {
    // a Runtime should only modify its bottom (current) scope
    // here scopes are lexical
    scopes: Vec<HashMap<String, ObjectId>>,
    objects: HashMap<ObjectId, ObjectCell>,
    next_id: ObjectId,
    meta: bool,
}

impl Runtime {
    fn new(meta: bool) -> Runtime {
        Runtime {
            scopes: vec![HashMap::new()],
            objects: HashMap::new(),
            next_id: 0,
            meta,
        }
    }
}


impl Runtime {
    fn evaluate(&mut self, expr: Expression) -> ObjectId {
        match expr {
            Expression::IntLiteral(i) => self.new_object( Object::Int(i.parse().expect("lexer should have validated string")) ),
            Expression::StringLiteral(s) => self.new_object( Object::String(s) ),
            Expression::Identifier(var) => {
                let id = self.get_variable(&var);
                self.shallow_copy_object(id)
            },
            Expression::Constructor { name, data } => {
                let data = data.into_iter()
                               .map(|e| self.evaluate(e))
                               .collect();
            
                self.new_object(Object::Constructor {
                    name,
                    data,
                })
            },
            Expression::Fun { args, return_type: _, body } => {
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
            Expression::Call { func, args: parameters } => {
                let func_id = self.evaluate(*func);
                match self.objects[&func_id].data.clone() {
                    Object::Closure { context, args, body } => {
                        let Expression::Block { statements } = body
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
                        
                        last_value.unwrap_or_else(||
                            // " . " is the unit value
                            self.new_object(Object::Constructor { name: None, data: Vec::new() })
                        )
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
            Expression::Block { statements } => {
                self.scopes.push(self.scopes.last().cloned().unwrap_or_default());
                
                let mut last_value = None;
                for stmt in statements {
                    last_value = self.run_statement(stmt.clone());
                }
                
                self.scopes.pop();
                
                last_value.unwrap_or_else(||
                    // " . " is the unit value
                    self.new_object(Object::Constructor { name: None, data: Vec::new() })
                )
            },
            Expression::Meta(inner) if self.meta => self.evaluate(*inner),
            Expression::Meta(_) => panic!("meta expressions cannot be evaluated at runtime"),
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
            Statement::Assign { name, value } => {
                let var_id = self.get_variable(&name);
                let value_id = self.evaluate(value);
                
                let object = self.objects[&value_id].clone();
                self.objects.get_mut(&var_id).map(|val| *val = object);

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

    fn shallow_copy_object(&mut self, id: ObjectId) -> ObjectId {
        let object = self.objects.get(&id).expect("object to copy should exist").data.clone();
        self.new_object(object)
    }
    
    fn deep_copy_object(&mut self, id: ObjectId) -> ObjectId {
        let object = match &self.objects.get(&id).expect("object to copy should exist").data {
            | obj @ Object::Int(_)
            | obj @ Object::String(_) => obj.clone(),
            Object::Constructor { name, data } => {
                let name = name.clone();
                let data = data
                    .clone()
                    .into_iter()
                    .map(|field| self.deep_copy_object(field))
                    .collect()
                ;

                Object::Constructor { name, data }
            },
            Object::Closure { context, args, body } => {
                let context = context.clone();
                let args = args.clone();
                let body = body.clone();

                let context =
                    context.into_iter()
                    .map(|(name, id)| (name, self.deep_copy_object(id)))
                    .collect();
                
                Object::Closure { context, args, body }
            },
            obj @ Object::BuiltinFunction{..} => obj.clone(),
        };

        self.new_object(object)
    }

    fn find_unbound_variables<'a, 'b>(
        &'a self,
        expr: &'b Expression,
        bound: HashSet<&'b String>,
    ) -> HashSet<&'b String> {
        match expr {
            | Expression::IntLiteral(_)
            | Expression::StringLiteral(_) => HashSet::new(),
            Expression::Identifier(name) => {
                if bound.contains(&name) {
                    HashSet::new()
                } else {
                    HashSet::from([name])
                }
            },
            Expression::Constructor { name: _, data } => {
                let mut found = HashSet::new();

                for field in data {
                    let subfound = self.find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }

                found
            },
            Expression::Call { func, args } => {
                let mut found = self.find_unbound_variables(func, bound.clone());

                for field in args {
                    let subfound = self.find_unbound_variables(field, bound.clone());

                    found.extend(subfound.into_iter());
                }

                found
            },
            Expression::Fun { args, return_type: _, body } => {
                let mut subbound = bound.clone();
                for (arg_name, _) in args {
                    subbound.insert(arg_name);
                }

                self.find_unbound_variables(body, subbound)
            },
            Expression::Block { statements } => {
                let mut subbound = bound.clone();
                let mut found = HashSet::new();

                for stmt in statements {
                    match stmt {
                        Statement::Let { variable, annotation: _, value } => {
                            found.extend(self.find_unbound_variables(value, subbound.clone()));
                            subbound.insert(variable);
                        },
                        | Statement::Expression(value)
                        | Statement::Assign { name: _, value } => {
                            found.extend(self.find_unbound_variables(value, subbound.clone()));
                        },
                    }
                }

                found
            },
            Expression::Meta(inner) =>
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

            rt.new_object(Object::Constructor { name: None, data: Vec::new() })
        });
        rt.add_builtin_function("clone", |rt, args| {
            if args.len() != 1 {
                panic!("invalid argument count while calling clone");
            }

            rt.deep_copy_object(args[0])
        });
        rt.add_builtin_function("add", |rt, args| {
            if args.len() != 2 {
                panic!("invalid argument count while calling clone");
            }

            let x = &rt.objects[&args[0]].data;
            let y = &rt.objects[&args[1]].data;

            match (x, y) {
                (Object::Int(x), Object::Int(y)) =>
                    rt.new_object(Object::Int(x+y)),
                _ => panic!("type error: expected ints")
            }
        });

        let id = rt.evaluate(Expression::Block {
            statements: self.root,
        });

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
