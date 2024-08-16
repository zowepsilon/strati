use std::{collections::{HashMap, HashSet}, iter};

use crate::ast::{Expression, Program, Statement};

type ObjectId = usize;

#[derive(Debug, Clone)]
enum Object {
    #[allow(unused)]
    Int(i32),
    #[allow(unused)]
    String(String),
    Constructor {
        #[allow(unused)]
        name: Option<String>,
        #[allow(unused)]
        data: Vec<ObjectId>,
    },
    Closure {
        context: HashMap<String, ObjectId>,
        args: Vec<String>,
        body: Expression,
    }
}

#[derive(Debug, Clone)]
struct ObjectCell {
    data: Object,
    #[allow(unused)]
    count: usize,
}

struct Runtime {
    // a Runtime should only modify its bottom (current) scope
    // here scopes are lexical
    scopes: Vec<HashMap<String, ObjectId>>,
    objects: HashMap<ObjectId, ObjectCell>,
    next_id: ObjectId,
}

impl Runtime {
    fn evaluate(&mut self, expr: Expression) -> ObjectId {
        match expr {
            Expression::IntLiteral(i) => self.new_object( Object::Int(i.parse().expect("lexer should have validated string")) ),
            Expression::StringLiteral(s) => self.new_object( Object::String(s) ),
            Expression::Identifier(var) => self.get_variable(&var),
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
                        self.shallow_copy_object(self.get_variable(name))
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
                let Object::Closure { context, args, body } = self.objects[&func_id].data.clone()
                    else { panic!("type error: expected closure value") };

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
    
    #[allow(unused)]
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
            Object::Closure { .. } => todo!(),
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
        }
    }
}

impl Program {
    pub fn interpret(self) {
        let mut rt = Runtime {
            scopes: Vec::new(),
            objects: HashMap::new(),
            next_id: 0,
        };

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
        }
    }
}
