use std::collections::HashMap;

use crate::ast::Program;


#[allow(unused)]
enum Value {
    Int(i32),
    String(String),
    Closure {
        blob: usize,
        context: (),
    },
}

#[allow(unused)]
struct ValueCell {
    count: usize,
    value: Value,
}

struct Blob {
    // TODO
}

#[allow(unused)]
struct Runtime {
    values: HashMap<usize, ValueCell>,
    blobs: Vec<Blob>,
}


impl Program {
    pub fn interpret(self) {
        todo!("interpreter")
    }
}
