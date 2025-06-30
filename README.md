# Strati

A functional language made for my TIPE in preparatory school that uses types as values at compile time to implement polymorphism. It is implemented as a tree-walk interpreter. It is mostly inspired by Zig and OCaml.

> The associated report (in French) can be found [here](rapport.pdf). It explains more precisely the features of the language.


Example :
```rust
// examples/list.str

const rec List = fun(T: Type) {
    .Nil | .Cons(T, List(T))
}

const map = fun(A: Type, B: Type) {
    let rec map_specialise = fun (xs: List(A), f: Fun(A) -> B) -> List(B) {
        match xs {
            .Nil -> .Nil,
            .Cons(h, t) -> .Cons(f(h), map_specialise(t, f))
        }
    }

    map_specialise
}

let add_one = fun (x: Int) -> Int {
    x + 1
}

let xs: List(Int) = .Cons(1, .Cons(2, .Cons(3, .Nil)))

map(Int, Int)(xs, add_one)

```

Examples can be found in [examples/](examples/). Every example is part of the test suite.

### Architecture

- `src/main.rs`: entry point 
- `src/lexer.rs`: lexer
- `src/parser.rs`: parser
- `src/ast.rs` data types for the abstract syntax tree, which have the double purpose of being the runtime values as well
- `ast/interpreter.rs`: walk-tree interpreter that runs the language once it is typed.
- `ast/stage1.rs`: typing and execution at compile time
- `src/tests.rs`: tests. runs every program in [examples/](examples/).

### Resources

This TIPE was a huge pretext for me to learn A LOT about compilers, type theory and many related subjects. 

The list of resources I've read can be found [here](REFERENCES.md).
