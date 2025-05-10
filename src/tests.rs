use std::collections::HashMap;

use crate::ast::{
    self,
    Expression as E,
    ExpressionData,
    ExpressionData::*,
    Ident,
    Statement as Stmt
};

use crate::run_program;

trait Utils: Sized {
    fn some(self) -> Option<Self> {
        Some(self)
    }

    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
impl<T> Utils for T {}

macro_rules! value {
    (i $x:literal) => {
        ExpressionData::IntLiteral($x.to_string()).untyped()
    };

    (s $x:literal) => {
        ExpressionData::StringLiteral($x.into()).untyped()
    };

    (# $x:ident) => {
        ExpressionData::Splice(stringify!($x)).untyped()
    };

    (. $( ($($sub:tt)*) )* ) => {
        ExpressionData::Constructor {
            name: None,
            data: vec![$( value!( $($sub)* ) ),*]
        }.untyped()
    };
    
    (. $name:ident $( ($($sub:tt)*) )* ) => {
        ExpressionData::Constructor {
            name: Some(ast::Ident::Plain(stringify!($name).to_string())),
            data: vec![$( value!( $($sub)* ) ),*]
        }.untyped()
    };
}

pub fn assert_value_eq(x: E, y: E) {
    use ExpressionData as ED;

    match (x.data, y.data) {
        (ED::IntLiteral(x), ED::IntLiteral(y)) => assert_eq!(x.parse::<i64>(), y.parse::<i64>()),
        (ED::StringLiteral(x), ED::StringLiteral(y)) => assert_eq!(x, y),
        (ED::Splice(x), ED::Splice(y)) => assert_eq!(x, y),
        (ED::Constructor {name: name1, data: data1}, ED::Constructor {name: name2, data: data2}) => {
            assert_eq!(name1, name2);

            for (x, y) in std::iter::zip(data1, data2) {
                assert_value_eq(x, y);
            }
        },
        | (ED::BuiltinInt, ED::BuiltinInt)
        | (ED::BuiltinType, ED::BuiltinType)
        | (ED::BuiltinString, ED::BuiltinString)
        | (ED::BuiltinQuote, ED::BuiltinQuote) => (),
        (expected, got) => assert_eq!(expected, got),
    }
}

#[test]
fn empty() {
    assert_eq!(
        run_program(&format!("examples/empty.str")),
        (E::unit_typed())
    );
}

#[test]
fn an_int() {
    assert_eq!(
        run_program("examples/an_int.str"),
        (E {
            data: IntLiteral("42".to_string()),
            type_: BuiltinInt.untyped().boxed().some(),
        })
    );
}

#[test]
fn a_string() {
    use crate::ast::ExpressionData::*;

    assert_eq!(
        run_program("examples/a_string.str"),
        (E {
            data: StringLiteral("Hello there!".to_string()),
            type_: BuiltinString.untyped().boxed().some(),
        })
    );
}

#[test]
fn a_fun() {
    assert_eq!(
        run_program("examples/a_fun.str"),
        E {
            data: Fun {
                args: [(Ident::Plain("x".to_string()), BuiltinInt.untyped())].to_vec(),
                return_type: BuiltinInt.untyped().boxed().some(),
                body: E {
                    data: Block {
                        statements: [Stmt::Expression(E {
                            data: Identifier("x".to_string()),
                            type_: BuiltinInt.untyped().boxed().some()
                        })]
                        .to_vec(),
                        flatten: false
                    },
                    type_: BuiltinInt.untyped().boxed().some()
                }
                .boxed(),
                context: HashMap::new(),
            },
            type_: E {
                data: FunType {
                    args: [BuiltinInt.untyped()].to_vec(),
                    return_type: BuiltinInt.untyped().boxed().some(),
                },
                type_: None
            }
            .boxed()
            .some()
        }
    );
}

#[test]
fn id() {
    let expected = value!(
        . (s "hello") (.Things (s "abc") (i 42))
    );

    assert_value_eq(run_program("examples/id.str"), expected);
}

#[test]
#[should_panic(expected = "assertion failed: type_.data.is_type(self)")]
fn id_not_a_type() {
    run_program("examples/id_not_a_type.str");
}

#[test]
#[should_panic(expected = "type error: $Int is not a subtype of $String")]
fn id_mismatched_types() {
    run_program("examples/id_mismatched_types.str");
}

#[test]
fn scopes() {
    assert_value_eq(run_program("examples/scopes.str"), value!(.));
}

#[test]
fn stage_scoping() {
    assert_value_eq(run_program("examples/stage_scoping.str"), value!(.));
}

#[test]
#[should_panic(expected = "type error: unknown variable z")]
fn unknown_variable() {
    run_program("examples/unknown_variable.str");
}

#[test]
fn quotes() {
    assert_value_eq(run_program("examples/quotes.str"), value!(.Result (i 42) (s "abc") ))
}

#[test]
fn dyn_typing() {
    assert_value_eq(run_program("examples/dyn_typing.str"), value!(.));
}

#[test]
fn hello_world() {
    assert_value_eq(run_program("examples/hello_world.str"), value!(.));
}
