use std::collections::HashMap;

use crate::ast::{
    Expression as E,
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

#[test]
fn empty() {
    assert_eq!(
        run_program(&format!("examples/{}.str", stringify!(empty))),
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
#[should_panic]
fn id_not_a_type() {
    run_program("examples/id_not_a_type.str");
}

#[test]
#[should_panic]
fn id_mismatched_types() {
    run_program("examples/id_mismatched_types.str");
}
