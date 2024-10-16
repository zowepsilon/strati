use crate::run_program;

macro_rules! test_from_example {
    ($name:ident: $src:literal => $expected:expr) => {
        #[test]
        fn $name() {
            #[allow(unused)]
            use crate::ast::{Expression, ExpressionData, Statement};

            assert_eq!(run_program($src), $expected);
        }
    }
}

test_from_example!{ empty: "examples/empty.str" => Expression::unit_typed() }

test_from_example!{ an_int: "examples/an_int.str" => Expression {
    data: ExpressionData::IntLiteral("42".to_string()),
    type_: Some(Box::new(ExpressionData::BuiltinInt.untyped())),
}}

test_from_example!{ a_string: "examples/a_string.str" => Expression {
    data: ExpressionData::StringLiteral("Hello there!".to_string()),
    type_: Some(Box::new(ExpressionData::BuiltinString.untyped())),
}}
