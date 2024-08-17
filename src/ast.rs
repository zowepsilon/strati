#[derive(Debug)]
pub struct Program {
    pub root: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntLiteral(String),
    StringLiteral(String),
    Identifier(String),
    Constructor {
        name: Option<String>,
        data: Vec<Expression>,
    },
    Fun {
        args: Vec<(String, Expression)>,
        return_type: Option<Box<Expression>>,
        body: Box<Expression>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Block {
        statements: Vec<Statement>
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        variable: String,
        annotation: Option<Expression>,
        value: Box<Expression>,
    },
    Expression(Expression),
    Assign {
        name: String,
        value: Expression,
    }
}
