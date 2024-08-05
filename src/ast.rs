#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<LetDeclaration>,
}

impl Program {
    pub fn new(declarations: Vec<LetDeclaration>) -> Program {
        Program { declarations, }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Unit,
    IntLiteral(String),
    StringLiteral(String),
    Identifier(String),
    Fun {
        args: Vec<(String, Expression)>,
        return_type: Box<Expression>,
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
    Let(LetDeclaration),
    Expression(Expression),
    Assign {
        name: String,
        value: Expression,
    }
}

#[derive(Debug, Clone)]
pub struct LetDeclaration {
    pub variable: String,
    pub annotation: Option<Expression>,
    pub value: Box<Expression>,
}
