pub type Program = Vec<LetDeclaration>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Untyped,
    Unit,
    Int,
    String,
    Struct {
        fields: Vec<(String, Type)>, // TODO: change to HashMap
    },
    Fun {
        args: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Unit,
    IntLiteral(String),
    StringLiteral(String),
    Identifier(String),
    Fun {
        args: Vec<(String, Type)>,
        return_type: Type,
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
pub struct Expression {
    pub kind: ExpressionKind,
    pub ty: Type,
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
    pub annotation: Option<Type>,
    pub value: Box<Expression>,
}

impl From<ExpressionKind> for Expression {
    fn from(kind: ExpressionKind) -> Self {
        match kind {
            | ExpressionKind::Unit => Expression { kind, ty: Type::Unit },
            | ExpressionKind::IntLiteral(_) => Expression { kind, ty: Type::Int },
            | ExpressionKind::StringLiteral(_) => Expression { kind, ty: Type::String },
            | _ => Expression { kind, ty: Type::Untyped }
        }
    }
}
