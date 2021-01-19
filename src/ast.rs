#[derive(Debug)]
pub struct Program<'a> {
    pub funcs: Vec<Function<'a>>,
    pub global_vars: Vec<Declaration<'a>>,
}

pub type Type = u32;

#[derive(Debug)]
pub struct Function<'a> {
    pub ret: Type,
    pub name: &'a str,
    pub params: Vec<Declaration<'a>>,
    pub stmts: Block<'a>,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Statement(Statement<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<BlockItem<'a> >,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Ret(Expression<'a>),
    Expression(Expression<'a>),
    // Declaration(Declaration<'a>),
    IfStmt(Box<Expression<'a>>, Box<Statement<'a>>),
    IfElseStmt(Box<Expression<'a>>, Box<Statement<'a>>, Box<Statement<'a>>),
    Block(Block<'a>),
    For{condition: Option<Expression<'a>>, update: Option<Expression<'a>>, body: Box<Statement<'a>>},
    DoWhile{body: Box<Statement<'a>>, condition: Expression<'a>},
    Continue,
    Break,
    Empty,
}


#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum UnaryOp {Neg, BNot, LNot, AddrOf, DeRef}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum AdditiveOp { Add, Sub }

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum MultiplicativeOp {Mul, Div, Mod}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum RelationalOp { Lt, Gt, Le, Ge }

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum EqualityOp { Eq, Ne }

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum LogicalAndOp {And}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum LogicalOrOp {Or}

#[derive(Debug)]
pub enum Unary<'a> {
    PostFix(Box<PostFix<'a>>),
    UnaryOp(UnaryOp, Box<Unary<'a>>),
    Cast(Type, Box<Unary<'a>>),
}

#[derive(Debug)]
pub enum PostFix<'a> {
    Primary(Box<Primary<'a>>),
    Call(&'a str, Vec<Expression<'a>>),
    Index(Box<PostFix<'a>>, Expression<'a>),
}

#[derive(Debug)]
pub enum Primary<'a> {
    Int(i32, std::marker::PhantomData<&'a()>),
    Expression(Expression<'a>),
    Identifier(&'a str),
}

#[derive(Debug)]
pub enum Multiplicative<'a> {
    Unary(Unary<'a>),
    MultiplicativeOp(MultiplicativeOp, Box<Multiplicative<'a>>, Box<Unary<'a>>)
}

#[derive(Debug)]
pub enum Additive<'a> {
    Multiplicative(Multiplicative<'a>),
    AdditiveOp(AdditiveOp, Box<Additive<'a>>, Box<Multiplicative<'a>>)
}

#[derive(Debug)]
pub enum Relational<'a> {
    Additive(Additive<'a>),
    RelationalOp(RelationalOp, Box<Relational<'a>>, Box<Additive<'a>>)
}

#[derive(Debug)]
pub enum Equality<'a> {
    Relational(Relational<'a>),
    EqualityOp(EqualityOp, Box<Equality<'a>>, Box<Relational<'a>>)
}

#[derive(Debug)]
pub enum LogicalAnd<'a> {
    Equality(Equality<'a>),
    LogicalAndOp(LogicalAndOp, Box<LogicalAnd<'a>>, Box<Equality<'a>>)
}

#[derive(Debug)]
pub enum LogicalOr<'a> {
    LogicalAnd(LogicalAnd<'a>),
    LogicalOrOp(LogicalOrOp, Box<LogicalOr<'a>>, Box<LogicalAnd<'a>>)
}

#[derive(Debug)]
pub enum Assignment<'a> {
    Conditional(Conditional<'a>),
    Assign(Unary<'a>, Box<Expression<'a>>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    Assignment(Assignment<'a>),
}

#[derive(Debug)]
pub enum Declaration<'a> {
    SoleDecl(Type, &'a str, Vec<u32>),
    FullDecl(Type, &'a str, Expression<'a>, Vec<u32>),
}

#[derive(Debug)]
pub enum Conditional<'a> {
    LogicalOr(LogicalOr<'a>),
    Conditional(LogicalOr<'a>, Box<Expression<'a>>, Box<Conditional<'a>>),
}






