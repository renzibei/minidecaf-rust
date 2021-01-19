use crate::ast::{*, UnaryOp::*, AdditiveOp::*, MultiplicativeOp::*, RelationalOp::*, EqualityOp::*, LogicalOrOp::*, LogicalAndOp::*};


pub struct Parser {}

#[parser_macros::lalr1(program)]
#[lex = r#"
priority = []

[lexical]
'int' = 'Int'
'if' = 'If'
'else' = 'Else'
'for' = 'For'
'while' = 'While'
'do' = 'Do'
'break' = 'Break'
'continue' = 'Continue'
'\(' = 'Lparen'
'\)' = 'Rparen'
'\{' = 'Lbrace'
'\}' = 'Rbrace'
'\[' = 'Lbracket'
'\]' = 'Rbracket'
'=' = 'Assign'
'\+' = 'Add'
'-' = 'Sub'
'\*' = 'Mul'
'/' = 'Div'
'%' = 'Mod'
'<' = 'Lt'
'<=' = 'Le'
'>=' = 'Ge'
'>' = 'Gt'
'==' = 'Eq'
'!=' = 'Ne'
'&&' = 'And'
'\|\|' = 'Or'
'~' = 'BNot'
'!' = 'LNot'
'\?' = 'Question'
':' = 'Colon'
'return' = 'Return'
',' = 'Comma'
'&' = 'AddrOf'
';' = 'Semicolon'
'\s+' = '_Eps'
'\d+' = 'Integer'
'[a-zA-Z_][a-zA-Z0-9_]*' = 'Identifier'

"#]

impl<'p> Parser {

    #[rule = "program -> "]
    fn program_empty() -> Program<'p> { Program {funcs: Vec::new(), global_vars: Vec::new() }}

    #[rule = "program -> program function"]
    fn program_func(mut prog: Program<'p>, func: Function<'p>) -> Program<'p> { (prog.funcs.push(func), prog).1 }

    #[rule = "program -> program declaration"]
    fn program_globals(mut prog: Program<'p>, decl: Declaration<'p>) -> Program<'p> {(prog.global_vars.push(decl), prog).1}

    #[rule = "function -> type Identifier Lparen parameter_list Rparen block"]
    fn function_with_body(_type: Type, _id: Token, _lparen: Token, _params: Vec<Declaration<'p>> , _rparen: Token,  _stmts: Block<'p>) -> Function<'p> {
        Function { ret:_type, name: _id.str(), params: _params, stmts:_stmts }
    }

    #[rule = "function -> type Identifier Lparen parameter_list Rparen Semicolon"]
    fn function_empty_body(_type: Type, _id: Token, _lparen: Token, _params: Vec<Declaration<'p>> , _rparen: Token, _semi: Token) -> Function<'p> {
        Function { ret:_type, name: _id.str(), params: _params, stmts: Block{stmts: Vec::new()}}
    }

    #[rule = "type -> Int"]
    fn type_int(_int: Token) -> Type {0}

    #[rule = "type -> type Mul"]
    fn type_pointer(_type: Type, _mul:Token) -> Type {_type + 1}

    #[rule = "block -> Lbrace block_items Rbrace"]
    fn block_to_items(_lbrace: Token, _items: Vec<BlockItem<'p>>, _rbrace: Token) -> Block<'p> {Block{stmts:_items}}

    #[rule = "block_items ->"]
    fn empty_block_item() -> Vec<BlockItem<'p>> {Vec::new()}


    #[rule = "block_items -> block_items block_item"]
    fn block_items(mut _stmts: Vec<BlockItem<'p>>, _stmt: BlockItem<'p>) -> Vec<BlockItem<'p>> { (_stmts.push(_stmt), _stmts).1 }

    #[rule = "block_item -> statement"]
    fn block_item_statement(_stmt: Statement<'p>) -> BlockItem<'p> { BlockItem::Statement(_stmt) }

    #[rule = "block_item -> declaration"]
    fn block_item_declaration(_decl: Declaration<'p>) -> BlockItem<'p> { BlockItem::Declaration(_decl) }

    #[rule = "statement -> Return expression Semicolon"]
    fn statement_return(_return: Token, _expr: Expression<'p>, _semi: Token) -> Statement<'p> { Statement::Ret(_expr) }

    #[rule = "statement -> expression Semicolon"]
    fn statement_expression(_expr: Expression<'p>, _semi: Token) -> Statement<'p> { Statement::Expression(_expr) }

    #[rule = "statement -> If Lparen expression Rparen statement"]
    fn if_statement(_if: Token, _lparen: Token, _expr: Expression<'p>, _rparen: Token, _stmt: Statement<'p>) -> Statement<'p> {Statement::IfStmt(Box::new(_expr), Box::new(_stmt))}

    #[rule = "statement -> If Lparen expression Rparen statement Else statement"]
    fn if_else_statement(_if: Token, _lparen: Token, _expr: Expression<'p>, _rparen: Token, _stmt1: Statement<'p>, _else: Token, _stmt2: Statement<'p>) -> Statement<'p> {Statement::IfElseStmt(Box::new(_expr), Box::new(_stmt1), Box::new(_stmt2))}

    #[rule = "statement -> For Lparen maybe_expression Semicolon maybe_expression Semicolon maybe_expression Rparen statement"]
    fn for_loop1(_for: Token, _lparen: Token, _init: Option<Expression<'p>>, _semi1: Token, _condition: Option<Expression<'p>>, _semi2: Token, _update: Option<Expression<'p>>, _rparen: Token, _body: Statement<'p>)
        -> Statement<'p> {Statement::Block(Block{stmts:vec![_init.map_or(BlockItem::Statement(Statement::Empty), |x| BlockItem::Statement(Statement::Expression(x))),
                            BlockItem::Statement(Statement::For{condition:_condition, update: _update, body:Box::new(_body)}) ]})}

    #[rule = "statement -> For Lparen declaration maybe_expression Semicolon maybe_expression Rparen statement"]
    fn for_loop2(_for: Token, _lparen: Token, _init: Declaration<'p>,  _condition: Option<Expression<'p>>, _semi2: Token, _update: Option<Expression<'p>>, _rparen: Token, _body: Statement<'p>)
        -> Statement<'p> {Statement::Block(Block{stmts:vec![BlockItem::Declaration(_init), BlockItem::Statement(Statement::For {condition:_condition, update: _update, body: Box::new(_body) })]})}

    #[rule = "statement -> While Lparen expression Rparen statement"]
    fn while_loop(_while: Token, _lparen: Token, _condition: Expression<'p>, _rparen: Token, _body: Statement<'p>)
        -> Statement<'p> {Statement::For {condition:Some(_condition), update: None, body: Box::new(_body)}}

    #[rule = "statement -> Do statement While Lparen expression Rparen Semicolon"]
    fn do_while_loop(_do: Token, _body:Statement<'p>, _while: Token,  _lparen: Token, _condition: Expression<'p>, _rparen: Token, _semi: Token)
        -> Statement<'p> {Statement::DoWhile {body:Box::new(_body), condition:_condition} }

    #[rule = "statement -> block"]
    fn stmt_block(_block: Block<'p>) -> Statement<'p> { Statement::Block(_block) }

    #[rule = "statement -> Continue Semicolon"]
    fn stmt_continue(_continue: Token, _semi: Token) -> Statement<'p> { Statement::Continue}

    #[rule = "statement -> Break Semicolon"]
    fn stmt_break(_break: Token, _semi: Token) -> Statement<'p> { Statement::Break}

    #[rule = "statement -> Semicolon"]
    fn empty_statement(_semi: Token) -> Statement<'p> {Statement::Empty}

    // #[rule = "statement -> declaration"]
    // fn statement_declaration(_decl: Declaration<'p>) -> Statement<'p> { Statement::Declaration(_decl) }

    #[rule = "parameter_list -> params"]
    fn parameter_list_params(params: Vec<Declaration<'p>>) -> Vec<Declaration<'p>> { params }

    #[rule = "parameter_list -> "]
    fn parameter_list_empty() -> Vec<Declaration<'p>> { Vec::new() }

    #[rule = "params -> parameter"]
    fn params_one_param(param: Declaration<'p>) -> Vec<Declaration<'p>> { vec![param] }

    #[rule = "params -> params Comma parameter"]
    fn params_more(mut params: Vec<Declaration<'p>>, comma: Token, new_p: Declaration<'p>) -> Vec<Declaration<'p>> { (params.push(new_p), params).1 }

    #[rule = "parameter -> type Identifier"]
    fn single_parameter(_type: Type, _id: Token) -> Declaration<'p> { Declaration::SoleDecl(_type, _id.str(), Vec::new()) }

    #[rule = "declaration -> type Identifier Semicolon"]
    fn sole_declaration(_type: Type, name: Token, _semi: Token) -> Declaration<'p> { Declaration::SoleDecl(_type, name.str(), Vec::new())}

    #[rule = "declaration -> type Identifier dimensional Semicolon"]
    fn array_declaration(_type: Type, name: Token, dims: Vec<u32>, _semi: Token) -> Declaration<'p> { Declaration::SoleDecl(_type, name.str(), dims)}

    #[rule = "dimensional -> Lbracket Integer Rbracket"]
    fn dimensional1(_lbra: Token, _int: Token, _rbra: Token) -> Vec<u32> {vec![_int.parse()]}

    #[rule = "dimensional -> dimensional Lbracket Integer Rbracket"]
    fn dimensional2(mut _dim: Vec<u32>, _lbra: Token, _int: Token, _rbra: Token) ->  Vec<u32> {(_dim.push(_int.parse()), _dim).1}

    #[rule = "declaration -> type Identifier Assign expression Semicolon"]
    fn full_declaration(_type: Type, name: Token, _assign: Token, _expr: Expression<'p>, _semi: Token) -> Declaration<'p> { Declaration::FullDecl(_type, name.str(), _expr, Vec::new()) }

    #[rule = "expression_list -> "]
    fn expression_list_empty() -> Vec<Expression<'p>> {Vec::new()}

    #[rule = "expression_list -> expressions"]
    fn expression_list_expressions(_exprs: Vec<Expression<'p>>) -> Vec<Expression<'p>> { _exprs }

    #[rule = "expressions -> expression"]
    fn expressions_one_expression(_expr: Expression<'p>) -> Vec<Expression<'p>> { vec![_expr] }

    #[rule = "expressions -> expressions Comma expression"]
    fn expressions_expansion(mut _exprs: Vec<Expression<'p>>, _comma: Token, _expr: Expression<'p>) -> Vec<Expression<'p>> {(_exprs.push(_expr), _exprs).1}

    #[rule = "expression -> assignment"]
    fn expression(_assign: Assignment<'p>) -> Expression<'p> {Expression::Assignment(_assign)}

    #[rule = "maybe_expression -> "]
    fn empty_expression() -> Option<Expression<'p>> { None }

    #[rule = "maybe_expression -> expression"]
    fn not_empty_maybe_expression(_expr: Expression<'p>) -> Option<Expression<'p>> { Some(_expr) }

    #[rule = "assignment -> conditional"]
    fn assignment1(co: Conditional<'p>) -> Assignment<'p> {Assignment::Conditional(co)}

    #[rule = "assignment -> unary Assign expression"]
    fn assignment2(_unary: Unary<'p>, _a: Token, _expr: Expression<'p>) -> Assignment<'p> {Assignment::Assign(_unary, Box::new(_expr))}

    #[rule = "conditional -> logical_or"]
    fn conditional1(lo: LogicalOr<'p>) -> Conditional<'p> {Conditional::LogicalOr(lo) }

    #[rule = "conditional -> logical_or Question expression Colon conditional"]
    fn conditional2(lo: LogicalOr<'p>, _qu: Token, _expr: Expression<'p>, _colon: Token, _co: Conditional<'p>) -> Conditional<'p> {Conditional::Conditional(lo, Box::new(_expr), Box::new(_co))}


    #[rule = "logical_or -> logical_and"]
    fn or_and(la: LogicalAnd<'p>) -> LogicalOr<'p> {LogicalOr::LogicalAnd(la)}

    #[rule = "logical_or -> logical_or Or logical_and"]
    fn or2(lo: LogicalOr<'p>, _i:Token, la: LogicalAnd<'p>) -> LogicalOr<'p> {LogicalOr::LogicalOrOp(Or, Box::new(lo), Box::new(la))}

    #[rule = "logical_and -> equality"]
    fn and_eq(eq: Equality<'p>) -> LogicalAnd<'p> {LogicalAnd::Equality(eq)}

    #[rule = "logical_and -> logical_and And equality"]
    fn and2(la: LogicalAnd<'p>, _i:Token, eq: Equality<'p>) -> LogicalAnd<'p> {LogicalAnd::LogicalAndOp(And, Box::new(la), Box::new(eq))}

    #[rule = "equality -> relational"]
    fn eq_relational(r: Relational<'p>) -> Equality<'p> {Equality::Relational(r)}

    #[rule = "equality -> equality Eq relational"]
    fn equal1(eq: Equality<'p>, _i:Token, r: Relational<'p>) -> Equality<'p> {Equality::EqualityOp(Eq, Box::new(eq), Box::new(r))}

    #[rule = "equality -> equality Ne relational"]
    fn unequal1(eq: Equality<'p>, _i:Token, r:Relational<'p>) -> Equality<'p> {Equality::EqualityOp(Ne, Box::new(eq), Box::new(r))}

    #[rule = "relational -> relational Lt additive"]
    fn less_than(r:Relational<'p>, _i:Token, a: Additive<'p>) -> Relational<'p> {Relational::RelationalOp(Lt, Box::new(r), Box::new(a))}

    #[rule = "relational -> relational Gt additive"]
    fn greater_than(r:Relational<'p>, _i:Token, a: Additive<'p>) -> Relational<'p> {Relational::RelationalOp(Gt, Box::new(r), Box::new(a))}

    #[rule = "relational -> relational Le additive"]
    fn less_equal(r:Relational<'p>, _i:Token, a: Additive<'p>) -> Relational<'p> {Relational::RelationalOp(Le, Box::new(r), Box::new(a))}

    #[rule = "relational -> relational Ge additive"]
    fn greater_equal(r:Relational<'p>, _i:Token, a: Additive<'p>) -> Relational<'p> {Relational::RelationalOp(Ge, Box::new(r), Box::new(a))}

    #[rule = "relational -> additive"]
    fn rela_add(a:Additive<'p>) -> Relational<'p> {Relational::Additive(a)}

    #[rule = "additive -> multiplicative"]
    fn add_mul(m: Multiplicative<'p>) -> Additive<'p> {Additive::Multiplicative(m)}

    #[rule = "additive -> additive Add multiplicative"]
    fn add1(a: Additive<'p>, _i: Token, m: Multiplicative<'p>) -> Additive<'p> {Additive::AdditiveOp(Add, Box::new(a), Box::new(m))}

    #[rule = "additive -> additive Sub multiplicative"]
    fn sub1(a: Additive<'p>, _i: Token, m: Multiplicative<'p>) -> Additive<'p> {Additive::AdditiveOp(Sub, Box::new(a), Box::new(m))}

    #[rule = "multiplicative -> unary"]
    fn mul_unary(u: Unary<'p>) -> Multiplicative<'p> {Multiplicative::Unary(u)}

    #[rule = "multiplicative -> multiplicative Mul unary"]
    fn mul1(m: Multiplicative<'p>, _i: Token, u: Unary<'p>) -> Multiplicative<'p> {Multiplicative::MultiplicativeOp(Mul, Box::new(m), Box::new(u)) }

    #[rule = "multiplicative -> multiplicative Div unary"]
    fn div1(m: Multiplicative<'p>, _i: Token, u: Unary<'p>) -> Multiplicative<'p> {Multiplicative::MultiplicativeOp(Div, Box::new(m), Box::new(u)) }

    #[rule = "multiplicative -> multiplicative Mod unary"]
    fn mod1(m: Multiplicative<'p>, _i: Token, u: Unary<'p>) -> Multiplicative<'p> {Multiplicative::MultiplicativeOp(Mod, Box::new(m), Box::new(u)) }

    #[rule = "unary -> postfix"]
    fn unary_primary(p: PostFix<'p>) -> Unary<'p> {Unary::PostFix(Box::new(p))}

    #[rule = "unary -> Sub unary"]
    fn expression_neg(_i: Token, u: Unary<'p>) -> Unary<'p> { Unary::UnaryOp(Neg, Box::new(u) )}

    #[rule = "unary -> BNot unary"]
    fn expression_bnot(_i: Token, u: Unary<'p>) -> Unary<'p> { Unary::UnaryOp(BNot, Box::new(u) )}

    #[rule = "unary -> LNot unary"]
    fn expression_lnot(_i: Token, u: Unary<'p>) -> Unary<'p> { Unary::UnaryOp(LNot, Box::new(u) )}

    #[rule = "unary -> AddrOf unary"]
    fn expression_addrof(_i: Token, u: Unary<'p>) -> Unary<'p> { Unary::UnaryOp(AddrOf, Box::new(u) )}

    #[rule = "unary -> Mul unary"]
    fn expression_deref(_i: Token, u: Unary<'p>) -> Unary<'p> { Unary::UnaryOp(DeRef, Box::new(u))}

    #[rule = "unary -> Lparen type Rparen unary"]
    fn expression_cast(_lparen: Token, _type: Type, _rparen: Token, u: Unary<'p>) -> Unary<'p> { Unary::Cast(_type, Box::new(u))}

    #[rule = "postfix -> primary"]
    fn postfix_primary(p: Primary<'p>) -> PostFix<'p> {PostFix::Primary(Box::new(p))}

    #[rule = "postfix -> Identifier Lparen expression_list Rparen"]
    fn postfix_call(_id: Token, _lp: Token, _exprs: Vec<Expression<'p>>, _rp: Token) -> PostFix<'p> {PostFix::Call(_id.str(), _exprs)}

    #[rule = "postfix -> postfix Lbracket expression Rbracket"]
    fn postfix_index(_postfix: PostFix<'p>, _lbra: Token, _expr: Expression<'p>, _rbra: Token) -> PostFix<'p> {PostFix::Index(Box::new(_postfix), _expr)}

    #[rule = "primary -> Integer"]
    fn expression_int(_i: Token) -> Primary<'p> { Primary::Int(_i.parse(), std::marker::PhantomData) }

    #[rule = "primary -> Lparen expression Rparen"]
    fn primary_exp(_lparen: Token, e: Expression<'p>, _rparen: Token) -> Primary<'p> { Primary::Expression(e)}

    #[rule = "primary -> Identifier"]
    fn primary_id(_id: Token) -> Primary<'p> {Primary::Identifier(_id.str()) }


}

impl<'p> Token<'p> {
    fn str(&self) -> &'p str { std::str::from_utf8(self.piece).unwrap() }
    fn parse<T>(&self) -> T where T: std::str::FromStr, <T as std::str::FromStr>::Err: std::fmt::Debug {
        self.str().parse().expect("failed to parse")
    }
}