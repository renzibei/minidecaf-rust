use crate::ast::*;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub struct IrProg<'a> {
    pub funcs: Vec<IrFunc<'a>>,
    pub global_vars: Vec<(&'a str, Result<i32, u32>)>,
}

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub name: &'a str,
    pub param_num: u32,
    pub var_num: u32,
    pub stmts: Vec<IrStmt>,
}

#[derive(Debug)]
pub enum IrStmt {
    Push(i32),
    Pop,
    FrameAddr(u32),
    Load,
    Store,
    Swap,
    Neg,
    Not,
    LNot,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Ret,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    Land,
    Lor,
    Label(u32),
    Beqz(u32),
    Bnez(u32),
    Br(u32),
    Call(u32),
    GlobalAddr(u32),
}

// #[derive(Debug)]
// pub struct IrStmts<'a> {
//     pub name: &'a str,
//     pub stmts: Vec<IrStmt>,
// }

fn have_same_params(f1: &Function, f2: &Function) -> bool {
    if &f1.params.len() != &f2.params.len() {
        return false;
    }
    for i in 0..f1.params.len() {

        match (&f1.params[i], &f2.params[i]) {
            (Declaration::SoleDecl(_type1, _name1, _dims1), Declaration::SoleDecl(_type2, _name2, _dims2)) => {
                if _type1 != _type2 || _dims1.len() > 0 || _dims2.len() > 0 {
                    return false;
                }
            },
            _ => {
                return false;
            }
        }
    }

    return true;
}



pub fn  ast2ir<'a>(p: &'a Program<'a>) -> IrProg<'a> {

    let (mut global_vars, mut temp_global_vars) = (HashMap::new(), Vec::new());
    for d in &p.global_vars {

        let (var_name, var_init, var_type, var_dims) = match d {
            Declaration::SoleDecl(_type, _name, _dims) => {
                (_name, Err(_dims.iter().product::<u32>()), *_type, _dims)
            },
            Declaration::FullDecl(_type, _name, _expr, _dims) => {
                let mut is_int = false;

                let (__name, __v) =
                if let Expression::Assignment(_assign) = _expr {
                    if let Assignment::Conditional(_cond) = _assign {
                        if let Conditional::LogicalOr(_lo_or) = _cond {
                            if let LogicalOr::LogicalAnd(_lo_and) = _lo_or {
                                if let LogicalAnd::Equality(_equal) = _lo_and {
                                    if let Equality::Relational(_rela) = _equal {
                                        if let Relational::Additive(_addi) = _rela {
                                            if let Additive::Multiplicative(_multi) = _addi {
                                                if let Multiplicative::Unary(_unary) = _multi {
                                                    if let Unary::PostFix(_post) = _unary {
                                                        if let PostFix::Primary(_primary) = &**_post {
                                                             if let Primary::Int(_x, _) = **_primary {
                                                                 if *_type == 0 {
                                                                     is_int = true;
                                                                     (_name, _x)
                                                                 }
                                                                 else {
                                                                     (_name, 0)
                                                                 }
                                                             }
                                                             else {
                                                                 (_name,0)
                                                             }
                                                        }
                                                        else {
                                                            let s1 = "";
                                                            (_name,0)
                                                        }
                                                    }
                                                    else {
                                                        let s2 = "";
                                                        (_name,0)
                                                    }
                                                }
                                                else {
                                                    let s = "";
                                                    (_name,0)
                                                }
                                            }
                                            else {
                                                let s = "";
                                                (_name,0)
                                            }
                                        }
                                        else {
                                            let s = "";
                                            (_name,0)
                                        }
                                    }
                                    else {
                                        let s = "";
                                        (_name,0)
                                    }
                                }
                                else {
                                    let s = "";
                                    (_name,0)
                                }
                            }
                            else {
                                let s = "";
                                (_name,0)
                            }
                        }
                        else {
                            let s = "";
                            (_name,0)
                        }
                    }
                    else {
                        let s = "";
                        (_name,0)
                    }
                }
                else {
                    let s = "";
                    (_name,0)
                };

                if !is_int {
                    panic!("Global variable can only be initialized with Int");
                }
                else {
                    (__name, Ok(__v), 0, _dims)
                }
            }

        };

        if global_vars.insert(*var_name, (global_vars.len() as u32, var_type, var_dims)).is_some() {
            panic!("global variable `{}` redefined in current context", var_name);
        }
        temp_global_vars.push((*var_name, var_init));
    }

    let (mut funcs , mut temp_funcs) = (HashMap::new(), Vec::new());
    for func in &p.funcs {
        if global_vars.contains_key(&func.name) {
            panic!("global variable name conflicts with function `{}`", func.name);
        }
        match funcs.entry(func.name) {
            Entry::Vacant(v) => {
                v.insert((temp_funcs.len() as u32, func));
                temp_funcs.push(get_ir_func(func, &funcs, &global_vars));
            }
            Entry::Occupied(o) => {
                let (prev_name, prev_func) = *o.get();
                if (prev_func.stmts.stmts.len() > 0 && func.stmts.stmts.len() > 0) || prev_func.ret != func.ret || !have_same_params(&prev_func, &func) {
                    panic!("conflict function definition `{}` in current context", func.name)
                }
                if func.stmts.stmts.len() > 0 {
                    temp_funcs[prev_name as usize] = get_ir_func(func, &funcs, &global_vars);
                }
            }
        }
    }
    IrProg { funcs: temp_funcs, global_vars: temp_global_vars }
}

// key:name, value: (address_index, type, dims)
type VarMap<'a> = HashMap<&'a str, (u32, u32, &'a Vec<u32>)>;
type FuncMap<'a> = HashMap<&'a str, (u32, &'a Function<'a>)>;

struct FuncContext<'a, 'b> {
    vars: Vec<VarMap<'a> >,
    global_vars: &'b VarMap<'a>,
    stmts: Vec<IrStmt>,
    loop_labels: Vec<(u32, u32)>,
    funcs: &'b FuncMap<'a>,
    var_num: u32,
    label_num: u32,
    ret: Type,
}

impl<'a> FuncContext<'a, '_> {
    // return is_global, index, type, dims
    fn lookup(&self, var_name: &str) -> (bool, u32, u32, &'a Vec<u32>) {
        for var_map in self.vars.iter().rev() {
            if let Some(x) = var_map.get(var_name) {
                return (false, x.0, x.1, x.2);
            }
        }

        if let Some(x) = self.global_vars.get(var_name) { return (true, x.0, x.1, x.2); }

        panic!("variable `{}` not defined", var_name)
    }

    fn get_label(&mut self) -> u32 {
        let ret_label = self.label_num;
        self.label_num += 1;
        return ret_label;
    }
}

fn get_ir_func<'a>(f: &Function<'a>, f_map: &FuncMap<'a>, global_vars: &VarMap<'a>) -> IrFunc<'a> {
    let mut ctx = FuncContext { vars: Vec::new(), global_vars, stmts: Vec::new(), loop_labels: Vec::new(), funcs: f_map, ret: f.ret, var_num: 0, label_num: 0 };

    ctx.vars.push(VarMap::new());
    for param in &f.params {
        if let Declaration::SoleDecl(_ty, _param_name, _dims) = param {
            assert!(_dims.is_empty(), "function parameter cannot be array");
            try_define_var(&mut ctx, _param_name, *_ty, _dims, 1);
        }
        else {
            panic!("how can?");
        }

    }


    get_items(&mut ctx, &f.stmts.stmts);
    match ctx.stmts.last() {
        Some(IrStmt::Ret) => {},
        _ => {
            ctx.stmts.push(IrStmt::Push(0));
            ctx.stmts.push(IrStmt::Ret);
        }
    }
    // for stmt in f.stmts.as_deref().unwrap_or(&[]) {
    //     get_stmt_stmts(&mut ctx, stmt);
    // }

    let param_count = f.params.len() as u32;
    IrFunc {name: f.name, param_num: param_count, var_num: ctx.var_num - param_count, stmts:ctx.stmts}
}

fn get_items<'a>(ctx: &mut FuncContext<'a, '_>, stmts: &'a Vec<BlockItem<'a>>) {
    for block_item in stmts {
        match block_item {
            BlockItem::Statement(stmt) => {
                get_stmt_stmts(ctx, &stmt);
            },
            BlockItem::Declaration(dela) => {
                get_decl_stmts(ctx, &dela);
            }
        }
    }
}

fn get_stmt_stmts<'a>(ctx: &mut FuncContext<'a, '_>, stmt:&'a Statement<'a>) {
    match stmt {
        Statement::Ret(e) => {
            let (_ty, _dims) = get_expr_stmts(ctx, e, true);
            if _ty != ctx.ret || !_dims.is_empty() {
                panic!("return type mismatch");
            }
            ctx.stmts.push(IrStmt::Ret);
        },
        Statement::Expression(e) => {
            get_expr_stmts(ctx, e, true);
            ctx.stmts.push(IrStmt::Pop);
        },
        Statement::IfStmt(e, _stmt) => {
            assert_int(get_expr_stmts(ctx, e, true));
            let end_label = ctx.get_label();
            ctx.stmts.push(IrStmt::Beqz(end_label));
            get_stmt_stmts(ctx, _stmt);
            ctx.stmts.push(IrStmt::Label(end_label));
        },
        Statement::IfElseStmt(e, _stmt1, _stmt2) => {
            assert_int(get_expr_stmts(ctx, e, true));
            let else_label = ctx.get_label();
            let end_label = ctx.get_label();
            ctx.stmts.push(IrStmt::Beqz(else_label));
            get_stmt_stmts(ctx, _stmt1);
            ctx.stmts.push(IrStmt::Br(end_label));
            ctx.stmts.push(IrStmt::Label(else_label));
            get_stmt_stmts(ctx, _stmt2);
            ctx.stmts.push(IrStmt::Label(end_label));
        },
        Statement::DoWhile{body:_body, condition:_condition} => {
            let begin_loop_label = ctx.get_label();
            let break_label = ctx.get_label();
            let continue_label = ctx.get_label();
            ctx.loop_labels.push((continue_label, break_label));
            ctx.stmts.push(IrStmt::Label(begin_loop_label));
            get_stmt_stmts(ctx, _body);
            ctx.stmts.push(IrStmt::Label(break_label));
            assert_int(get_expr_stmts(ctx, _condition, true));
            ctx.stmts.push(IrStmt::Bnez(begin_loop_label));
            ctx.stmts.push(IrStmt::Label(continue_label));
            ctx.loop_labels.pop();

        },

        Statement::For {condition:_condition, update:_update, body:_body} => {
            let begin_loop_label = ctx.get_label();
            let break_label = ctx.get_label();
            let continue_label = ctx.get_label();
            ctx.loop_labels.push((continue_label, break_label));
            ctx.stmts.push(IrStmt::Label(begin_loop_label));
            if let Some(_condition) = _condition {
                assert_int(get_expr_stmts(ctx, _condition, true));
                ctx.stmts.push(IrStmt::Beqz(continue_label));
            }
            get_stmt_stmts(ctx, _body);
            ctx.stmts.push(IrStmt::Label(break_label));
            if let Some(_update) = _update {
                get_expr_stmts(ctx, _update, true);
                ctx.stmts.push(IrStmt::Pop);
            }
            ctx.stmts.push(IrStmt::Br(begin_loop_label));
            ctx.stmts.push(IrStmt::Label(continue_label));
            ctx.loop_labels.pop();
        },
        Statement::Break => {
            ctx.stmts.push(IrStmt::Br(ctx.loop_labels.last().expect("break").0))
        }
        ,
        Statement::Continue => {
            ctx.stmts.push(IrStmt::Br(ctx.loop_labels.last().expect("continue").1))
        }
        ,
        Statement::Block(_block) => {
            ctx.vars.push(VarMap::new());
            get_items(ctx, &_block.stmts);
            ctx.vars.pop();
        }
        Statement::Empty => {

        }
        // Statement::Declaration(d) => {
        //     get_decl_stmts(ctx, d);
        // }
    }
}

fn try_define_var<'a>(ctx: &mut FuncContext<'a, '_>, name: &'a str, _ty: Type, dims: &'a Vec<u32>, new_size: u32) {
    let now_var_num = ctx.var_num + new_size - 1;
    if ctx.vars.last_mut().unwrap().insert(name, (now_var_num, _ty, dims)).is_some() {
        panic!("variable {} already defined ", name);
    }
    ctx.var_num = now_var_num + 1;
}

fn get_decl_stmts<'a>(ctx: &mut FuncContext<'a, '_>, dela:&'a Declaration<'a>) {
    let mut now_var_num = ctx.var_num;
    let mut should_init = false;
    match dela {
        Declaration::SoleDecl(_ty, _name, _dims) => {
            let new_size = _dims.iter().product::<u32>();
            now_var_num += new_size - 1;
            try_define_var(ctx, _name, *_ty, _dims, new_size);
            if _dims.is_empty() {
                ctx.stmts.push(IrStmt::Push(0));
                should_init = true;
            }

        },
        Declaration::FullDecl(_ty, _name, _e, _dims) => {
            try_define_var(ctx, _name, *_ty, _dims, 1);
            let (_ty2, temp_dims) = get_expr_stmts(ctx, _e, true);
            if *_ty != _ty2 {
                panic!("Type mismatch when declaration assignment");
            }
            assert!(temp_dims.is_empty(), "can not assign array in declaration");
            should_init = true;
        }
    }
    if should_init {
        ctx.stmts.push(IrStmt::FrameAddr(now_var_num));
        ctx.stmts.push(IrStmt::Store);
        ctx.stmts.push(IrStmt::Pop);
    }
}

// fn get_ir_stmts<'a>(f: &Function<'a>) -> Vec<IrStmt> {
//
//     return stmts;
// }

fn get_lor_stmts<'a>(ctx: &mut FuncContext<'a, '_>, lor: &LogicalOr, should_load: bool) -> (Type, &'a [u32]) {
    match lor {
        LogicalOr::LogicalAnd(_land) => get_land_stmts(ctx, _land, should_load),
        LogicalOr::LogicalOrOp(op, _lor, _land) => {
            let (_ty1, _dims1) = get_lor_stmts(ctx, _lor, should_load);
            let (_ty2, _dims2) = get_land_stmts(ctx, _land, should_load);
            if _ty1 != _ty2 || _ty1 != 0 || !_dims1.is_empty() || !_dims2.is_empty() {
                panic!("Type mismatch in lor");
            }
            ctx.stmts.push(IrStmt::Lor);
            (0, &[])
        }
    }
}

fn get_land_stmts<'a>(ctx: &mut FuncContext<'a, '_>, land: &LogicalAnd, should_load: bool) -> (Type, &'a [u32]) {
    match land {
        LogicalAnd::Equality(_eq) => get_equal_stmts(ctx, _eq, should_load),
        LogicalAnd::LogicalAndOp(op, _land, _eq) => {
            let (_ty1, _dims1) = get_land_stmts(ctx, _land, should_load);
            let (_ty2, _dims2) = get_equal_stmts(ctx, _eq, should_load);
            if _ty1 != _ty2 || _ty1 != 0 || !_dims1.is_empty() || !_dims2.is_empty() {
                panic!("Type check failed in land");
            }
            ctx.stmts.push(IrStmt::Land);
            (0, &[])
        }
    }
}

fn get_equal_stmts<'a>(ctx: &mut FuncContext<'a, '_>, eq: &Equality, should_load: bool) -> (Type, &'a [u32]) {
    match eq {
        Equality::Relational(_r) => get_rela_stmts(ctx, _r, should_load),
        Equality::EqualityOp(op, _eq, _r) => {
            let (_ty1, _dims1) = get_equal_stmts(ctx, _eq, should_load);
            let (_ty2, _dims2) = get_rela_stmts(ctx, _r, should_load);
            if _ty1 != _ty2 || !_dims1.is_empty() || !_dims2.is_empty() {
                panic!("Type check failed in equality");
            }
            match op {
                EqualityOp::Eq => ctx.stmts.push(IrStmt::Eq),
                EqualityOp::Ne => ctx.stmts.push(IrStmt::Ne),
            }
            (0, &[])
        }
    }
}

fn get_rela_stmts<'a>(ctx: &mut FuncContext<'a, '_>, r: &Relational, should_load: bool) -> (Type, &'a [u32]) {
    match r {
        Relational::Additive(_a) => get_add_stmts(ctx, _a, should_load),
        Relational::RelationalOp(op, _r, _a) => {
            let (_ty1, _dims1) = get_rela_stmts(ctx, _r, should_load);
            let (_ty2, _dims2) = get_add_stmts(ctx, _a, should_load);
            if _ty1 != _ty2 || _ty1 != 0 || !_dims1.is_empty() || !_dims2.is_empty() {
                panic!("Type check failed in relational");
            }
            match op {
                RelationalOp::Lt => ctx.stmts.push(IrStmt::Lt),
                RelationalOp::Gt => ctx.stmts.push(IrStmt::Gt),
                RelationalOp::Le => ctx.stmts.push(IrStmt::Le),
                RelationalOp::Ge => ctx.stmts.push(IrStmt::Ge),
            }
            (0, &[])
        }
    }
}

fn get_add_stmts<'a>(ctx: &mut FuncContext<'a, '_>, a: &Additive, should_load: bool) -> (Type, &'a [u32]) {
    match a {
        Additive::AdditiveOp(op, _a, _m) => {

            let (_ty1, _dims1) = get_add_stmts(ctx, _a, should_load);
            let (_ty2, _dims2) = get_mul_stmts(ctx, _m, should_load);
            assert!(_dims1.is_empty() && _dims2.is_empty());

            if _ty1 == 0 && _ty2 == 0 {
                match op {
                    AdditiveOp::Add => ctx.stmts.push(IrStmt::Add),
                    AdditiveOp::Sub => ctx.stmts.push(IrStmt::Sub),
                }
                (0, &[])
            }
            else if _ty2 == 0 || (_ty1 == 0 && *op == AdditiveOp::Add) {
                if _ty1 == 0 {
                    ctx.stmts.push(IrStmt::Swap);
                }
                ctx.stmts.push(IrStmt::Push(4));
                ctx.stmts.push(IrStmt::Mul);
                match op {
                    AdditiveOp::Add => ctx.stmts.push(IrStmt::Add),
                    AdditiveOp::Sub => ctx.stmts.push(IrStmt::Sub),
                }
                if _ty1 != 0 {
                    (_ty1, &[])
                }
                else {
                    (_ty2, &[])
                }
            }
            else if _ty1 == _ty2 && *op == AdditiveOp::Sub {
                match op {
                    AdditiveOp::Add => ctx.stmts.push(IrStmt::Add),
                    AdditiveOp::Sub => ctx.stmts.push(IrStmt::Sub),
                }
                ctx.stmts.push(IrStmt::Push(4));
                ctx.stmts.push(IrStmt::Div);
                (0, &[])
            }
            else {
                panic!("type mismatch in add/sub");
            }


        }
        Additive::Multiplicative(_m) => {
            get_mul_stmts(ctx, _m, should_load)
        }
    }
}

fn get_mul_stmts<'a>(ctx: &mut FuncContext<'a, '_>, m: &Multiplicative, should_load: bool) -> (Type, &'a [u32]) {
    match m {
        Multiplicative::Unary(_u) => {
            get_unary_stmts(ctx, _u, should_load)
        },
        Multiplicative::MultiplicativeOp(op, _m, _u) => {
            let (_ty1, _dims1) = get_mul_stmts(ctx, _m, should_load);
            let (_ty2, _dims2) = get_unary_stmts(ctx, _u, true);
            assert!(_dims1.is_empty() && _dims2.is_empty());
            if _ty1 != _ty2 || _ty1 != 0 {
                panic!("type check failed in multiplicative");
            }
            match op {
                MultiplicativeOp::Mul => ctx.stmts.push(IrStmt::Mul),
                MultiplicativeOp::Div => ctx.stmts.push(IrStmt::Div),
                MultiplicativeOp::Mod => ctx.stmts.push(IrStmt::Mod),
            }
            (0, &[])
        }
    }
}

fn is_expr_lvalue(expr: &Expression) -> bool {
    if let Expression::Assignment(_assign) = expr {
        if let Assignment::Conditional(_condition) = _assign {
            if let Conditional::LogicalOr(_lor) = _condition {
                if let LogicalOr::LogicalAnd(_land) = _lor {
                    if let LogicalAnd::Equality(_equality) = _land {
                        if let Equality::Relational(_rela) = _equality {
                            if let Relational::Additive(_additive) = _rela {
                                if let Additive::Multiplicative(_multi) = _additive {
                                    if let Multiplicative::Unary(_un) = _multi {
                                        return is_lvalue(_un)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return false;
}

fn is_lvalue(u: &Unary) -> bool {
    match u {
        Unary::UnaryOp(op, _u) => {
            if *op == UnaryOp::DeRef {
                true
            }
            else {
                false
            }
        },
        Unary::PostFix(_post) => {
            match &**_post {
                PostFix::Primary(_prim) => {
                    match &**_prim {
                        Primary::Identifier(_name) => true,
                        Primary::Expression(_expr) => is_expr_lvalue(&_expr),
                        _ => false
                    }
                },
                PostFix::Index(_index, _expr) => {
                    true
                }
                _ => false
            }
        },
        _ => false
    }
}

fn assert_lvalue(u: &Unary) {
    let lvalue_flag = is_lvalue(u);
    if !lvalue_flag {
        panic!("expect lvalue");
    }
}

fn assert_int((ty, dims): (Type, &[u32])) {assert!(ty == 0 && dims.is_empty(), "expect int type but get {}", ty);}



fn get_unary_stmts<'a>(ctx: &mut FuncContext<'a, '_>, u: &Unary, should_load: bool) -> (Type, &'a [u32]) {
    match u {
        Unary::PostFix(_p) => get_postfix_stmts(ctx, _p, should_load),
        Unary::UnaryOp(op, _u) => {

            match op {
                UnaryOp::Neg => {
                    assert_int(get_unary_stmts(ctx, _u, true));
                    ctx.stmts.push(IrStmt::Neg);
                    (0, &[])
                },
                UnaryOp::BNot => {
                    assert_int(get_unary_stmts(ctx, _u, true));
                    ctx.stmts.push(IrStmt::Not);
                    (0, &[])
                },
                UnaryOp::LNot => {
                    assert_int(get_unary_stmts(ctx, _u, true));
                    ctx.stmts.push(IrStmt::LNot);
                    (0, &[])
                },
                UnaryOp::AddrOf => {
                    assert_lvalue(_u);
                    let (_ty, _dims) = get_unary_stmts(ctx, _u, false);
                    assert!(_dims.is_empty(), "cannot get address of array");
                    (_ty + 1, &[])
                },
                UnaryOp::DeRef => {
                    let (_ty, _dims) = get_unary_stmts(ctx, _u, true);
                    assert!(_ty > 0 || _dims.is_empty(), "expect pointer type in deref");
                    if should_load {
                        ctx.stmts.push(IrStmt::Load);
                    }
                    (_ty - 1, &[])
                },
            }
        },
        Unary::Cast(_type, _u) => {
            get_unary_stmts(ctx, _u, true);
            // if _ty != *_type {
            //     panic!("Failed to cast type from {} to {}" , _ty , _type);
            // }
            (*_type, &[])
        }
    }
}

fn get_decl_type(decl: &Declaration) -> Type {
    match decl {
        Declaration::SoleDecl(_type, _name, _dims) => *_type,
        Declaration::FullDecl(_type, _name, _expr, _dims) => *_type,
    }
}

fn get_postfix_stmts<'a>(ctx: &mut FuncContext<'a, '_>, _post: &PostFix, should_load: bool) -> (Type, &'a [u32]) {
    return match _post {
        PostFix::Primary(_p) => get_primary_stmts(ctx, _p, should_load),
        PostFix::Call(func, args) => {
            let (id, f) = *ctx.funcs.get(func).expect("Can not find function");
            assert_eq!(args.len(), f.params.len(), "function call args len not match");
            for (a, p) in args.iter().zip(f.params.iter()) {
                let (temp_ty, temp_dims) = get_expr_stmts(ctx, a, should_load);
                if temp_ty != get_decl_type(p) || !temp_dims.is_empty() {
                    panic!("function call type mismatch");
                }
                assert_eq!(temp_dims.len(), 0, "function return type cannot be array");
            }
            ctx.stmts.push(IrStmt::Call(id));
            (f.ret, &[])
        },
        PostFix::Index(_pointer, _index) => {
            let (_ty, _dims) = get_postfix_stmts(ctx, _pointer, true);
            assert!(_ty as u32 > 0 || _dims.len() > 0, "Index expect pointer or array");
            assert_int(get_expr_stmts(ctx, _index, true));
            ctx.stmts.push(IrStmt::Push((4 * _dims.iter().skip(1).product::<u32>()) as i32));
            ctx.stmts.push(IrStmt::Mul);
            ctx.stmts.push(IrStmt::Add);
            if should_load && _dims.len() <= 1 {ctx.stmts.push(IrStmt::Load);}
            if let Some(temp_dims) = _dims.get(1..) {
                (_ty, temp_dims)
            }
            else {
                (_ty - 1, _dims)
            }
        }
    }
}

fn get_primary_stmts<'a>(ctx: &mut FuncContext<'a, '_>, p: &Primary, should_load: bool) -> (Type, &'a [u32]) {

    return match p {
        Primary::Int(x, _) => {ctx.stmts.push(IrStmt::Push(*x)); (0, &[]) },
        Primary::Expression(_e) => get_expr_stmts(ctx, _e, should_load),
        Primary::Identifier(_name) => {
            let (is_global, var_id, _ty, _dims) = ctx.lookup(_name);
            if is_global {
                ctx.stmts.push(IrStmt::GlobalAddr(var_id));
            }
            else {
                ctx.stmts.push(IrStmt::FrameAddr(var_id));
            }
            if should_load && _dims.is_empty() {
                ctx.stmts.push(IrStmt::Load);
            }
            (_ty, &_dims)
        }
    }
}

fn get_assign_stmts<'a>(ctx: &mut FuncContext<'a, '_>, a: &Assignment, should_load: bool) -> (Type, &'a [u32]) {
    match a {
        Assignment::Conditional(_con) => get_conditional_stmts(ctx, _con, should_load),
        Assignment::Assign(_un, _expr) => {
            assert_lvalue(_un);
            let (_ty1, _dims1) = get_expr_stmts(ctx, _expr, should_load);
            let (_ty2, _dims2) = get_unary_stmts(ctx, _un, false);
            if _ty1 != _ty2 || !_dims1.is_empty() || !_dims2.is_empty() {
                panic!("Failed to assign, expect type {} get {} " , _ty2 , _ty1);
            }

            ctx.stmts.push(IrStmt::Store);
            (_ty1, &[])
        }
    }
}

fn get_conditional_stmts<'a>(ctx: &mut FuncContext<'a, '_>, c: &Conditional, should_load: bool) -> (Type, &'a [u32]) {
    match c {
        Conditional::LogicalOr(_lor) => get_lor_stmts(ctx, _lor, should_load),
        Conditional::Conditional(_lor, _expr, _con) => {
            assert_int(get_lor_stmts(ctx, _lor, should_load));
            let else_label = ctx.get_label();
            let end_label = ctx.get_label();
            ctx.stmts.push(IrStmt::Beqz(else_label));
            let (_ty1, _dims1) = get_expr_stmts(ctx, _expr, should_load);
            ctx.stmts.push(IrStmt::Br(end_label));
            ctx.stmts.push(IrStmt::Label(else_label));
            let (_ty2, _dims2) = get_conditional_stmts(ctx, _con, should_load);
            if _ty1 != _ty2 || !_dims1.is_empty() || !_dims2.is_empty() {
                panic!("Type mismatch in conditional statement");
            }
            ctx.stmts.push(IrStmt::Label(end_label));
            (_ty1, &[])
        }
    }
}

fn get_expr_stmts<'a>(ctx: &mut FuncContext<'a, '_>, e: &Expression, should_load: bool) -> (Type, &'a [u32]) {
    match e {
        Expression::Assignment(_a) => get_assign_stmts(ctx, _a, should_load),

    }
}