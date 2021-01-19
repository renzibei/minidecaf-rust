use std::io::{Result, Write};
use crate::ir::*;

pub fn ir2assembly(ir_prog: &IrProg, output: &mut impl Write) -> Result<()> {

    for var in &ir_prog.global_vars {


        writeln!(output, ".data")?;
        writeln!(output, "{}:", var.0)?;
        match var.1 {
            Ok(init_value) => writeln!(output, "  .word {}", init_value)?,
            Err(size) => writeln!(output, "  .zero {}", size * 4)?,
        }

        writeln!(output)?;
    }

    writeln!(output, ".text")?;
    for ir_func in &ir_prog.funcs {
        writeln!(output, ".global {}", ir_func.name)?;
        writeln!(output, "{}:", ir_func.name)?;



        writeln!(output, " sw s0, -{}(sp)", (ir_func.var_num + 1) * 4)?;
        writeln!(output, " sw ra, -{}(sp)", (ir_func.var_num + 2) * 4)?;
        writeln!(output, " add s0, sp, {}", ir_func.param_num * 4)?;
        writeln!(output, " add sp, sp, -{}", (ir_func.var_num + 2) * 4)?;

        for stmt in &ir_func.stmts {
            writeln!(output, " # {:?}", stmt)?;
            match stmt {
                IrStmt::Push(x) => {
                    writeln!(output, " addi sp, sp, -4")?;
                    writeln!(output, " li t1, {}", x)?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Pop => {
                    writeln!(output, " addi sp, sp, 4")?;
                },
                IrStmt::FrameAddr(k) => {
                    writeln!(output, " addi sp, sp, -4")?;
                    writeln!(output, " addi t1, s0, -{}", 4 + 4 * k)?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Load => {
                    writeln!(output, " lw t1, 0(sp)")?;
                    writeln!(output, " lw t1, 0(t1)")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Store => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(t2)")?;
                },
                IrStmt::Swap => {
                    writeln!(output, " lw t0, 0(sp)")?;
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " sw t0, 4(sp)")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Ret => {
                    writeln!(output, " lw a0, 0(sp)")?;
                    writeln!(output, " mv sp, s0")?;
                    writeln!(output, " lw s0, -{}(sp)", (ir_func.param_num + ir_func.var_num + 1) * 4)?;
                    writeln!(output, " lw ra, -{}(sp)", (ir_func.param_num + ir_func.var_num + 2) * 4)?;
                    writeln!(output, " ret")?;
                },
                IrStmt::Neg => {
                    writeln!(output, " lw t1, 0(sp)")?;
                    writeln!(output, " neg t1, t1")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Not => {
                    writeln!(output, " lw t1, 0(sp)")?;
                    writeln!(output, " not t1, t1")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                }
                ,
                IrStmt::LNot => {
                    writeln!(output, " lw t1, 0(sp)")?;
                    writeln!(output, " seqz t1, t1")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Add => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " add t1, t1, t2")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Sub => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " sub t1, t1, t2")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Mul => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " mul t1, t1, t2")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Div => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " div t1, t1, t2")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Mod => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " rem t1, t1, t2")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Eq => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " xor t1, t1, t2")?;
                    writeln!(output, " seqz t1, t1")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Ne => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " xor t1, t1, t2")?;
                    writeln!(output, " snez t1, t1")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Le => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " slt t1, t2, t1")?;
                    writeln!(output, " xor t1, t1, 1")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Ge => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " slt t1, t1, t2")?;
                    writeln!(output, " xor t1, t1, 1")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Lt => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " slt t1, t1, t2")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Gt => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " slt t1, t2, t1")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Land => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " snez t1,t1")?;
                    writeln!(output, " snez t2,t2")?;
                    writeln!(output, " and t1,t1,t2")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Lor => {
                    writeln!(output, " lw t1, 4(sp)")?;
                    writeln!(output, " lw t2, 0(sp)")?;
                    writeln!(output, " or t1,t1,t2")?;
                    writeln!(output, " snez t1,t1")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " sw t1, 0(sp)")?;
                },
                IrStmt::Label(x) => {
                    writeln!(output, "Label_{}_{}:", ir_func.name, x)?;
                },
                IrStmt::Br(x) => {
                    writeln!(output, " j Label_{}_{}", ir_func.name, x)?;
                },
                IrStmt::Beqz(x) => {
                    writeln!(output, " lw t1, 0(sp)")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " beqz t1, Label_{}_{}", ir_func.name, x)?;
                },
                IrStmt::Bnez(x) => {
                    writeln!(output, " lw t1, 0(sp)")?;
                    writeln!(output, " addi sp, sp, 4")?;
                    writeln!(output, " bnez t1, Label_{}_{}", ir_func.name, x)?;
                },
                IrStmt::Call(x) => {
                    writeln!(output, " jal {}", ir_prog.funcs[*x as usize].name)?;
                    writeln!(output, " sw a0, -4(sp)")?;
                    writeln!(output, " add sp, sp, -4")?;
                },
                IrStmt::GlobalAddr(x) => {
                    writeln!(output, " add sp, sp, -4")?;
                    writeln!(output, " la t1, {}", ir_prog.global_vars[*x as usize].0)?;
                    writeln!(output, " sw t1, 0(sp)")?;

                }
            }
        }
    }


    Ok(())
}