# Step3 Report

## 实验内容

step3增加了`+ - * / %`以及在表达式中的括号。

我的parser的规则定义与实验指导书的规范完全一致，在AST的定义上与[Mashplant的参考实现](https://github.com/decaf-lang/minidecaf/tree/mashplant)有一些差别。参考实现的优点在于使用了语法分析工具的优先级功能，实现更加简单，将二元操作等都视为Expr表达式，编码比较简单。我的代码没有使用词法分析工具的优先级功能，实现更繁琐。

## 思考题

1. 请给出将寄存器 `t0` 中的数值压入栈中所需的 riscv 汇编指令序列；请给出将栈顶的数值弹出到寄存器 `t0`中所需的 riscv 汇编指令序列。

将`t0`压入栈中的riscv汇编指令为

```assembly
addi sp, sp, -4
sw t0, 0(sp)
```

将栈顶数值弹到`t0`中的汇编指令为

```assembly
lw a0, 0(sp)
addi sp, sp, 4
```



2. 语义规范中规定“除以零、模零都是未定义行为”，但是即使除法的右操作数不是 0，仍然可能存在未定义行为。请问这时除法的左操作数和右操作数分别是什么？请将这时除法的左操作数和右操作数填入下面的代码中，分别在你的电脑（请标明你的电脑的架构，比如 x86-64 或 ARM）中和 RISCV-32 的 qemu 模拟器中编译运行下面的代码，并给出运行结果。（编译时请不要开启任何编译优化）

   

```c
#include <stdio.h>

int main() {
  int a = -2147483647 - 1;
  int b = -1;
  printf("%d\n", a / b);
  return 0;
}
```

上面的代码中的除法为 -2147483648 / -1，理论结果为2147483648，而根据规范，商不能表示的整数除法是未定义行为。

我的电脑为x84-64架构，使用编译命令`gcc -o test -O0 -g test.c`，得到的输出为`Floating point exception: 8`

在RISCV-32的spike模拟器上，输出为`-2147483648`



