# Step5 Report

## 实验内容

增加了局部变量的使用和赋值语句，并引入了栈帧

在对于变量是否定义与重复定义的检查上，每个函数内使用了HashMap来储存变量表，在IR生成阶段进行语义检查

## 思考题

### 1

栈帧的构成分为

1. 表达式运算栈
2. 局部变量空间
3. 返回地址和旧的栈帧基址(old fp)

表达式运算栈最小空间为0，即当前不计算表达式

局部变量空间的空间最小为0，即当前函数没有局部变量

第三部分的大小固定为8Byte

### 2

在定义变量时，如果变量之前定义过，不报错，使用原变量内存储存变量。

在查找变量时不需要修改。