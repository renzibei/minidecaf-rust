# Step1 Report

## 实验内容

### 运行minilexer, miniparser 和 minivisitor

当输入的字节流如下时

```c
int main() {
	return 0;
}
```

运行minilexer.py的输出如下

```
token kind   text                
-----------  ------------------- 
Int          int                 
Identifier   main                
Lparen       (                   
Rparen       )                   
Lbrace       {                   
Return       return              
Integer      0                 
Semicolon    ;                   
Rbrace       }      
```

以此作为miniparser.py的输入，输出的语法树如下

```lua
program(function(type(Int(int)), Identifier(main), Lparen((), Rparen()), Lbrace({), statement(Return(return), expression(Integer(0)), Semicolon(;)), Rbrace(})))
```

minivisitor.py输出的汇编代码如下

```assembly
.text
        .globl  main
main:
        li      a0,0
        ret
```

### 使用lalr1进行lex和parse

我是用工具lalr1来完成lex和parse的步骤

按照**EBNF**规范，小写字母开头的字母串表示非终结符，大写字母开头的字母串表示终结符。使用过程宏描述规则。

```toml
#[rule = "program -> function"]
#[rule = "function -> Type Identifier Lparen Rparen Lbrace statement Rbrace"]
#[rule = "statement -> Return expression Semicolon"]
#[rule = "expression -> Integer"]
```

终结符的正则模式如下

```toml
'int' = 'Type'
'\(' = 'Lparen'
'\)' = 'Rparen'
'\{' = 'Lbrace'
'\}' = 'Rbrace'
'return' = 'Return'
';' = 'Semicolon'
'\s+' = '_Eps'
'\d+' = 'Integer'
'[a-zA-Z_][a-zA-Z0-9_]+' = 'Identifier'
```

经过parser，得到AST

### 使用中间码

从上一步得到的AST进一步生成IR，目前的IR比较简单，有两个指令`push`和`ret`

目前从AST到IR只需要将函数中的语句都转化为IR指令

从IR到汇编时，其中`push x`对应的risc-v汇编码为`addi sp, sp, -4 ; li t1, X ; sw t1, 0(sp)`

`ret`对应的汇编码为`lw a0, 0(sp) ; addi sp, sp, 4 ; ret`

### 综合

对于如下minidecaf代码

```c
int main() {
    return 100;
}
```

生成的risc-v汇编码为

```assembly
.text
.global main
main:
 addi sp, sp, -4
 li t1, 100
 sw t1, 0(sp)
 lw a0, 0(sp)
 addi sp, sp, 4
 ret
```

## 思考题

### 修改 minilexer 的输入（`lexer.setInput` 的参数），使得 lex 报错

如果代码中有非ascii字符，则会引发报错，如下面的代码

```c
你int main() {
	return 0;
}
```

输出如下，当解析Token时，匹配了`r'.'`，属于error Token，引发报错。

```
token kind   text                
-----------  ------------------- 
Traceback (most recent call last):
  File "/Users/fanqu/programs/compile/minidecaf-tutorial-code/step1/minilexer.py", line 94, in <module>
    dumpLexerTokens(default())
  File "/Users/fanqu/programs/compile/minidecaf-tutorial-code/step1/minilexer.py", line 89, in dumpLexerTokens
    for tok in lexer.lex():
  File "/Users/fanqu/programs/compile/minidecaf-tutorial-code/step1/minilexer.py", line 45, in lex
    raise Exception(f"lex error at input position {self.pos}")
Exception: lex error at input position 4
```

### 修改 minilexer 的输入，使得 lex 不报错但 parse 报错

下面的输入在lex上不会出错，会先匹配前导数字再匹配字母串。

```c
0123int main() {
	return 0;
} 
```

minilexer输出如下

```
token kind   text                
-----------  ------------------- 
Integer      0123                
Int          int                 
Identifier   main                
Lparen       (                   
Rparen       )                   
Lbrace       {                   
Return       return              
Integer      0                   
Semicolon    ;                   
Rbrace       }  
```

而当parse时就会出现错误

```
Traceback (most recent call last):
  File "/Users/fanqu/programs/compile/minidecaf-tutorial-code/step1/miniparser.py", line 70, in <module>
    print(default().parse("program"))
  File "/Users/fanqu/programs/compile/minidecaf-tutorial-code/step1/miniparser.py", line 44, in parse
    children.append(self.parse(child))
  File "/Users/fanqu/programs/compile/minidecaf-tutorial-code/step1/miniparser.py", line 44, in parse
    children.append(self.parse(child))
  File "/Users/fanqu/programs/compile/minidecaf-tutorial-code/step1/miniparser.py", line 41, in parse
    raise Exception(f"syntax error, {child} expected but {tok.kind.name} found")
Exception: syntax error, Int expected but Integer found
```

### 在 riscv 中，哪个寄存器是用来存储函数返回值的

在本例中，整数寄存器a0用于储存函数返回值。而在更多的代码中，整数寄存器a0,a1，浮点数寄存器fa0, fa1都可以用于储存返回值。而当返回值是更大的结构体时，则会储存在栈中。

## 参考

参考了Mashplant助教的实现[https://github.com/decaf-lang/minidecaf/tree/mashplant](https://github.com/decaf-lang/minidecaf/tree/mashplant)



