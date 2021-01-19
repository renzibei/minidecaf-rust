# Step6 Report

## 实验内容

实现了if语句与条件表达式

在step5中并没有实现非终结符statement可以为空的特性，因为statement\*可以为空。

但是step6引入了if，if语句的body可以为空，因此给statement增加了一个empty

对于吊悬else问题，根据规范写的生成式结合lalr1的默认优先级，自动解决了优先级问题(

## 思考题

Rust与Go语言的if-else语法的优点是不会出现吊悬else问题，else与哪个if语句结合很明确。程序员所想与编译器的实现将一致。另外编译器也不需要花额外的精力在优先级上，实现简单效率高。

