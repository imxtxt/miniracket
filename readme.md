## 如何编译Miniracket编译器
首先请安装好OCaml的开发环境(版本大于5.3)，然后运行下面的命令编译Miniracket编译器的源代码：
``` shell
 dune build
```

## 如何使用Miniracket编译器
使用下列命令编译程序：
``` shell
./_build/default/main.exe file_path.rkt
比如：./_build/default/main.exe examples/array1.rkt 
```
这会生成名为output.s的x86汇编文件，然后使用Clang把output.s和runtime.c链接起来. 命令如下：
``` shell
clang runtime.c output.s
```

这会生成一个名为a.out的可执行文件，直接运行a.out便可以看到程序的运行结果

## Miniracket的常用语法结构
#### 函数定义
函数定义的关键字是define，然后跟着一个参数列表，接着是返回类型，最后是函数体，下面是一些例子：
``` Rust
  (define (add [x : Integer] [y : Integer]) : Integer
    (+ x y))
```

#### Let绑定
let 绑定的格式如下：
``` OCaml
  (let ([id expression1]) expression2)
```
首先，expression1会被执行，然后它的值会和id绑定起来，接下来执行expression2.


#### If 表达式
If表达式的格式如下：
``` OCaml
  (if expression1 expression2 expression3)
```
如果expression1的结果为true，那么会执行expression2，反之会执行expression3

#### vector 表达式
``` OCaml
    (vector 1 2 3)
```
我们可以使用vector-ref访问tuple的成员, 比如：
``` OCaml
  (vector-ref (vector 1 2 3) 1)
```

#### 数组
``` OCaml
  (array 3 10)
```
创建一个长度为3的数组，数组中的每一个成员为10
我们可以使用array-ref访问array的成员, 比如：
``` OCaml
  (array-ref (array 3 10) 1)
```

#### While 表达式
While表达式的格式如下：
``` Rust
  (while expression1 expression2 )
```

#### begin 表达式
begin表达式的格式如下：
``` OCaml
  (begin
    expression1
    expression2
    expression3
    expression4)
```
sequance表达式的会从左到右执行所有表达式，最后一个表达式的值为sequance表达式的值

#### 赋值表达式
我们可以为变量，数组，tuple赋值，格式如下：
``` OCaml
  (set! a 10)
  (array-set! a 10 20)
  (vector-set! a 20 10)
```

#### void表达式
我们可以用(void)创建一个void值

#### 函数调用表达式
函数调用和racket一样，下面是一些例子：
``` OCaml
  (add 1 2)
  (sub 1 2)
```

#### lambda 表达式
lambda表达式是现代编程语言的重要特性,下面是一些例子：
``` OCaml
  (lambda: ([x : Integer] [y : Integer]) : Integer
    (+ x y))
```
#### 类型
Miniracket有Integer, Boolean, Void, Vector, Array, Function类型。
Vector类型的书写方式为：(Vector Type Type Type)。
Array类型的书写方式为：(Array Type)。
Function类型的书写方式为: (type type type -> type).
