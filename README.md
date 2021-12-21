# Compiler
This is the source code of my compiler of my Zyba language. The language transpiles into PHP and could be eventually used as a substitute of PHP for developing web applications. However, neither the compiler nor the language are complete.

Code example:
```
factorial fun[n: Int] Int (
    i: 1
    result: 1
    while i <= n (
        result: result * n
    )
    result
)
```
