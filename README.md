# Compiler
This is the source code of my compiler of my Zyba language. The language transpiles into PHP and could be eventually used as a substitute of PHP for developing web applications. However, neither the compiler nor the language are complete. Also, please note that the tests are obsolete and don't even compile.

Code example:
```
; It should be obvious what this function does
factorial fun[n: int] int {
    i = 1
    result = 1
    while i <= n {
        result = result * n
        i = i + 1
    }
    result
}

; Zyba uses 1b and 0b for boolean values, but if you want, you can define your own true and false constants
true 1b
false 0b

; More constants
maths {
    pi    3
    e     3
    three 3
}

circleArea fun[radius: float] float {
    radius * radius * maths.pi
}
```
