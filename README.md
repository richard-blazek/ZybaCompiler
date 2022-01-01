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

isPrime fun[n: int] bool {
    prime = 1b
    if n < 2 {
        prime = 0b
    } else {
        i = 2
        while i < n & prime {
            prime = n % i != 0
            i = i + 1
        }
    }
    prime
}

sum fun[n: int.array] int {
    total = 0
    i = 0
    while i < n.size {
        total = total + n.get[i]
        i = i + 1
    }
    total
}

range fun[n: int] int.array {
    result = int.array
    i = 0
    while i < n {
        result.append[i + 1]
        i = i + 1
    }
    result
}

concat fun[a: int.array.array] int.array {
    result = int.array
    i = 0
    while i < a.size {
        result.append[a.get[i]]
        i = i + 1
    }
    result
}

merge fun[a: int.array, b: int.array] int.array {
    result = int.array
    i = 0
    while (i < a.size) & (i < b.size) {
        result.append[a.get[i] b.get[i]]
        i = i + 1
    }
    result
}

```
