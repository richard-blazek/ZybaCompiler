# Compiler
This is the source code of my compiler of my Zyba language. The language transpiles into PHP and could be eventually used as a substitute of PHP for developing web applications. However, neither the compiler nor the language are complete. Also, please note that the tests are obsolete and don't even compile.

Code example:
```python
# It should be obvious what this function does
factorial = fun[n: int] int {
    result = 1
    while n > 1 {
        result = result * n
        n = n - 1
    }
    result
}

# Zyba uses 1b and 0b for boolean values, but if you want, you can define your own true and false constants
true = 1b
false = 0b

# More constants
maths = {
    pi    3
    e     3
    three 3
}

circleArea = fun[radius: real] real {
    radius * radius * maths.pi
}

isPrime = fun[n: int] bool {
    prime = 1b
    if n < 2 {
        prime = 0b
    } else {
        i = 2
        while i * i <= n & prime {
            prime = n % i != 0
            i = i + 1
        }
    }
    prime
}

sum = fun[n: int.list] int {
    total = 0
    i = 0
    while i < n.size {
        total = total + n.get[i]
        i = i + 1
    }
    total
}

range = fun[n: int] int.list {
    result = int.list
    i = 0
    while i < n {
        result.append[i + 1]
        i = i + 1
    }
    result
}

concat = fun[a: int.list.list] int.list {
    result = int.list
    i = 0
    while i < a.size {
        result.append[a.get[i]]
        i = i + 1
    }
    result
}

merge = fun[a: int.list, b: int.list] int.list {
    result = int.list
    i = 0
    while (i < a.size) & (i < b.size) {
        result.append[a.get[i] b.get[i]]
        i = i + 1
    }
    result
}

# Linear congruential pseudo-random generator
a = 1103515245
c = 12345 + int
m = 256r10000

next = fun[x: int]int {
    x * a + c % m
}

seed = fun[s: int] int.fun {
    fun[] int {
        s = next[s]
        s
    }
}

```
