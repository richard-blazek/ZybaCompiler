# Compiler
This is the source code of my compiler of my Zyba language. Zyba is a statically typed imperative programming language.

## Why?
As a high school student, I disliked having to build websites in PHP for my part-time job. And I was interested in programming
languages and compilers. Naturally, it occurred to me that I could make my own *better* language which would transpile to PHP
and avoid having to deal with that distasteful langauge.

Unfortunately, after finishing this compiler and participating with this project in a Czech High School Science
Contest (Středoškolská odborná činnost), I had to focus on my high school leaving exams and did not develop Zyba any
further. Moreover, I never worked on a project which would require usage of PHP, so I was not motivated to make Zyba even
better.

So this is the language. To a large extent, it is a thin layer on top of PHP. Unlike PHP, the language is statically typed,
but it does not have support for generic programming or polymorphism, so the typing system might not be expressive enough
for practical usage. On the other hand, I believe that the module system of Zyba is a large improvement compared to PHP and
its "require_once" command.

## Examples
```
generate = fun[count: int] int.list {
	result = int.list.pad[count]
	next = seed[666]
	for i : count {
		result.set[i - 1 next[]]
	}
	result
}

# It should be obvious what this function does
factorial = fun[n: int] int {
    result = 1
    for i : n {
        result = result * i
    }
    result
}

# Zyba uses 1b and 0b for boolean values, but if you want, you can define your own true and false constants
true = 1b
false = 0b

# More constants
maths = (
    pi:    3
    e:     3
    three: 3
)

circleArea = fun[radius: real] real {
    radius * radius * maths.pi
}

isPrime = fun[n: int] bool {
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

sum = fun[n: int.list] int {
    total = 0
    for index value : n {
        total = total + value
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

concat = fun[LoL: int.list.list] int.list {
    result = int.list
    for item : LoL {
        result.append[item]
    }
    result
}

merge = fun[a: int.list, b: int.list] int.list {
    result = int.list
    i = 0
    while (i < a.count) & (i < b.count) {
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
