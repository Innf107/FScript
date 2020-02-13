# FScript
* [1 Introduction](#1)
    * [1.1 The commandline interface](#1.1)
    * [1.2 Basic Syntax](#1.2)
* [2 Basic Concepts](#2)
    * [2.1 Types](#2.1)
    * [2.2 IO](#2.2)
    * [2.3 Modules](#2.3)
* [3 The Standard Library](#3)
    * [3.1 Native Functions](#3.1)
    * [3.2 Base.fscript](#3.2)
    * [3.3 AssociationList.fscript](#3.3)
* [4 The Experimental Library](#4)
    * [4.1 Ratio.fscript](#4.1)

# <a name="1">1 Introduction
FScript is a small, purely functional, interpreted 
dynamic programming language. 
It is my first proper language so there may be some bugs particularly with the parser.

FScript was written in [Haskell](https://haskell.org) using [Parsec](https://hackage.haskell.org/package/parsec) as a parsing library.
Because this is my first language, it does not actually compile to machine code, but is executed by the haskell runtime. 
Because of that, do not expect FScript to be very fast.

FScript's syntax is very similar to [Haskell](https://haskell.org) and [JSON](https://www.json.org/) is natively supported as a subset of FScript.

# <a name="1.1">1.1 The commandline interface 
If you know haskell, you are probably familiar with [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) already.
If you are not, don't worry! GHCi is just a REPL (**R**ead **E**valuate **P**rint **L**oop) for Haskell.
This is the same as Python's interactive interpreter: a **command prompt**, in which you can **evaluate** FScript statements or expressions, have the result **printed** out to the console and **repeat**.

This will be quite useful for the following documentation, because you may want to follow along:
```bash
fscript --repl
```

To run regular scripts you can just enter
```bash
fscript <FILENAME>
```

If you want further information type in 
```bash
fscript --help
```
# <a name="1.2">1.2 Basic Syntax

## Statements
All statements **have** to be ended by a semicolon.
In Fscript there are 3 types of statements.
* [Import](#1.2.1)
* [Definition](#1.2.2)
* [IO](#1.2.3)

### <a name="1.2.1">Import
Fscript allows you to import modules with a very simple syntax
```haskell
import MODULENAME
```
Imports will be covered in more detail in [2.3 Modules](#2.3)
 
### <a name="1.2.2">Definition
Unlike a lot of imperative languages and even a lot of functional ones, FScript does not actually have function definitions.
There are just Variable definitions and first class functions.
Variable definitions are very simple.
```haskell
x = EXPRESSION
```
If you want to define functions you have to use lambda expressions, which will be covered in more detail in [**TODO**](#TODO).
```haskell
f = \x -> EXPRESSION USING x

g = \x -> \y -> EXPRESSION USING x AND y
```

### <a name="1.2.3">IO
IO statements are just regular expressions not written in a [definition](#2.1.1).
Those expressions have to return a value of type IO, otherwise the Statement will fail.
More information on IO will be covered in [2.2 IO](#2.2) and types will be covered in [2.1 Types](#2.1)

For now a function that returns an IO is `print`.

```haskell
print "Hello, World!"
print 42
print "1337"
1 + 2 

-- Output: 
-- "Hello, World!"
-- 42
-- "1337"
-- Error: Can only run values of type IO!
```

## Expressions:
FScript features 5 types of expressions
* [Literals](#1.2.2.1)
* [Variable lookups](#1.2.2.2)
* [Function calls](#1.2.2.3)
* [if/else expressiona](#1.2.2.4)
* [let expressions](#1.2.2.5)

### <a name="1.2.2.1">Literals
FScript features literals for every primitive type. Types will be covered in more detail in [2.1 Types](#2.1).

**Numbers**
```haskell
25
2166.675
```
Note that all numbers are represented as 64bit floating points. If you want better precision you maywant to check out [experimental/Ratio](#4.1)

**Booleans**
```haskell
True
False
```
Note that the functions `true` and `false` exist in [Base](#3.1) for compatibility with JSON

**Char**
```haskell
x = 'H'
```

**Lists**
```haskell
[1, True, Null, f 5, 2 * (3 + 5), SOMEOTHEREXPRESSION]

"Hello, World!"

"Hi!" == ['H', 'i', '!']
```
Just like Haskell, FScript does not have a dedicated type for strings. 
Instead strings are represented as lists of `Char`s

**Null**
```haskell
Null
```
Again, there is also `null` for compatibility with JSON

**Lambda Expressions**

As stated previously, Lambdas are the only way to create functions in FScript.
If you want functions with multiple parameters, you have to explicitly curry them (as covered in more detail in [TODO](#TODO)).

```haskell
f = \x -> x * x;

(\y -> f(y) + y) 5;

-- Multiple parameters
g = \x -> \y -> x * y;
```

**Records**
```haskell
{
    x:42,
    y:"Hello, World!",
    a: {
        someLongName:True,
        someExpr: 1 + 2,
        f: \x -> x * x
    },
    "z": "A strink key!"
}
```
Records are fully compatible with JSON. This means, that any valid JSON is automatically a valid FScript record.
If you want to parse a string as a record, use the native function [eval](#1).

### <a name="1.2.2.2">Variable lookups
Variable lookups are very simple, after you've defined a variable (remember that functions are just variables in FScript), you can look it up by just typing its name.
This may seem obvoius to you, but some languages like PHP require you to prefix variable lookup with `$`
```haskell
-- Definition
x = 5

-- Lookup
print x
y = x + 2
```

### <a name="1.2.2.3">if/else expressions
If you are coming from imperative languages like Java or C, you are probably used to if/else **statements**.
If/else **expressions** are... well... *expressions*. So what's the difference?
Expressions **always** return a value, while statements **never** return anything, which means that in if/else expressions the else branch is **not** optional.
Thus if/else expressions are more similiar to the ternary operator (`?:`) in imperative languages.
```haskell
f = \x -> if x == 42 
            then "the answer to everything!" 
            else "just some random number";

print (f 5);
-- "just some random number"

print(f 42);
-- "the answer to everything!"
```

### <a name="1.2.2.4">Let expressions
You may have noticed, that with what was presented so far, there is no way to create local variables.
Let expressions solve that issue in a great way!
```haskell
f = \x -> let y = x ^ 8 in x + y

print(let x = 5 in x + 3)
```                                                                
Because they are expressions, you can use let expressions anywhere you can put an expression!
More on this in [TODO](#TODO)


# <a name="2"> 2 Basic Concepts
