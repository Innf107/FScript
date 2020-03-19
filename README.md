# FScript
* [1 Introduction](#1)
    * [1.1 The commandline interface](#1.1)
    * [1.2 Prerequisites](#1.2)
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

FScript's syntax is very similar to [Haskell](https://haskell.org) although [JSON](https://www.json.org/) is natively supported as a subset of FScript.

# <a name="1.1">1.1 The commandline interface 
If you know haskell, you are probably familiar with [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) already.
If you are not, don't worry! GHCi is just a REPL (**R**ead **E**valuate **P**rint **L**oop) for Haskell.
This is the same as Python's interactive interpreter: a **command prompt**, in which you can **evaluate** FScript statements or expressions, have the result **printed** out to the console and **repeat**.

FScript's REPL will be quite useful for the following documentation, so you may want to follow along:
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
# <a name="1.2">1.2 Prerequisites
This guide assumes you already know how to use a functional programming language, ideally haskell. If there is some Feature or Concept of the language that you don't understand, try looking for an explaination for Haskell.

# <a name="2"> 2 Basic Concepts
FScript is a functional programming language, which means that you define functions by composing other functions. You can define functions with the same syntax as you would use in Haskell:
```haskell
f x = x + 1;
```
One important difference you may have spotted is that you have to end FScript statements with a semicolon. This may be a bit annoying at times, but its a consequence of fscript being whitespace insensitive, which means that you do not have to care about indentation!

Another difference between haskell and fscript is that while in haskell you can perform **side effects** (like printing something to the console) in your main function, you do not actually have a main function in fscript. Instead **any top-level function call** will be treated as an IO action, which makes sense, because you cannot do anything else in a top-level function call anyway.

TLDR; Instead of writing 
```haskell
main = print 42
```
you can just do
```haskell
print 42;
```

If instead of haskell or F#, you're used to languages like Java or Scala, that last example may have confused you. Unlike in Java where you have to surround fumction arguments in parentheses and seperate them with commas, you just have to seperate them with spaces ib FScript.
`Java`
```Java
f(x, y, z)
```
`Haskell`
```haskell
f x y z
```

This is due to a property of the language called explicit currying. If you want to learn more, you should probably look up implicit currying in haskell, but I'm going to provide a summary:
If an FScript function takes more than one argument, it actually just takes the first and returns a new function that captures this first one and takes the remaining arguments to return a result.
This means that a function call like
```haskell
f x y = x + y;
```
actually gets translated in something like this
```haskell
f x = \y -> x + y;
```

A very convenient consequence of this is *partial application*.
Partial application means, not giving a function all arguments, to produce a new function that just takes those remaining arguments. This is very useful when working with higher order functions like `map` or `filter`.

Example:
```haskell
add x y = x + y;

map (add 5) [1, 2, 3];
-- [6, 7, 8]

-- the same as
map (\x -> add 5 x) [1, 2, 3];
```
This function `add` would usually take 2 arguments and add them up, but in this example it is only supplied 1 (5).
Thus it returns a new function that only takes the second argument and adds it to the first (5). You can imagine that new function like this:
```haskell
f y = that only takes the second argument and adds it to the first (5). You can imagine that new function like this:
```haskell
f y = 5 + y;
```

**Lambdas!**
In those examples, you may have seen me use *Lambda expressions* already. A lambda can basically be thought of as a function literal. FScript and haskell both use `\ for them, because the 

## <a name="2.1">2.1 Types


# <a name="3"> 3 The Standard Library

## <a name="3.1"> 3.1 Native Functions
A native function (or NativeF) is a function, that is implemented in the Haskell runtime instead of FScript.
The implementations for these are located in [NativeFs.hs](app/NativeFs.hs).

* [eval](#3.1eval)
* [compIO](#3.1compIO)
* [readLine](#3.1readLine)
* [throw](#3.1throw)
* [rem](#3.1rem)
* [put](#3.1put)
* [add](#3.1add)
* [sub](#3.1sub)
* [mul](#3.1mul)
* [ord](#3.1ord)
* [div](#3.1div)
* [debugRaw](#3.1debugRaw)
* [head](#3.1head)
* [tail](#3.1tail)
* [typeof](#3.1typeof)
* [cons](#3.1cons)
* [get](#3.1get)
* [set](#3.1set)
* [pureIO](#3.1pureIO)
* [round](#3.1round)

###<a name=3.1eval>eval
`eval` evaluates FScript code given as a string. 
Note that it only evaluates **expressions** and not **statements**.
The implementation for eval is located in [Main.hs](app/Main.hs) because it needs access to additional Runtime state.
```haskell
(eval "3 + 1")
-- 4

(eval "x = 3")
-- Type Exception: Can only evaluate strings
```

###<a name=3.1compIO>compIO
`compIO` lets you compose IO actions. It is basically the FScript equivalent of haskell's `>>=`.
`>>=` is actually an alias for `compIO`, defined in Base.
More information in [IO](#2.2)

###<a name=3.1readLine>readLine
`readLine` reads a line from stdin. You have to use compIO to be able to use that line.
```haskell
+> readLine >>= print
Test
"Test"
```

###<a name=3.1throw>throw
`throw` takes two string arguments: The **type** of the exception and the **message**.
```haskell
f = \x -> if ((typeof x) == "Num") 
    then x 
    else throw "Type" "f needs its parameter to be of type Num";
```

###<a name=3.1rem>rem
The remainder of two integers.
```haskell
rem 3 4
-- 3.0
rem -1 4
-- -1.0
```

###<a name=3.1put>put
Prints a string to stdout. `print` is defined as follows:
```haskell
print = \x -> put (show x)
```

###<a name=3.1add>add
Simple addition. `+` is an alias
```haskell
add 3 4
-- 7.0
```
###<a name=3.1sub>sub
Simple subtraction. `-` is an alias
```haskell
sub 3 4
-- -1.0
```
###<a name=3.1mul>mul
Simple multiplication. `*` is an alias
```haskell
mul 3 4
-- 12
```
###<a name=3.1ord>ord
Compares two values.
Returns `-1` if the first is less than the second.
Returns `1` if it is larger and `0` if they are equal. 
###<a name=3.1div>div
Simple division. `/` is an alias
```haskell
div 3 4
-- 0.75
```
###<a name=3.1debugRaw>debugRaw
A wrapper around haskell's show function.
###<a name=3.1head>head
Returns the first element of a list
```haskell
head [1,2,3,4]
-- 1.0
```
###<a name=3.1tail>tail
Returns every element of a list except the first.
```haskell
tail [1,2,3,4]
-- [2.0, 3.0, 4.0]
``` 
###<a name=3.1typeof>typeof
returns the type of a value as a string. The type can be one of `Num`, `Bool`, `Char`, `List` or `Record``.
###<a name=3.1cons>cons
Prepends a value to a list. An alias is `:`.
```haskell
cons 1 [2, 3, 4, 5]
-- [1, 2, 3, 4, 5]
```
###<a name=3.1get>get
Gets a field from a record, specified by a string
```haskell
get "x" {x: 5}
-- 5
```
###<a name=3.1set>set
Sets a field in a record, specified by a string
```haskell
set "x" 5 {x: 3}
-- {x: 5}

set "y" 5 {x: 3}
-- {x: 3, y: 5}
```
###<a name=3.1pureIO>pureIO
creates an IO that does nothing but just returns a value. This is useful in combination with `compIO`
```haskell
((pureIO 5) >>= print)
-- "5"
```
###<a name=3.1round>round
rounds a number
```haskell
round 4.7
-- 5
round 2.1
-- 2
```
