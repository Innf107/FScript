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
```c
If you want further information type in 
```bash
fscript --help
```
# <a name="1.2">1.2 Basic Syntax
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
#TODO