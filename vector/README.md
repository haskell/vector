The `vector` package [![Build Status](https://github.com/haskell/vector/workflows/CI/badge.svg)](https://github.com/haskell/vector/actions?query=branch%3Amaster)
====================

This package includes various modules that will allow you to 
work with vectors and use an optimisation framework called [*fusion*](#fusion). 
In this context, vector is an `Int`-indexed array-like data structure with a simpler 
API that can contain any Haskell value. Additionally, its equivalence 
to C-style arrays and optimisation via fusion accelerates vector’s 
performance and makes it a great alternative to list. By installing this 
package, you’ll be able to work with [boxed, unboxed, storable, and primitive 
vectors](#vectors-available-in-the-package) as well as their generic interface.


## Table of Contents

<!-- no toc -->
- [Installation](#installation)
- [Tutorial](#tutorial)
- [Vector vs Array](#vector-vs-array)
- [Vectors Available in the Package](#vectors-available-in-the-package)
- [Fusion](#fusion)

## Installation

If you use **cabal**, modify `package.cabal` so that its `build-depends` 
section includes vector package:
```
build-depends: base ^>=4.17.2.1
             , vector ==0.13.1.0
```
If you use **stack**, modify `package.yaml` so that its `depends` 
section includes vector package:
```
dependencies:
- base >= 4.7 && < 5
- vector == 0.13.1.0
```


## Tutorial

A beginner-friendly tutorial for vectors can be found on 
[MMHaskell](https://mmhaskell.com/data-structures/vector).


If you have already started your adventure with vectors, 
the tutorial on [Haskell Wiki](https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial) 
covers more ground.

## Vector vs Array

Arrays are data structures that can store a multitude of elements 
and allow immediate access to every one of them. Even though Haskell 
has a built-in [Data.Array module](https://hackage.haskell.org/package/array-0.5.7.0), 
arrays might be a bit overwhelming to use due to their complex API. 
Conversely, vectors incorporate the array’s *O(1)* access to elements 
with a much friendlier API of lists. Since they allow for framework 
optimisation via loop fusion, vectors emphasise efficiency and keep 
a rich interface. Unless you’re confident with arrays, it’s 
well-advised to use vectors when looking for a similar functionality.

## Vectors Available in the Package

**Boxed vectors** store each of its elements as a pointer to its value. 
Because we cannot directly access the contents of a boxed vector, they 
are slower in comparison to unboxed vectors.


**Unboxed vectors** store solely their elements’ values instead of pointers. 
To be unboxed, the elements need to be constant in size. Since we can directly 
access the contents of the unboxed vector, working with them is quite efficient.


**Storable vectors** are pinned, convertible to and from pointers, and 
usable in C functions.


**Primitive vectors** contain elements of primitive type. 
Primitive types can be recognised by the hash sign attached at 
the end of value and/or type’s name, e.g. `3#` or `Int#`. You can read 
more about them [here](https://downloads.haskell.org/~ghc/5.00/docs/set/primitives.html).

## Fusion

An optimisation framework provided in this package, fusion 
is a technique that merges several functions into one and forces 
it to produce only one outcome. Without fusion, your program might 
generate intermediate results for each function separately and 
stall its performance.