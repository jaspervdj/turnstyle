---
title: Turnstyle Specification
author: Jasper Van der Jeugt
version: v0.0.1
---

The program is encoded as an image.  A **lossless** image format should be used
so exact colors are preserved.  The use of [PNG] is recommended because of its
wide support and decent compression.

We assume that the reader is somewhat familiar with Lambda Calculus.
If you want to learn more about Lambda Calculus, we recommend
[this video with Graham Hutton](https://www.youtube.com/watch?v=eis11j_iGMs) or
[this paper by Raúl Rojas](https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf)
for a good introduction.

# Syntax

Turnstyle programs are evaluated by reading and evaluating **expressions** from
the image.  An expression is read using a given **position** (represented as
integral _(x, y)_ coordinates) and **heading** (right, down, left or up).

The top-level expression of a program is found at the center of the left side of
the image _(0, floor (image height / 2))_, and the initial heading is **right**.

To read an expression, we consider the **Turnstyle shape** of the pixels
surrounding the current **position** and facing the current **heading**.
If any of these four pixels lies outside of the image, the program should
terminate with an error.

Here is the Turnstyle shape illustrated for all four headings, with the current
position indicated by the small circle, and the heading represented using an
arrow:

![Headings](enter.svg)

For brevity, in all further illustrations in the specification we will assume we
are heading right.

We use the following names to refer to the four pixels that make up the
Turnstyle shape: **L**eft, **C**enter, **F**ront, and **R**ight.

![Pixel names](label.svg)

Turnstyle programs can use _any colors_, as long as we can compare two colors
for (in-)equality.  This gives us **15 unique patterns**.  Here is a cheatsheet:

![Cheatsheet](cheatsheet.svg)

The pattern determines the expression that we read evaluate.  There are five
kinds of expressions:

1.  [Variables](#variables)
2.  [Lambda abstraction](#lambda-abstraction)
3.  [Function application](#function-application)
4.  [Symbols](#symbols) ([primitive operations](#primitives) and numeric literals)
5.  [Identity](#identity) (no-ops)

## Variables

In Turnstyle, we use _colors_ as variable names.  Depending on the pattern,
we pick the color of the pixel indicated by the letters LCFR:

![Variables](var.svg)

This evaluates to the value assigned to the variable.  If the variable is
unassigned, the program should terminate with an error.

## Lambda abstraction

Lambda abstraction evaluates to the anonymous function _(λv.e)_, where the
variable _v_ is the color of the pixel indicated with the letters LCR, and _e_
is the expression at the Turnstyle shape indicated by the arrow.

![Lambda abstraction](lam.svg)

## Function application

Function application evaluates the expression _(f x)_,
where _f_ is the Turnstyle shape indicated by the solid arrow (→)
and _x_ is the Turnstyle shape indicated by the dashed arrow (⇢).

![Function application](app.svg)

If you visualize standing on the image and looking towards the front, the
left-hand side of the application will always be to the left of the right-hand
side of the application.

## Symbols

Symbols encode literals in the program.  We compare the relative **areas** of
the left, front and right pixels.

An **area** is defined as the number of pixels in a contiguous color region.
Pixels of the same color that only touch diagonally are **not** considered
contiguous.

![Symbols](symbol.svg)

 -  If _area(L) = 1_, the Turnstyle evaluates to a **numeric** literal
    of the integer value _area(F)^area(R)_.  Commonly, _area(R)_ is 1.
 -  If _area(L) = 2_, the Turnstyle evaluates to a **primitive function**.
    In this case, _area(F)_ determines the **primitive module**,
    and _area(R)_ determines the **primitive opcode**.
    See also [Primitives](#primitives).
 -  Symbols with _area(L) > 2_ are reserved for future updates to the
    specification.

## Identity

For all other patterns, we evaluate the expression at the Turnstyle indicated by
the arrow (→).  You can visualize this as following the color of the line.

![Identity](id.svg)

# Semantics

## Primitives

This is an overview of the different primitive functions and what they do.

### Input (module=1)

| Opcode | Description                                                                                                                                            |
| :----- | :----------------------------------------------------------------------------------------------------------------------------------------------------- |
| 0      | _((`in_num` k) l)_ reads a number `x` from `stdin`, then evaluates _(k x)_. If `stdin` is closed or an exception occurs, _l_ is evaluated instead.     |
| 1      | _((`in_char` k) l)_ reads a character `c` from `stdin`, then evaluates _(k c)_. If `stdin` is closed or an exception occurs, _l_ is evaluated instead. |

### Output (module=2)

| Opcode | Primitive                                                                                     |
| :----- | :-------------------------------------------------------------------------------------------- |
| 0      | _((`out_number` x) k)_ outputs `x` as a number to `stdout`, and then evaluates _k_.           |
| 1      | _((`out_char` x) k)_ outputs `x` as an Unicode character to `stdout`, and then evaluates _k_. |

### Numerical operations (module=3)

| Opcode | Primitive                                                                          |
| :----- | :--------------------------------------------------------------------------------- |
| 0      | _((`num_add` x) y)_ evaluates to _x + y_.                                          |
| 1      | _((`num_sub` x) y)_ evaluates to _x - y_.                                          |
| 2      | _((`num_mul` x) y)_ evaluates to _x * y_.                                          |
| 3      | _((`num_div` x) y)_ evaluates to _x / y_.                                          |
| 4      | _((`num_mod` x) y)_ evaluates to _x % y_.  Both operands must be integral numbers. |

### Comparisons (module=4)

Turnstyle has no primitive boolean type, and uses [Church encoding] instead,
i.e. _true = λxy.x_ and _false = λxy.y_.

| Opcode | Primitive                                                                |
| :----- | :----------------------------------------------------------------------- |
| 0      | _((((`cmp_eq` x) y) t) f)_ evaluates _t_ if _x = y_, and _f_ otherwise.  |
| 1      | _((((`cmp_lt` x) y) t) f)_ evaluates _t_ if _x < y_, and _f_ otherwise.  |
| 2      | _((((`cmp_gt` x) y) t) f)_ evaluates _t_ if _x > y_, and _f_ otherwise.  |
| 3      | _((((`cmp_lte` x) y) t) f)_ evaluates _t_ if _x ≤ y_, and _f_ otherwise. |
| 4      | _((((`cmp_gte` x) y) t) f)_ evaluates _t_ if _x ≥ y_, and _f_ otherwise. |

## Evaluation order

Turnstyle does not require a specific evaluation order, although the semantics
of the [primitives](#primitives) must be respected (e.g. evaluating
`((((cpm_eq 1) 2) t) f)` should not evaluate `t`).

## Cyclic programs

The Turnstyle position and direction can be manipulated in a way that it ends up
in a previously visited shape.  In that case, there is no finite corresponding
expression in the lambda calculus.

## Precision

Turnstyle borrows its numeric precision concepts from [Scheme][Scheme
Exactness]:

> Scheme numbers are either exact or inexact. A number is exact if it was
> written as an exact constant or was derived from exact numbers using only
> exact operations. A number is inexact if it was written as an inexact
> constant, if it was derived using inexact ingredients, or if it was derived
> using inexact operations. Thus inexactness is a contagious property of a
> number.

[Church encoding]: https://en.wikipedia.org/wiki/Church_encoding
[PNG]: http://libpng.org/pub/png/
[Scheme Exactness]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_8.html#SEC52
