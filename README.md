Turnstyle is a graphical [esoteric programming language] loosely inspired by
[Piet].  Both encode programs as images, however, the similarities end at the
syntax level.

Where Piet is really a stack machine in disguise, Turnstyle is an encoding of
the (untyped) [Lambda calculus].  This allows for building more reusable images
at a higher level of abstraction, while still keeping the specification small,
making it relatively easy to develop new interpreters.

This repository contains [the language specification](spec/), a reference
implementation in Haskell, and a
[simple JavaScript implementation](turnstyle.js).

# Specification

The cheatsheet below provides a high-level overview, for more details please
consult [the Turnstyle Language Specification](spec/).

![Cheatsheet](spec/cheatsheet.svg)

# Examples

The starting Turnstyle shape is outlined.  Click on the examples to run them.

## 0

_((`num_sub` 8) 8)_ -- returns exit code 0:

[![](examples/minimal.svg)](examples/minimal.png)

## Y

_λf. (λx. f (x x)) (λx. f (x x))_ -- a somewhat symmetrical, reusable Y
combinator:

[![](examples/y.svg)](examples/y.png)

## ⥁

Of course, fixed-point combinators are a primitive tool necessary to compensate
for the limitations of one-dimensional programming.  In two-dimensional
programs, we can achieve recursion using a more visually intuitive approach.
This program prints the sequence of natural numbers:

[![](examples/loop.svg)](examples/loop.png)

## π

Prints an approximation of _pi_ by comparing the circle to its diameter:

[![](examples/pi.svg)](examples/pi.png)

# Design Principles

 -  Lean towards pixel art but allow larger graphics as well
 -  Integers should be represented by areas, so 0 does not have a literal
    equivalent
 -  Allow relative creative in freedom in color choice and images
 -  But also ensure it is possible to create very dense images
 -  Allow users to implement and share code that looks like specific images
 -  A relatively large number of primops should be possible so we can
    actually write real programs.  This lead to using at least two numbers
    for primops.

[esoteric programming language]: https://en.wikipedia.org/wiki/Esoteric_programming_language
[Lambda calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[Piet]: https://www.dangermouse.net/esoteric/piet.html
