findexpr
========

Overview
--------

You know how when you first learn algebra you're given an equation like "3 + _x_ = 8", and you have to find _x_?  **findexpr** solves a related problem, one in which the _operators_ are the unknowns.  That is, given "3 _op_ 5 = 8" you have to solve for _op_ and find "+".  Let's try that:

    $ echo '3 5 => 8' | findexpr
    unary ops: - abs sgn
    binary ops: + * - / ^ mod max min
    constants: 1
    allow reps: yes
    require all: no
    solutions: 1
    columns: a b => f1
    3 5 => 8

    f1(a,b) = a + b

Ta da!  Of course, **findexpr** is designed to solve much trickier problems.  For example, can you find a single function _f_ such that _f_(2, 3) = 4, _f_(3, 4) = 2, and _f_(4, 2) = 3?  **findexpr** can:

    $ echo -e '2 3 => 4\n3 4 => 2\n4 2 => 3' | findexpr
    unary ops: - abs sgn
    binary ops: + * - / ^ mod max min
    constants: 1
    allow reps: yes
    require all: no
    solutions: 1
    columns: a b => f1
    2 3 => 4
    3 4 => 2
    4 2 => 3

    f1(a,b) = floor((b + 1) / ((a - 1) mod b))

**findexpr** provides control over the set of allowable operators, the set of constants that can be used, whether each input can be used more than once or not at all, and the number of alternative solutions desired.

Alas **findexpr** has two main limitations:

1. It works only with integers, not floating-point numbers.

2. It can be sloooooooooow.

The slowness comes from **findexpr** using a brute-force algorithm to solve for the mystery function.  To make up for that, the **findexpr** implementation is multithreaded so the more CPU cores you have, the faster it'll run.

Installation
------------

**findexpr** is written in Haskell and has been tested only with the [Glasgow Haskell Compiler](http://www.haskell.org/ghc/).  You'll need that to compile the code.  Currently, the build process consists of the usual

    $ make
    $ make install

and yes, you can override `prefix` and `bindir` on the `make install` command line.  Some day I'll switch over to [Cabal](http://www.haskell.org/cabal/) as the build tool, but Make works fine for now.

Author
------

Scott Pakin, *scott-fexpr@pakin.org*
