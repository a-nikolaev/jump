
# Functional Jump

This is an example implementation of a simple pure ML-like functional language with
a **non-standard control flow mechanism**, which we can call **functional jump** for now.

It is a simple generalized control flow mechanism for implementing 
both iteration and branching. Roughly speaking, it tries to unify the following concepts:

  - **pattern matching**
  - **ad-hoc exception introduction and handling** 
  - **goto** (yay! the considered-harmful one!) 
  - related to continuations

## Example

A function computing the sum of the first `n` integers:

    let sum = fun n ->
      >> (n, 0) where
      | (0, acc) -> acc
      | (k, acc) -> >> (k-1, acc+k)
      end

## Goals

Potentially, this mechanism can be useful for simplifying recursive function definitions that
describe iterations, but involve complicated logic or order of execution that is painful to
express in a clear way with normal recursion.

It provides seamless integration of finite-automaton-like control flow into a functional language.
Kinda wondering if there is any relation to co-induction or any other fancy topics.

It seems that the correctness of labels and jumps can be statically type-checked, but I haven't proved it yet.

## How to build and run

You have to install `ocaml` compiler and `menhir` parser generator. 
It is recommended to use OCaml package manager `opam` to install `menhir`.

    make
    ./jump < test/sum.jump

In addition, the file "jump.vim" provides some basic syntax highlighting for Vim text editor. 
(Please follow/search for the official Vim instructions for installing such syntax files.)

## Status

  - Practically no error reporting.
  - No documentation about this control flow mechanism.

## References

1. Edsger Dijkstra (March 1968). "[Go To Statement Considered Harmful](http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html)".
Communications of the ACM 11 (3): 147–148. 

## License

The software is distributed under the conditions of the BSD 3-clause license.
