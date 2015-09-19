
# Functional Jump

This is an example implementation of a simple pure functional language with
a new control flow mechanism, which can be called **functional jump**.

It is a new generalized control flow mechanism for implementing 
both iteration and branching. Roughly speaking, it tries to unify the following concepts:

  - pattern matching
  - ad-hoc exception introduction and handling 
  - goto (yay! the considered-harmful one!)

## Example

A function computing the sum of the first `n` integers:

    let sum = fun n ->
      >> (n, 0) where
      | (0, acc) -> acc
      | (k, acc) -> >> (k-1, acc+k)
      end

## How to build and run

You have to unstall `ocaml` compiler and `menhir` parser generator. 
It is recommended to use OCaml package manager `opam` to install `menhir`.

    make
    ./jump < test/sum.jump

In addition, the file "jump.vim" provides some basic syntax highlighting for Vim text editor. 
(Please follow/search for the official Vim instructions for installing such syntax files.)

## Status

  - Practically no error reporting.
  - No documentation about the jump opearation.

## License

The software is distributed under the conditions of the BSD 3-clause license.
