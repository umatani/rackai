This directory contains implementations of the models in

 Macros that Work Together
 Flatt, Culpepper, Darais, and Findler

to run in Racket version 5.1.1 or later.

The models sources are

  mm-core.rkt --- section 3.1
  mm-stxobj.rkt --- section 3.2
  mm-parse.rkt --- section 3.3
  mm-expand.rkt --- section 3.4
  mm-defmacro.rkt --- section 3.5
  mm-stxcase.rkt --- section 3.6
  mm-local.rkt --- section 3.7
  mm-defs.rkt --- section 3.8

Except for the first, each contains a number of examples bound with
`define-example'. The `define-example' form shows the input expression
(in concrete syntax) and the expected parsed term (which makes the
example a test case). Run the file in DrRacket, then for an example
like `simple-parse-example', run it in one of the following ways using
the DrRacket interactions window:

 * (simple-parse-example 'expand) --- produces the expansion result
 * (simple-parse-example 'parse) --- parses the expansion to an AST


The typesetting definitions in each "mm-....rkt" file---which are used
directly to produce the paper---are of limited interest, but the
"rewrite.rkt" module controls how a few details are hidden in the
paper's model. Notably, choosing fresh names requires a
name-generation accumulator, which "rewrite.rkt" causes to be hidden.
