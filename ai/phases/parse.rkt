#lang racket
(require (only-in "../../interp/phases/parse.rkt" parse&parse*/resolve)
         (only-in "syntax.rkt" resolve))
(provide (all-defined-out))

(define-values (parse parse*) (parse&parse*/resolve resolve))
