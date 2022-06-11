#lang racket
(require "../conc/base/reduction.rkt"
         (only-in "tmp2.rkt" -->))

(define-unit test@
  (import)
  (export)
  (define-reduction ==> #:super (--> /)
    [(? number? x) (* x x) 'mul])


  (((reducer-of ==>)) (cons 3 4) #;8))

(invoke-unit test@)
