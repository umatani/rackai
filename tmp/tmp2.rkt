#lang racket
(require "../conc/base/reduction.rkt")
(provide (reduction-out -->))

(define-reduction (--> <+>) [(cons a b) (<+> a b) 'add])

(((reducer-of -->) +) (cons 3 4))

(define-reduction ==> #:super (--> *)
  [(? number? x) (* x x) 'mul])

(((reducer-of ==>)) (cons 3 4))
