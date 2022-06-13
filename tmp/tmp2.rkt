#lang racket
(require "../reduction.rkt")
(provide --> -->^)

(define-reduction (--> <+>) [(cons a b) (<+> a b) 'add])

(((reducer-of -->) +) (cons 3 4))

(define-reduction ==> #:super (--> *)
  [(? number? x) (* x x) 'mul])

(((reducer-of ==>)) (cons 3 4))
