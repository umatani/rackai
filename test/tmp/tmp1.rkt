#lang racket
(require "../../reduction.rkt")

(define-reduction (-->/+ <+>)
  [(cons a b) (<+> a b) 'add])

(define -->+ ((reducer-of -->/+) +))
(-->+ (cons 2 3))

(define-reduction ==> #:super (-->/+ *))

(define ==>* ((reducer-of ==>)))
(==>* (cons 2 3))

(define-reduction ~~> #:super ==>)

(define ~~>* ((reducer-of ~~>)))
(~~>* (cons 2 3))
