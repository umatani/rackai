#lang racket
(require "../reduction.rkt")


;; within-signatures で only を書けるかの確認

(define-signature XY^ (X Y))

(define-reduction (--->/+ <+>) #:within-signatures [(only XY^ X)]
  [(cons a b) (<+> a b X) 'add])

(define-unit XY@ (import) (export XY^)
  (define X   1)
  (define Y 100))

(define reducer2 (reducer-of --->/+ #:within-units [XY@]))
((reducer2 +) (cons 3 4))
