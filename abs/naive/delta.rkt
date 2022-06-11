#lang racket
(require (only-in "../../conc/base/core/delta.rkt" delta)
         (only-in "struct-core.rkt" num? bool?))
(provide δ)

;; ----------------------------------------
;; Implementation of primitives:

;(: δ : Prim (Listof Val) -> Val)
(define (δ p vs)
  (printf "δ")
  (match (cons p vs)
    [`(+ ,(? num? ns) ...) 'num]
    [`(- ,(? num? n) ,(? num? ns) ...) 'num]
    [`(* ,(? num? ns) ...) 'num]
    [`(/ ,(? num? n) ,(? num? ns) ...) 'num]
    [`(< ,(? num? n1) ,(? num? n2) ,(? num? ns) ...) 'bool]
    [`(= ,(? num? n1) ,(? num? n2) ,(? num? ns) ...) 'bool]

    [(cons p vs) (delta p vs)]))
