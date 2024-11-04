#lang racket/base
(require
 racket/unit
 (only-in "../mix.rkt"              define-mixed-unit inherit)
 (only-in "../nondet.rkt"           pure)
 "../signatures.rkt"
 (only-in "../base/domain-unit.rkt" [domain@ base:domain@]))
(provide domain@)

;; ----------------------------------------
;; Implementation of Domains:

(define-mixed-unit domain@
  (import)
  (export domain^)
  (inherit [base:domain@ α ≤ₐ [base:δ δ] val? stx?])

  ; δ : Prim (Listof Val) → (SetM Val)
  (define (δ p vs) (pure (base:δ p vs))))
