#lang racket/base
(require
 racket/unit
 (only-in "../nondet.rkt"     lift)
 (only-in "../mix.rkt"        define-mixed-unit inherit)
 (only-in "../set.rkt"        ∅ set-add)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../base/units.rkt" [mstore@ base:mstore@]))
(provide mstore@)

(define-mixed-unit mstore@
  (import)
  (export  mstore^)
  (inherit [base:mstore@ init-Σ alloc-name alloc-scope alloc-𝓁])

  ;; Set-based Σ

  ; lookup-Σ : Σ Nam -> (SetM (Setof StoBind))
  ;          : Σ 𝓁   -> (SetM (U Val ξ κ))
  (define (lookup-Σ Σ0 k)
    (lift (hash-ref (Σ-tbl Σ0) k ∅)))

  ; update-Σ : Σ Nam (Setof StoBind) -> Σ
  ;          : Σ 𝓁   (U Val ξ κ)     -> Σ
  (define (update-Σ Σ0 k v)
    (Σ (Σ-size Σ0)
      (hash-update (Σ-tbl Σ0) k
                   (λ (old) (set-add old v)) ∅))))
