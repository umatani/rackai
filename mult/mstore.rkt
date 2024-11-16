#lang racket/base
(require
 racket/unit
 (only-in "../nondet.rkt"     lift)
 (only-in "../mix.rkt"        define-mixed-unit inherit)
 (only-in "../set.rkt"        ∅ set-add for/set)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../base/units.rkt" [mstore@ base:mstore@]))
(provide mstore@)

(define-mixed-unit mstore@
  (import)
  (export  mstore^)
  (inherit [base:mstore@    init-Σ all-nams alloc-name alloc-scope alloc-𝓁])

  ;;;; Set-based Σ

  ;; lookup-Σ : Σ Nam → (SetM (Setof StoBind))
  ;;          : Σ 𝓁   → (SetM (U Val ξ κ))
  (define (lookup-Σ Σ k)
    (lift (hash-ref (Σ-tbl Σ) k ∅)))

  ;; update-Σ : Σ Nam (Setof StoBind) → Σ
  ;;          : Σ 𝓁   (U Val ξ κ)     → Σ
  (define (update-Σ Σ₀ k v)
    (Σ (Σ-size Σ₀) (hash-update (Σ-tbl Σ₀) k (λ (vs) (set-add vs v)) ∅))))
