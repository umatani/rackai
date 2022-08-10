#lang racket
(require
 (except-in racket do)
 "../nondet.rkt"
 "../mix.rkt"
 
 (only-in "../term.rkt" use-terms)
 
 (only-in "../signatures.rkt" syntax^ mstore^)
 (only-in "../config.rkt" config^ #%term-forms)
 (rename-in "../interp-base/units.rkt" [mstore@ base:mstore@]))
(provide mstore@)

(define-mixed-unit mstore@
  (import (only config^
                Σ%))
  (export mstore^)
  (inherit [base:mstore@ init-Σ alloc-name alloc-𝓁])

  (use-terms Σ)

  ;; Set-based Σ

  ; lookup-Σ : Σ Nam -> (SetM (Setof StoBind))
  ;          : Σ 𝓁   -> (SetM (U Val ξ κ))
  (define (lookup-Σ Σ0 k)
    (lift (hash-ref (Σ-tbl Σ0) k (λ () (set)))))

  ; update-Σ : Σ Nam (Setof StoBind) -> Σ
  ;          : Σ 𝓁   (U Val ξ κ)     -> Σ
  (define (update-Σ Σ0 k v)
    (Σ (Σ-size Σ0)
      (hash-update (Σ-tbl Σ0) k
                   (λ (old) (set-add old v)) (set)))))
