#lang racket
(require
 (except-in racket do)
 "../../nondet.rkt"
 "../../mix.rkt"
 
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" config^ syntax^ menv^ mstore^)
 (only-in "../../config.rkt" config^ #%term-forms)
 ;; partially reused from conc/base
 (rename-in "../base/units.rkt" [mstore@ base:mstore@]))
(provide mstore@)

(define-mixed-unit mstore@
  (import (only config^
                Σ%))
  (export mstore^)
  (inherit [base:mstore@ init-Σ alloc-name alloc-scope alloc-𝓁])

  (use-terms Σ)

  ;; Set-based Σ

  ; lookup-Σ : Σ Nam -> (SetM (U (Setof StoBind) Val ξ κ))
  (define (lookup-Σ Σ0 nam)
    (lift (hash-ref (Σ-tbl Σ0) nam (λ () (set)))))

  ; update-Σ : Σ Nam (U (Setof StoBind) Val ξ κ) -> Σ
  (define (update-Σ Σ0 nam u)
    (Σ (Σ-size Σ0)
      (hash-update (Σ-tbl Σ0) nam
                   (λ (old) (set-add old u)) (set)))))
