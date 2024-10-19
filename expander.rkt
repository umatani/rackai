#lang racket/base
(require
 racket/unit
 (only-in racket/match match-let)
 (only-in "nondet.rkt" do <- pure)
 (only-in "set.rkt" set ∅)
 "signatures.rkt")
(provide core-expander@ phases-expander@
         base-full-expander@ mult-full-expander@)

;; expander : δ Stx →       (Cons Stx Σ)        (base)
;;          : δ Stx → (SetM (Cons Stx Σ))       (mult)

(define-unit core-expander@
  (import (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    expand))
  (export expander^)

  (define (expander δ stx)
    (expand δ stx (init-ξ) (init-Σ))))


(define-unit phases-expander@
  (import (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    expand))
  (export expander^)

  (define (expander δ stx)
    (expand δ 0 stx (init-ξ) ∅ (init-Σ))))


(require "base/full/terms.rkt")

(define-unit base-full-expander@
  (import (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    expand))
  (export expander^)

  (define (expander δ stx)
    (match-let ([(cons stx′ (Σ* Σ _ _))
                 (expand δ 0 stx (init-ξ) (Σ* (init-Σ) ∅ ∅))])
      (cons stx′ Σ))))

(define-unit mult-full-expander@
  (import (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    expand))
  (export expander^)

  (define (expander δ stx)
    (do (cons stx′ (Σ* Σ _ _)) <- (expand δ 0 stx (init-ξ)
                                          (Σ* (init-Σ) ∅ ∅))
        (pure (cons stx′ Σ)))))
