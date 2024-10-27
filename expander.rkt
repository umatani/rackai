#lang racket/base
(require
 racket/unit
 (only-in racket/match            match-let)
 (only-in "nondet.rkt"            do <- pure lift)
 (only-in "reduction.rkt"         apply-reduction*)
 (only-in "set.rkt"               set ∅)
 "signatures.rkt"
 (only-in "terms.rkt"             [Stxξ c:Stxξ] [ζ c:ζ])
 (only-in "base/phases/terms.rkt" [Stxξ p:Stxξ] [ζ p:ζ])
 (only-in "base/full/terms.rkt"   [Stxξ f:Stxξ] [ζ f:ζ] [Σ̂ f:Σ̂] InEval?))
(provide core-expander@ phases-expander@ full-expander@)

;; expander : δ Stx → (SetM (Cons Stx Σ))

(define-unit core-expander@
  (import (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    ==>))
  (export expander^)

  ;; expand : δ Stx ξ Σ → (SetM (Cons Stx Σ))
  (define (expand δ stx ξ Σ)
    (define ==>δ (==> δ))
    (define ζᵢ (c:ζ (c:Stxξ stx ξ) '● Σ))

    (do (c:ζ stx′ '● Σ′) <- (apply-reduction* ==>δ ζᵢ)
        (pure (cons stx′ Σ′))))

  (define (expander δ stx)
    (expand δ stx (init-ξ) (init-Σ))))


(define-unit phases-expander@
  (import (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    ==>))
  (export expander^)

  ;; expand : δ Ph Stx ξ Scps Σ → (SetM (Cons Stx Σ))
  (define (expand δ ph stx ξ scpsₚ Σ)
    (define ==>δ (==> δ))
    (define ζᵢ   (p:ζ (p:Stxξ ph stx ξ scpsₚ) '● Σ))

    (do (p:ζ stx′ '● Σ′) <- (apply-reduction* ==>δ ζᵢ)
        (pure (cons stx′ Σ′))))
  
  (define (expander δ stx)
    (expand δ 0 stx (init-ξ) ∅ (init-Σ))))


;(require "base/full/terms.rkt")

(define-unit full-expander@
  (import (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    ==>))
  (export expander^)

  ;; expand : δ Ph Stx ξ Σ̂ → (SetM (Cons Stx Σ̂))
  (define (expand δ ph stx ξ Σ̂)
    (define ==>δ (==> δ))
    (define ζᵢ   (f:ζ (f:Stxξ ph stx ξ) '● Σ̂))

    (do ζ′ <- (apply-reduction* (==>δ) ζᵢ)
        ;; mult の場合のみ stuck が生じる．
        ;; その原因は，set-box!とbind-syntaxesがstoreへのassignmentで
        ;; あることによりstore中の値の多重化が生じること．
        (f:ζ stx′ '● Σ̂′) <- (if (not (InEval? ζ′))
                              (pure ζ′)
                              (lift ∅))
        (pure (cons stx′ Σ̂′))))

  (define (expander δ stx)
    (do (cons stx′ (f:Σ̂ Σ _scpsₚ _scpsᵤ)) <- (expand δ 0 stx (init-ξ)
                                                     (f:Σ̂ (init-Σ) ∅ ∅))
        (pure (cons stx′ Σ)))))
