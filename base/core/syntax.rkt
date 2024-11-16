#lang racket/base
(require
 racket/unit
 (only-in racket/match    match)
 (only-in "../../set.rkt" ∅ set-add)
 "../../signatures.rkt"
 "terms.rkt"
 (prefix-in common: "../../syntax.rkt"))
(provide syntax@)

(define-unit syntax@
  (import
   (only  domain^    stx?))
  (export syntax^)

  ;; ----------------------------------------
  ;; Syntax-object operations:

  ;; empty-ctx : → Scps
  (define (empty-ctx) ∅)

  ;; in-hole : Stx Stx → Stx
  (define (in-hole stx x)
    (match stx
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx x)
                  (common:in-hole-stl in-hole stl x))
            ctx)]
      [(Hole) x]
      [_      stx]))

  ;; add : Stx Scp → Stx
  ;;   Simply pushes scopes down through a syntax object
  (define (add stx scp)
    (common:map-ctx stx (λ (ctx) (set-add ctx scp))))

  ;; flip : Stx Scp → Stx
  ;;   Pushes flipping a scope down through a syntax object
  (define (flip stx scp)
    (common:map-ctx stx (λ (ctx) (common:⊕ scp ctx))))

  ;; proper-stl? : Val → Boolean
  (define (proper-stl? x)
    (or (Null? x)
        (and (Pair? x) (stx? (Pair-a x)) (proper-stl? (Pair-d x)))))

  ;; prune : Ph Stx Scps → Stx
  (define (prune ph stx scps)
    (error 'prune "internal error: must not be used in core."))
  )
