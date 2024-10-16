#lang racket
(require
 "../../signatures.rkt"
 "terms.rkt"
 (prefix-in common: "../../syntax.rkt"))
(provide syntax@)

(define-unit syntax@
  (import)
  (export syntax^)

  ;; ----------------------------------------
  ;; Syntax-object operations:

  ;; not used in core
  (define (at-phase   . _args) (error "must not be used"))
  (define (prune      . _args) (error "must not be used"))
  (define (update-ctx . _args) (error "must not be used"))

  (define zip      common:zip)
  (define unzip    common:unzip)
  (define strip    common:strip)

  ;; empty-ctx : → Scps
  (define (empty-ctx) (set))

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
  )
