#lang racket/base
(require
 racket/unit
 (only-in racket/match        match)
 (only-in "../../set.rkt"     set-add)
 (only-in "../../mix.rkt"     define-mixed-unit inherit)
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../../misc.rkt"    subtract)
 (only-in "../../syntax.rkt"  map-ctx in-hole-stl at-phase update-ctx ⊕
          )
 (only-in "../core/units.rkt" [syntax@ core:syntax@]))
(provide syntax@)


(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [core:syntax@    proper-stl?])

  ;; ----------------------------------------
  ;; Syntax-object operations:

  ;; empty-ctx : → (HashTable Ph Scps) 
  (define (empty-ctx) (make-immutable-hash))

  ;; in-hole : Stx Stx → Stx
  (define (in-hole stx x)
    (match stx
      [(Stxξ ph stx ξ scps)    ; added
       (Stxξ ph (in-hole stx x) ξ scps)]
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx x)
                  (in-hole-stl in-hole stl x))
            ctx)]
      [(Hole) x]
      [_      stx]))

  ;; add : Ph Stx Scp → Stx
  ;;   Similar to one-phase `add`, but must update context at a given phase
  (define (add ph stx scp)
    (map-ctx stx (λ (ctx)
                   (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))))

  ;; flip : Ph Stx Scp → Stx
  ;;   Similar to one-phase `flip`, but must update context at a given phase
  (define (flip ph stx scp)
    (map-ctx stx
             (λ (ctx)
               (update-ctx ctx ph (⊕ scp (at-phase ctx ph))))))

  ;; prune : Ph Stx Scps → Stx
  ;;   Recursively removes a set of scopes from a syntax object at a given phase
  (define (prune ph stx scps)
    (map-ctx stx
             (λ (ctx)
               (update-ctx ctx ph (subtract (at-phase ctx ph) scps)))))
  )
