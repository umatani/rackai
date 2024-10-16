#lang racket
(require
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../../misc.rkt" subtract)
 (prefix-in common: "../../syntax.rkt"))
(provide syntax@)

(define-unit syntax@
  (import)
  (export syntax^)

  ;; ----------------------------------------
  ;; Syntax-object operations:

  (define zip   common:zip)
  (define unzip common:unzip)
  (define strip common:strip)

  ;; empty-ctx : → (HashTable Ph Scps) 
  (define (empty-ctx) (make-immutable-hash))

  ;; in-hole : Stx Stx → Stx
  (define (in-hole stx x)
    (match stx
      [(Stxξ ph stx ξ scps)    ; added
       (Stxξ ph (in-hole stx x) ξ scps)]
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx x)
                  (common:in-hole-stl in-hole stl x))
            ctx)]
      [(Hole) x]
      [_      stx]))

  ;; at-phase : Ctx Ph → Scps
  (define (at-phase ctx ph)
    (hash-ref ctx ph (set)))

  ;; update-ctx : Ctx Ph Scps → Ctx
  ;;   Updates the mapping of a `ctx` at a particular phase
  (define (update-ctx ctx ph scps)
    (hash-set ctx ph scps))

  ;; add : Ph Stx Scp → Stx
  ;;   Similar to one-phase `add`, but must update context at a given phase
  (define (add ph stx scp)
    (common:map-ctx stx (λ (ctx)
                          (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))))

  ;; flip : Ph Stx Scp → Stx
  ;;   Similar to one-phase `flip`, but must update context at a given phase
  (define (flip ph stx scp)
    (common:map-ctx stx
                    (λ (ctx)
                      (update-ctx ctx ph (common:⊕ scp (at-phase ctx ph))))))

  ;; prune : Ph Stx Scps → Stx
  ;;   Recursively removes a set of scopes from a syntax object at a given phase
  (define (prune ph stx scps)
    (common:map-ctx stx
                    (λ (ctx)
                      (update-ctx ctx ph (subtract (at-phase ctx ph) scps)))))
  )
