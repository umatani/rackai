#lang racket
(require
 racket/match racket/dict
 "../../../set.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt" terms-extra^ syntax^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../units.rkt" [syntax@ super:syntax@]))
(provide syntax@)


(define-mixed-unit syntax@
  (import (only terms^
                Stx% Stxξ% Hole%)
          (only terms-extra^
                atom?))
  (export syntax^)
  (inherit [super:syntax@ addremove strip subtract union in-hole-stl
                          binding-lookup biggest-subset
                          stl->seq snoc zip unzip])

  (use-terms Stx Stxξ Hole)

  (define (empty-ctx) (make-immutable-hash))

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stxξ ph stx1 ξ scps) (Stxξ ph (in-hole stx1 v) ξ scps)] ; added
      [(Stx (? atom? atom) ctx) (Stx atom ctx)]
      [(Stx (cons stx1 stl) ctx)
       (Stx (cons (in-hole stx1 v) (in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx]))

  ;; ----------------------------------------
  ;; Syntax-object operations:

  ; at-phase : Ctx Ph -> Scps
  (define (at-phase ctx ph)
    (dict-ref ctx ph (λ () (set))))

  ;; Updates the mapping of a `ctx` at a particular phase
  ; update-ctx : Ctx Ph Scps -> Ctx
  (define (update-ctx ctx ph scps)
    (dict-set ctx ph scps))

  ;; Similar to one-phase `add`, but must update context
  ;; at a given phase
  ; add : Ph Stx Scp -> Stx
  (define (add ph stx scp)
    (match stx
      [(Stx (cons stx stl) ctx)
       (Stx (cons (add ph stx scp) (add-stl ph stl scp))
            (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]))

  ; add-stl : Ph Stl Scp -> Stl
  (define (add-stl ph stl scp)
    (match stl
      ['() '()]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (add ph stx scp) (add-stl ph stl scp))
            (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(cons stx stl) (cons (add ph stx scp) (add-stl ph stl scp))]))


  ;; Similar to one-phase `flip`, but must update context
  ;; at a given phase
  ; flip : Ph Stx Scp -> Stx
  (define (flip ph stx scp)
    (match stx
      [(Stx (cons stx stl) ctx)
       (Stx (cons (flip ph stx scp) (flip-stl ph stl scp))
            (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]))

  ; flip-stl : Ph Stl Scp -> Stl
  (define (flip-stl ph stl scp)
    (match stl
      ['() '()]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (flip ph stx scp) (flip-stl ph stl scp))
            (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(cons stx stl) (cons (flip ph stx scp) (flip-stl ph stl scp))]))

  ;; Recursively removes a set of scopes from a syntax object
  ;; at a given phase
  ; prune : Ph Stx Scps -> Stx
  (define (prune ph stx scps_p)
    (match stx
      [(Stx (cons stx stl) ctx)
       (Stx (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))
            (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (subtract
                                     (at-phase ctx ph) scps_p)))]))

  ; prune-stl : Ph Stl Scps -> Stl
  (define (prune-stl ph stl scps_p)
    (match stl
      ['() '()]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))
            (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(cons stx stl) (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))])))
