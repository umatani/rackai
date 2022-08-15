#lang racket
(require
 racket/match racket/dict
 "../../set.rkt"
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt" syntax^)
 (only-in "terms.rkt" #%term-forms
          Atom% Stx% Null% Pair% Hole%
          Stxξ%
          prim?)
 (only-in "../units.rkt" [syntax@ super:syntax@]))
(provide syntax@)

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@ addremove strip subtract union in-hole-stl
                          alloc-scope binding-lookup biggest-subset zip unzip])

  (use-terms Atom Stx Null Pair Hole Stxξ)

  (define (empty-ctx) (make-immutable-hash))

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stxξ ph stx ξ scps) (Stxξ ph (in-hole stx v) ξ scps)] ; added
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
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
      [(Stx (Null) ctx)
       (Stx (Null) (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (add ph stx scp) (add-stl ph stl scp))
            (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]))

  ; add-stl : Ph Stl Scp -> Stl
  (define (add-stl ph stl scp)
    (match stl
      [(Stx (Null) ctx)
       (Stx (Null) (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (add ph stx scp) (add-stl ph stl scp))
            (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
      [(Null) (Null)]
      [(Pair stx stl) (Pair (add ph stx scp) (add-stl ph stl scp))]))


  ;; Similar to one-phase `flip`, but must update context
  ;; at a given phase
  ; flip : Ph Stx Scp -> Stx
  (define (flip ph stx scp)
    (match stx
      [(Stx (Null) ctx)
       (Stx (Null) (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (flip ph stx scp) (flip-stl ph stl scp))
            (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]))

  ; flip-stl : Ph Stl Scp -> Stl
  (define (flip-stl ph stl scp)
    (match stl
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (flip ph stx scp) (flip-stl ph stl scp))
            (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
      [(Null) (Null)]
      [(Pair stx stl) (Pair (flip ph stx scp) (flip-stl ph stl scp))]))

  ;; Recursively removes a set of scopes from a syntax object
  ;; at a given phase
  ; prune : Ph Stx Scps -> Stx
  (define (prune ph stx scps_p)
    (match stx
      [(Stx (Null) ctx)
       (Stx (Null) (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (prune ph stx scps_p) (prune-stl ph stl scps_p))
            (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (subtract
                                     (at-phase ctx ph) scps_p)))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (update-ctx ctx ph (subtract
                                     (at-phase ctx ph) scps_p)))]))

  ; prune-stl : Ph Stl Scps -> Stl
  (define (prune-stl ph stl scps_p)
    (match stl
      [(Stx (Null) ctx)
       (Stx (Null) (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (prune ph stx scps_p) (prune-stl ph stl scps_p))
            (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
      [(Null) (Null)]
      [(Pair stx stl) (Pair (prune ph stx scps_p) (prune-stl ph stl scps_p))])))
