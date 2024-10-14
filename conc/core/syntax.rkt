#lang racket
(require
 (only-in "../../mix.rkt"   define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt"
 (only-in    "../units.rkt" [syntax@ super:syntax@]))
(provide syntax@)

(define-mixed-unit syntax@
  (import domain^)
  (export syntax^)
  (inherit [super:syntax@ zip unzip in-hole-stl
                          alloc-scope addremove strip subtract union
                          binding-lookup biggest-subset])
  ;; not used in core
  (define (at-phase   . _args) (error "must not be used"))
  (define (prune      . _args) (error "must not be used"))
  (define (update-ctx . _args) (error "must not be used"))

  ;; ----------------------------------------
  ;; stx utils

  (define (empty-ctx) (set))

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx]))

  ;; ----------------------------------------
  ;; Syntax-object operations:

  ;; Simply pushes scopes down through a syntax object
  ; add : Stx Scp -> Stx
  (define (add stx scp)
    (match stx
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (add stx scp) (add-stl stl scp))
            (set-add ctx scp))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (set-add ctx scp))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (set-add ctx scp))]))

  ; add-stl : Stl Scp -> Stl
  (define (add-stl stl scp)
    (match stl
      [(Null) (Null)]
      [(? stx? stx) (add stx scp)]
      [(Pair stx stl) (Pair (add stx scp) (add-stl stl scp))]))

  ;; Pushes flipping a scope down through a syntax object
  ; flip : Stx Scp -> Stx
  (define (flip stx scp)
    (match stx
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (flip stx scp) (flip-stl stl scp))
            (addremove scp ctx))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (addremove scp ctx))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (addremove scp ctx))]))

  ; flip-stl : Stl Scp -> Stl
  (define (flip-stl stl scp)
    (match stl
      [(Null) (Null)]
      [(? stx? stx) (flip stx scp)]
      [(Pair stx stl) (Pair (flip stx scp) (flip-stl stl scp))])))
