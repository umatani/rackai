#lang racket
(require
 "../../set.rkt"
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" terms-extra^ syntax^)
 (only-in "../../terms.rkt"
          Atom% Stx% Null% Pair% Hole%
          prim?
          #%term-forms)
 (only-in "../units.rkt" [syntax@ super:syntax@])
 (only-in "config.rkt" config^))
(provide syntax@)

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@ zip unzip in-hole-stl
            alloc-scope addremove strip subtract union
            binding-lookup biggest-subset])

  (use-terms Atom Stx Null Pair Hole)

  ;; not used in core
  (define (at-phase .   args) (error "must not be used"))
  (define (prune    .   args) (error "must not be used"))
  (define (update-ctx . args) (error "must not be used"))

  ;; ----------------------------------------
  ;; stx utils

  (define (empty-ctx) (set))

  ; in-hole : Stx Stx -> Stl
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
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (add stx scp) (add-stl stl scp))
            (set-add ctx scp))]
      [(Stx (? Atom? atom) ctx) (Stx atom (set-add ctx scp))]
      [(Stx (? prim? prim) ctx) (Stx prim (set-add ctx scp))]
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
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (flip stx scp) (flip-stl stl scp))
            (addremove scp ctx))]
      [(Stx (? Atom? atom) ctx)
       (Stx atom (addremove scp ctx))]
      [(Stx (? prim? prim) ctx)
       (Stx prim (addremove scp ctx))]
      [(Pair stx stl) (Pair (flip stx scp) (flip-stl stl scp))])))
