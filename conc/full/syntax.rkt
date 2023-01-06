#lang racket
(require
 racket/match
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt" domain^ syntax^)
 (only-in "terms.rkt" #%term-forms
          Stx% Pair% Hole% Stxξ%)
 (only-in "../units.rkt"        [syntax@ super:syntax@])
 (only-in "../phases/units.rkt" [syntax@ phases:syntax@]))
(provide syntax@)


(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@  zip unzip in-hole-stl
                           alloc-scope addremove strip subtract union
                           binding-lookup biggest-subset]
           [phases:syntax@ empty-ctx add add-stl flip flip-stl
                           at-phase update-ctx prune])
  (use-terms Stx Pair Hole Stxξ)

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stxξ ph stx ξ) (Stxξ ph (in-hole stx v) ξ)] ; remove scps
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx])))