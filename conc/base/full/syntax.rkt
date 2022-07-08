#lang racket
(require
 racket/match
 "../../../mix.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt" terms-extra^ syntax^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../units.rkt"        [syntax@ super:syntax@])
 (only-in "../phases/units.rkt" [syntax@ phases:syntax@]))
(provide syntax@)

(define-mixed-unit syntax@
  (import (only terms^
                Stx% Stxξ% Hole%)
          (only terms-extra^
                atom?))
  (export syntax^)
  (inherit [super:syntax@  stl->seq zip unzip snoc in-hole-stl
                           addremove strip subtract union
                           binding-lookup biggest-subset]
           [phases:syntax@ empty-ctx add add-stl flip flip-stl
                           at-phase update-ctx prune])
  (use-terms Stx Stxξ Hole)

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stxξ ph stx ξ) (Stxξ ph (in-hole stx v) ξ)] ; remove scps
      [(Stx (? atom? atom) ctx) (Stx atom ctx)]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx])))
