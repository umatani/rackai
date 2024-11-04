#lang racket/base
(require
 racket/unit
 (only-in racket/match          match)
 (only-in "../../mix.rkt"       define-mixed-unit inherit)
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../../syntax.rkt"    in-hole-stl)
 (only-in "../phases/units.rkt" [syntax@ phases:syntax@]))
(provide syntax@)


(define-mixed-unit syntax@
  (import)
  (export  syntax^)
  (inherit [phases:syntax@ empty-ctx add flip proper-stl?])

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stxξ ph stx ξ) (Stxξ ph (in-hole stx v) ξ)] ; remove scps
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx])))
