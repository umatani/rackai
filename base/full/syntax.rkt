#lang racket
(require
 (only-in "../../mix.rkt"       define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt"
 (prefix-in common: "../../syntax.rkt")
 (only-in "../phases/units.rkt" [syntax@ phases:syntax@]))
(provide syntax@)


(define-mixed-unit syntax@
  (import)
  (export  syntax^)
  (inherit [phases:syntax@ empty-ctx add flip at-phase update-ctx prune])

  (define zip   common:zip)
  (define unzip common:unzip)
  (define strip common:strip)

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stxξ ph stx ξ) (Stxξ ph (in-hole stx v) ξ)] ; remove scps
      [(Stx (Pair stx stl) ctx)
       (Stx (Pair (in-hole stx v) (common:in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx])))
