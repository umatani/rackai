#lang racket
(require (only-in "../core/syntax.rkt" in-hole-stl)
         (only-in "../phases/syntax.rkt" resolve)
         "struct.rkt")
(provide in-hole resolve*)

;(: in-hole : Stx Stx -> Stx)
(define (in-hole stx v)
  (match stx
    [(Stxξ ph stx ξ) (Stxξ ph (in-hole stx v) ξ)] ; remove scps
    [(GenStx (? Atom? atom) ctx) (GenStx atom ctx)]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
    [(Hole) v]
    [_ stx]))

;(: resolve* : Ph (Listof Id) Σ -> (Listof Nam))
(define (resolve* ph val Σ)
  (match val
    ['() '()]
    [(cons id val2) (cons (resolve ph id Σ) (resolve* ph val2 Σ))]))
