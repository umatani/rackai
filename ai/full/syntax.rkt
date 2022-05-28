#lang racket
(require "../../interp/full/struct.rkt"
         (only-in "../../interp/core/expand.rkt" lookup-ξ)

         ;; Abstract version
         (only-in "../phases/syntax.rkt" resolve))
(provide (all-defined-out))

;(: resolve* : Ph (Listof Id) Σ -> (Setof (Listof Nam)))
(define ((resolve*/resolve resolve) ph val Σ)
  (match val
    ['() (set '())]
    [(cons id val2)
     (for*/set ([nam  (in-set (resolve ph id Σ))]
                [nams (in-set ((resolve*/resolve resolve) ph val2 Σ))])
       (cons nam nams))]))

(define (id=? ph id nam ξ Σ)
  (let ([nam0 (resolve ph id Σ)])
    (and (subset? (set nam) nam0) (not (TStop? (lookup-ξ ξ nam))))))
