#lang racket
(require "../../base/nondet.rkt"
         "../../base/full/struct.rkt"
         (only-in "../../base/core/expand.rkt" lookup-ξ)

         ;; Set-based version
         (only-in "../phases/syntax.rkt" resolve))
(provide (all-defined-out))

;(: resolve* : Ph (Listof Id) Σ -> (SetM (Listof Nam)))
(define ((resolve*/resolve resolve) ph val Σ)
  (match val
    ['() (pure '())]
    [(cons id val2)
     (do nam  <- (resolve ph id Σ)
         nams <- ((resolve*/resolve resolve) ph val2 Σ)
         (pure (cons nam nams)))
     #;
     (for*/set ([nam  (in-set (resolve ph id Σ))]
                [nams (in-set ((resolve*/resolve resolve) ph val2 Σ))])
       (cons nam nams))]))

(define (id=? ph id nam ξ Σ)
  (let ([nam0 (car (do (resolve ph id Σ)))])
    (and (subset? (set nam) nam0) (not (TStop? (lookup-ξ ξ nam))))))
