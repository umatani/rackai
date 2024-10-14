#lang racket
(require
 (only-in "../../mix.rkt" define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../units.rkt"  [bind@ super:bind@]))
(provide bind@)

(define-mixed-unit bind@
  (import  (only menv^ lookup-ξ))
  (export  bind^)
  (inherit [super:bind@ bind resolve])

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (resolve #:phase ph id Σ)])
      (and (eq? nam nam0) (not (TStop? (lookup-ξ ξ nam)))))))
