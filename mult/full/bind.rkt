#lang racket
(require
 (only-in "../../nondet.rkt" results)
 (only-in "../../mix.rkt"    define-mixed-unit)
 "../../signatures.rkt"
 "../../conc/full/terms.rkt"
 (only-in "../units.rkt"     [bind@ super:bind@]))
(provide bind@)

(define-mixed-unit bind@
  (import  (only menv^    lookup-ξ))
  (export  bind^)
  (inherit [super:bind@ bind resolve])

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (results (resolve #:phase ph id Σ))])
      (and (subset? (set nam) nam0)
           (andmap (λ (at) (not (TStop? at)))
                   (set->list (results (lookup-ξ ξ nam))))))))
