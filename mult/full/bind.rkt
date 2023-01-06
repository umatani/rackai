#lang racket
(require
 "../../set.rkt"
 "../../nondet.rkt"
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ menv^ mstore^ bind^)
 (only-in "../../conc/full/terms.rkt" #%term-forms
          TStop%)
 (only-in "../units.rkt" [bind@ super:bind@]))
(provide bind@)

(define-mixed-unit bind@
  (import (only menv^
                lookup-ξ))
  (export bind^)
  (inherit [super:bind@ bind resolve])
  (use-terms TStop)

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (results (resolve #:phase ph id Σ))])
      (and (subset? (set nam) nam0)
           (andmap (λ (at) (not (TStop? at)))
                   (set->list (results (lookup-ξ ξ nam))))))))