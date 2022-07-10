#lang racket
(require
 "../../../set.rkt"
 "../../../nondet.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ mstore^ bind^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 (only-in "../units.rkt" [bind@ super:bind@]))
(provide bind@)

(define-mixed-unit bind@
  (import (only terms^
                TStop%)
          (only menv^
                lookup-ξ))
  (export bind^)
  (inherit [super:bind@ bind resolve])

  (use-terms TStop)

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (car (do (resolve #:phase ph id Σ)))])
      (and (subset? (set nam) nam0) (not (TStop? (lookup-ξ ξ nam)))))))