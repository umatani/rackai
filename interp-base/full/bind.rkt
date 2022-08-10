#lang racket
(require
 racket/match
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ menv^ mstore^ bind^)

 (only-in "../units.rkt" [bind@ super:bind@])
 (only-in "config.rkt" config^ #%term-forms))
(provide bind@)


(define-mixed-unit bind@
  (import (only config^
                TStop%)
          (only menv^
                lookup-ξ))
  (export bind^)
  (inherit [super:bind@ bind resolve])

  (use-terms TStop)

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (resolve #:phase ph id Σ)])
      (and (eq? nam nam0) (not (TStop? (lookup-ξ ξ nam)))))))
