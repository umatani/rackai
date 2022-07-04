#lang racket
(require
 "../../../set.rkt"
 "../../../nondet.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ mstore^ bind^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 (only-in "../bind-unit.rkt" [bind@ super:bind@]))
(provide bind@)

(define-unit bind/super@
  (import
   (only terms^
         TStop%)
   (only menv^
         lookup-ξ)
   (prefix super: (only bind^
                        bind resolve)))
  (export bind^)

  (use-terms TStop)

  (define bind      super:bind)
  (define resolve   super:resolve)

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (car (do (resolve #:phase ph id Σ)))])
      (and (subset? (set nam) nam0) (not (TStop? (lookup-ξ ξ nam)))))))

(define-compound-unit/infer bind@
  (import terms^ syntax^ menv^ mstore^)
  (export b)
  (link (([sb : bind^]) super:bind@)
        (([b  : bind^]) bind/super@ sb)))
