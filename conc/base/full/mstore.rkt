#lang racket
(require
 racket/match
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ bind^ mstore^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../bind-unit.rkt" bind@)
 ;; partially reused from conc/base/phases
 (only-in "../phases/mstore.rkt" [mstore@ phases:mstore@]))
(provide mstore@)

(define-unit mstore/bind@
  (import
   (only terms^
         TStop%)
   (only menv^
         lookup-ξ)
   (prefix b: (only bind^
                    bind resolve))
   (prefix phases: (only mstore^
                         init-Σ lookup-Σ alloc-name alloc-scope)))
  (export mstore^)

  (use-terms TStop)

  (define init-Σ      phases:init-Σ)
  (define lookup-Σ    phases:lookup-Σ)
  (define alloc-name  phases:alloc-name)
  (define alloc-scope phases:alloc-scope)

  (define bind        b:bind)
  (define resolve     b:resolve)

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (resolve #:phase ph id Σ)])
      (and (eq? nam nam0) (not (TStop? (lookup-ξ ξ nam)))))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export msto)
  (link (() bind@ msto)
        (([pmsto : mstore^]) phases:mstore@)
        (([msto  : mstore^]) mstore/bind@ pmsto)))
