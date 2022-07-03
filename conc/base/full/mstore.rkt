#lang racket
(require
 racket/match
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ resolve^ mstore^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../resolve-unit.rkt" resolve@)
 ;; partially reused from conc/base/phases
 (only-in "../phases/mstore.rkt" [mstore@ phases:mstore@]))
(provide mstore@)

(define-unit mstore/resolve@
  (import
   (only terms^
         TStop%)
   (only menv^
         lookup-ξ)
   (prefix r: (only resolve^
                    resolve))
   (prefix phases: (only mstore^
                         init-Σ lookup-Σ bind alloc-name alloc-scope)))
  (export mstore^)

  (use-terms TStop)

  (define init-Σ      phases:init-Σ)
  (define lookup-Σ    phases:lookup-Σ)
  (define bind        phases:bind)
  (define alloc-name  phases:alloc-name)
  (define alloc-scope phases:alloc-scope)

  (define resolve     r:resolve)

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (resolve #:phase ph id Σ)])
      (and (eq? nam nam0) (not (TStop? (lookup-ξ ξ nam)))))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export msto)
  (link (([pmsto : mstore^]) phases:mstore@)
        (()                  resolve@        msto)
        (([msto  : mstore^]) mstore/resolve@ pmsto)))
