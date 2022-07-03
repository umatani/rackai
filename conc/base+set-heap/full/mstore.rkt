#lang racket
(require
 "../../../set.rkt"
 "../../../nondet.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ bind^ mstore^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 ;; partially reused from conc/base+set-heap/phases
 (only-in "../phases/mstore.rkt" [mstore@ phases:mstore@])

 (only-in "../bind-unit.rkt" bind@))
(provide mstore@)

(define-unit mstore/bind@
  (import
   (only terms^
         TStop%)
   ;; from conc/base/core
   (only menv^
         lookup-ξ)
   (prefix b: (only bind^
                    bind resolve))
   ;; from conc/base+set-heap/phases
   (prefix phases: (only mstore^
                         init-Σ lookup-Σ bind alloc-name alloc-scope)))
  (export mstore^)

  (use-terms TStop)

  (define init-Σ      phases:init-Σ)
  (define lookup-Σ    phases:lookup-Σ)
  (define alloc-name  phases:alloc-name)
  (define alloc-scope phases:alloc-scope)

  (define bind        b:bind)
  (define resolve     b:resolve)

  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (let ([nam0 (car (do (resolve #:phase ph id Σ)))])
      (and (subset? (set nam) nam0) (not (TStop? (lookup-ξ ξ nam)))))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export msto)
  (link (() bind@ msto)
        (([pmsto : mstore^]) phases:mstore@)
        (([msto  : mstore^]) mstore/bind@ pmsto)))
