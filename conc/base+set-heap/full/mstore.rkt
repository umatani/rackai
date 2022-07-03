#lang racket
(require
 "../../../set.rkt"
 "../../../nondet.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ resolve^ mstore^ phase^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 ;; partially reused from conc/base+set-heap/phases
 (only-in "../phases/mstore.rkt" [mstore@ phases:mstore@])

 (only-in "../resolve-unit.rkt"  resolve@))
(provide mstore@)

(define-unit mstore/resolve@
  (import
   (only terms^
         TStop%)
   ;; from conc/base/core
   (only menv^
         lookup-ξ)
   (prefix r: (only resolve^
                    resolve))
   ;; from conc/base+set-heap/phases
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
    (let ([nam0 (car (do (resolve #:phase ph id Σ)))])
      (and (subset? (set nam) nam0) (not (TStop? (lookup-ξ ξ nam)))))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^ phase^)
  (export msto)
  (link (([pmsto : mstore^]) phases:mstore@)
        (() resolve@ msto)
        (([msto  : mstore^]) mstore/resolve@ pmsto)))
#;
(define-compound-unit mstore@
  (import [t : terms^] [stx : syntax^] [me : menv^] [ph : phase^])
  (export msto)
  (link (([pmsto : mstore^]) phases:mstore@  t stx me ph)
        (([r    : resolve^]) resolve@        t stx msto ph)
        (([msto : mstore^])  mstore/resolve@ t me r ph pmsto)))
