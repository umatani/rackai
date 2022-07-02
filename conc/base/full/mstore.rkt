#lang racket
(require
 racket/match
 (only-in "../../../term.rkt"        use-terms)
 (only-in "terms.rkt"                terms^ #%term-forms)
 (only-in "../../../syntax-sig.rkt"  syntax^)
 (only-in "../../../menv-sig.rkt"    menv^)
 (only-in "../../../resolve-sig.rkt" resolve^)
 (only-in "../../../mstore-sig.rkt"  mstore^)
 (only-in "../../../phase-sig.rkt"   phase^)

 (only-in "../resolve-unit.rkt"      resolve@)
 ;; partially reused from conc/base/phases
 (only-in "../phases/mstore.rkt"     [mstore@ phases:mstore@]))
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

(define-compound-unit mstore@
  (import [t : terms^] [stx : syntax^] [me : menv^] [ph : phase^])
  (export msto)
  (link (([pmsto : mstore^]) phases:mstore@  t stx me ph)
        (([r    : resolve^]) resolve@        t stx me msto ph)
        (([msto : mstore^])  mstore/resolve@ t me r pmsto)))
