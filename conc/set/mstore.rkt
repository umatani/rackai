#lang racket
(require
 (except-in racket do)
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" syntax^ menv^ mstore^)
 (only-in "../base/core/terms.rkt" terms^ #%term-forms)

 ;; partially reused from conc/base
 (rename-in "../base/mstore.rkt" [mstore@ base:mstore@]))
(provide mstore@)

(define-unit mstore/super@
  (import (only terms^
                Sym% Stx% Σ%)
          (prefix base: (only mstore^
                              init-Σ alloc-name alloc-scope)))
  (export mstore^)

  (use-terms Sym Stx Σ)

  (define init-Σ      base:init-Σ)
  (define alloc-name  base:alloc-name)
  (define alloc-scope base:alloc-scope)

  ;; Set-based Σ

  ; lookup-Σ : Σ Nam -> (SetM (U (Setof StoBind) Val ξ))
  (define (lookup-Σ Σ0 nam)
    (lift (hash-ref (Σ-tbl Σ0) nam (λ () (set))))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export msto)
  (link (([smsto : mstore^]) base:mstore@)
        (([msto  : mstore^]) mstore/super@ smsto)))
