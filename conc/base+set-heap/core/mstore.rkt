#lang racket
(require
 (except-in racket do)
 "../../../nondet.rkt"
 (only-in "../../../term.rkt" use-terms)
 
 (only-in "../../../signatures.rkt"
          syntax^ menv^ mstore^)
 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)

 ;; partially reused from conc/base/core
 (rename-in "../../base/core/mstore.rkt" [mstore@ base:mstore@]))
(provide mstore@)

(define-unit mstore/super@
  (import (only terms^
                Sym% Stx% Σ%)
          (prefix base: (only mstore^
                              init-Σ)))
  (export mstore^)

  (use-terms Sym Stx Σ)

  (define init-Σ base:init-Σ)

  ;; Set-based Σ

  ; lookup-Σ : Σ Nam -> (SetM (U (Setof StoBind) Val ξ))
  (define (lookup-Σ Σ0 nam)
    (lift (hash-ref (Σ-tbl Σ0) nam (λ () (set)))))

  ;; Finite-domain allocation

  ; alloc-name : Id Σ -> (Values Nam Σ)
  (define (alloc-name id Σ0)
    (match-let ([(Stx (Sym nam) _) id]
                [(Σ size tbl) Σ0])
      (values (string->symbol (format "~a:~a" nam size))
              (Σ (add1 size) tbl))))

  ; alloc-scope : Symbol Σ -> (Values Scp Σ)
  (define (alloc-scope s Σ0)
    (match-let ([(Σ size tbl) Σ0])
      (values (string->symbol (format "~a::~a" s size))
              (Σ (add1 size) tbl)))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export msto)
  (link (([smsto : mstore^]) base:mstore@)
        (([msto  : mstore^]) mstore/super@ smsto)))
