#lang racket
(require
 racket/match
 "../../../set.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ mstore^)
 (only-in "terms.rkt" terms^ #%term-forms))
(provide mstore@)

(define-unit mstore@
  (import (only terms^
                Sym% Stx% Σ%)
          (only syntax^
                add biggest-subset binding-lookup)
          (only menv^
                extend-ξ))
  (export mstore^)

  (use-terms Sym Stx Σ)

  ;; ----------------------------------------
  ;; Expand-time store operations:

  ; init-Σ : -> Σ
  (define (init-Σ) (Σ 0 (make-immutable-hash)))

  ;; lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ)
  (define (lookup-Σ Σ0 nam)
    (hash-ref (Σ-tbl Σ0) nam (λ () (set))))

  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

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
