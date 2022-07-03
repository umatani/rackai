#lang racket
(require
 racket/match
 "../../../set.rkt"
 (only-in "../../../term.rkt" use-terms)
 (only-in "../../../dprint.rkt" dprint)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ bind^ mstore^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../bind-unit.rkt" bind@))
(provide mstore@)

(define-unit mstore/bind@
  (import (only terms^
                Sym% Stx% Σ%)
          (only syntax^
                add biggest-subset binding-lookup)
          (only menv^
                extend-ξ)
          (prefix b: (only bind^
                           bind resolve id=?)))
  (export mstore^)

  (use-terms Sym Stx Σ)


  ;; ----------------------------------------
  ;; Expand-time store operations:

  ; init-Σ : -> Σ
  (define (init-Σ) (Σ 0 (make-immutable-hash)))

  ;; lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ)
  (define (lookup-Σ Σ0 nam)
    (hash-ref (Σ-tbl Σ0) nam (λ () (set))))

  (define bind    b:bind)
  (define resolve b:resolve)
  (define id=?    b:id=?)

  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

  ; alloc-name : Id Σ -> (Values Nam Σ)
  (define (alloc-name id Σ0)
    (dprint 'core 'alloc-name "")
    (match-let ([(Stx (Sym nam) _) id]
                [(Σ size tbl) Σ0])
      (values (string->symbol (format "~a:~a" nam size))
              (Σ (add1 size) tbl))))

  ; alloc-scope : Symbol Σ -> (Values Scp Σ)
  (define (alloc-scope s Σ0)
    (dprint 'core 'alloc-scope "")
    (match-let ([(Σ size tbl) Σ0])
      (values (string->symbol (format "~a::~a" s size))
              (Σ (add1 size) tbl)))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export mstore^)
  (link   bind@ mstore/bind@))
