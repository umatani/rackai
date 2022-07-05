#lang racket
(require
 racket/match
 "../../set.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt" syntax^ menv^ mstore^)
 (only-in "../../terms.rkt" terms^ #%term-forms))
(provide mstore@)

(define-unit mstore@
  (import (only terms^
                Sym% Stx% 𝓁% Σ%)
          (only syntax^
                add biggest-subset binding-lookup)
          (only menv^
                extend-ξ))
  (export mstore^)

  (use-terms Sym Stx 𝓁 Σ)

  ;; ----------------------------------------
  ;; Expand-time store operations:

  ; init-Σ : -> Σ
  (define (init-Σ) (Σ 0 (make-immutable-hash)))

  ;; lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ κ)
  (define (lookup-Σ Σ0 nam)
    (hash-ref (Σ-tbl Σ0) nam (λ () (set))))

  ; update-Σ : Σ Nam (U (Setof StoBind) Val ξ κ) -> Σ
  (define (update-Σ Σ0 nam u)
    (Σ (Σ-size Σ0)
      (hash-set (Σ-tbl Σ0) nam u)))

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
              (Σ (add1 size) tbl))))

  ; alloc-𝓁 : Σ -> (Values 𝓁 Σ)
  (define (alloc-𝓁 Σ0)
    (match-let ([(Σ size tbl) Σ0])
      (values (𝓁 (string->symbol (format "𝓁~a" size)))
              (Σ (add1 size) tbl)))))
