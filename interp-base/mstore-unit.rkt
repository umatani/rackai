#lang racket/unit
(require
 racket/match
 (for-syntax racket)
 "../set.rkt"
 (only-in "../term.rkt" use-terms)

 (only-in "../signatures.rkt" config^ syntax^ menv^ mstore^)
 (only-in "../terms.rkt"
          Sym% Stx% 𝓁%
          [#%term-forms tm:#%term-forms])
 (only-in "../config.rkt" [#%term-forms cfg:#%term-forms]))

(import
 (only config^
       Σ%)
 (only syntax^
       add biggest-subset binding-lookup))
(export mstore^)

(define-syntax #%term-forms
  (append (syntax-local-value #'tm:#%term-forms)
          (syntax-local-value #'cfg:#%term-forms)))
(use-terms Sym Stx 𝓁 Σ)

;; ----------------------------------------
;; Expand-time store operations:

; init-Σ : -> Σ
(define (init-Σ) (Σ 0 (make-immutable-hash)))

; lookup-Σ : Σ Nam -> (Setof StoBind)
;          : Σ 𝓁   -> (U Val ξ κ)
(define (lookup-Σ Σ0 k)
  (hash-ref (Σ-tbl Σ0) k (λ () (set))))

; update-Σ : Σ Nam (Setof StoBind) -> Σ
;          : Σ 𝓁   (U Val ξ κ)     -> Σ
(define (update-Σ Σ0 k v)
  (Σ (Σ-size Σ0)
    (hash-set (Σ-tbl Σ0) k v)))

;; ----------------------------------------
;; Alloc name & 𝓁 helpers for expander:

; alloc-name : Id Σ -> (Values Nam Σ)
(define (alloc-name id Σ0)
  (match-let ([(Stx (Sym nam) _) id]
              [(Σ size tbl) Σ0])
    (values (string->symbol (format "~a:~a" nam size))
            (Σ (add1 size) tbl))))

; alloc-𝓁 : Stx Σ -> (Values 𝓁 Σ)
;   - called from push-κ
;   - called from alloc-def-ξ and alloc-box (full)
;   - stx is used in abs for ensuring finiteness of the domain
(define (alloc-𝓁 stx Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values ;(𝓁 (string->symbol (format "𝓁:~a:~a" stx size)))
     (𝓁 (cons stx size)) 
     (Σ (add1 size) tbl))))
