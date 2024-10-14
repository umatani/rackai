#lang racket/unit
(require
 (only-in racket/match match-let)
 (only-in "../set.rkt" set)
 "../signatures.rkt"
 "../terms.rkt")

(import)
(export mstore^)

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
