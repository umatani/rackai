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
;;   Σ       ::= Nat × Nam → (Setof StoBind)
;;   StoBind ::= (StoBind Scps Nam)

;; init-Σ : → Σ
(define (init-Σ) (Σ 0 (make-immutable-hash)))

;; lookup-Σ : Σ Nam → (Setof StoBind)
;;          : Σ 𝓁   → (U Val ξ κ)
(define (lookup-Σ Σ₀ k)
  (hash-ref (Σ-tbl Σ₀) k (set)))

;; update-Σ : Σ Nam (Setof StoBind) → Σ
;;          : Σ 𝓁   (U Val ξ κ)     → Σ
(define (update-Σ Σ₀ k v)
  (Σ (Σ-size Σ₀)
    (hash-set (Σ-tbl Σ₀) k v)))

;; ----------------------------------------
;; Alloc name, scope, and 𝓁 for expander:

;; alloc-name : Id Σ → (Values Nam Σ)
(define (alloc-name id Σ₀)
  (match-let ([(Stx (Sym nam) _) id]
              [(Σ size tbl) Σ₀])
    (values (string->symbol (format "~a:~a" nam size))
            (Σ (add1 size) tbl))))

;; alloc-scope : Symbol Σ → (Values Scp Σ)
(define (alloc-scope s Σ₀)
  (match-let ([(Σ size tbl) Σ₀])
    (values (string->symbol (format "~a:~a" s size))
            (Σ (add1 size) tbl))))

;; alloc-𝓁 : Stx Σ → (Values 𝓁 Σ)
;;   - called from push-κ
;;   - called from alloc-def-ξ and alloc-box (full)
;;   - stx is used in abs for ensuring finiteness of the domain
(define (alloc-𝓁 stx Σ₀)
  (match-let ([(Σ size tbl) Σ₀])
    (values
     (𝓁 (cons stx size)) ; (𝓁 (string->symbol (format "𝓁:~a:~a" stx size)))
     (Σ (add1 size) tbl))))
