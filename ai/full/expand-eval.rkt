#lang racket
(require (only-in "../../interp/reduction.rkt" reducer-of)
         "../../interp/full/struct.rkt"
         (only-in "../../interp/full/expand-eval.rkt"
                  -->f/store eval/--> ==>f/Σ expand/==>)

         ;; Abstract version
         (only-in "../core/eval.rkt"
                  lookup-store update-store* alloc-loc* push-cont)
         (only-in "../core/expand.rkt" alloc-name alloc-scope)
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/expand.rkt" regist-vars)
         (only-in "../phases/syntax.rkt" bind resolve))
(provide (all-defined-out))


;; ----------------------------------------
;; Box allocations and updates:

;(: alloc-box : Σ -> (Values 𝓁 Σ))
(define (alloc-box Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (𝓁 (string->symbol (format "b:~a" size)))
            (Σ (add1 size) tbl))))

;(: box-lookup : Σ 𝓁 -> Val)
(define (box-lookup Σ 𝓁)
  (hash-ref (Σ-tbl Σ) 𝓁))

;(: box-update : Σ 𝓁 Val -> Σ)
(define (box-update Σ0 𝓁0 val)
  (match-let ([(Σ size binds) Σ0])
    (Σ size (hash-set binds 𝓁0 val))))

;; ----------------------------------------
;; Definition-context environment allocations and updates:

;(: alloc-def-ξ : Σ -> (Values 𝓁 Σ))
(define (alloc-def-ξ Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (𝓁 (string->symbol (format "ξ:~a" size)))
            (Σ (add1 size) tbl))))

;(: def-ξ-lookup : Σ 𝓁 -> ξ)
(define (def-ξ-lookup Σ0 𝓁)
  (hash-ref (Σ-tbl Σ0) 𝓁))

;(: def-ξ-update : Σ 𝓁 ξ -> Σ)
(define (def-ξ-update Σ0 𝓁 ξ)
  (match-let ([(Σ size tbl) Σ0])
    (Σ size (hash-set tbl 𝓁 ξ))))


(define-values (-->f ==>f)
  (letrec ([-->f (λ () ((reducer-of -->f/store)
                         lookup-store update-store* alloc-loc* push-cont
                         alloc-box box-lookup box-update
                         alloc-def-ξ def-ξ-lookup def-ξ-update
                         bind resolve alloc-name alloc-scope
                         parse ==>f))]
           [==>f (λ () ((reducer-of ==>f/Σ)
                         bind resolve alloc-name alloc-scope regist-vars
                         parse -->f))])
    (values (-->f) (==>f))))

(define eval (eval/--> -->f))
(define expand (expand/==> ==>f))
