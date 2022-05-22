#lang racket
(require "../../interp/core/struct.rkt"
         (only-in "../../interp/reduction.rkt" reducer-of)
         (only-in "../../interp/core/syntax.rkt" add)
         (only-in "../../interp/core/expand.rkt"
                  extend-ξ ==>c/Σ expand/==>)

         ;; Abstract version
         (only-in "syntax.rkt" bind resolve)
         (only-in "eval.rkt" -->c)
         (only-in "parse.rkt" parse))
(provide (all-defined-out))

; (: alloc-name : Id Σ -> (Values Nam Σ))
(define (alloc-name id Σ0)
  (match-let ([(GenStx (Sym nam) _) id]
              [(Σ size tbl) Σ0])
    (values (string->symbol (format "~a:~a" nam size))
            (Σ (add1 size) tbl))))

; (: alloc-scope : Symbol Σ -> (Values Scp Σ))
(define (alloc-scope s Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (string->symbol (format "~a::~a" s size))
            (Σ (add1 size) tbl))))

;(: regist-vars : Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define (regist-vars scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) stx) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (add id scp)]
                   [(Σ_3) (bind Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))


(define ==>c ((reducer-of ==>c/Σ)
              bind resolve alloc-name alloc-scope regist-vars parse -->c))

(define expand (expand/==> ==>c))


