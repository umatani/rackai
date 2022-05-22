#lang racket
(require (only-in "../../interp/reduction.rkt" reducer-of)
         "../../interp/phases/struct.rkt"
         (only-in "../../interp/phases/syntax.rkt" add)
         (only-in "../../interp/core/expand.rkt" extend-ξ)
         (only-in "../../interp/phases/expand.rkt" ==>p/Σ expand/==>)

         ;; Abstract version
         (only-in "../core/eval.rkt" -->c)
         (only-in "../core/expand.rkt" alloc-name alloc-scope)
         (only-in "syntax.rkt" bind resolve)
         (only-in "parse.rkt" parse))
(provide (all-defined-out))

;; This is the same as the single-phase one, but with `ph`
;; threaded through to `add` & `bind`
;(: regist-vars : Ph Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define (regist-vars ph scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) stx) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars ph scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (add ph id scp)]
                   [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))


(define ==>p ((reducer-of ==>p/Σ)
              bind resolve alloc-name alloc-scope regist-vars parse -->c))

(define expand (expand/==> ==>p))
