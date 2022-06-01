#lang racket
(require "../../base/set.rkt"
         "../../base/reduction.rkt"
         "../../base/phases/struct.rkt"
         (only-in "../../base/core/syntax.rkt" zip unzip snoc union)
         (only-in "../../base/phases/syntax.rkt"
                  empty-ctx in-hole add flip prune at-phase)
         (only-in "../../base/core/eval.rkt" init-env init-store)
         (only-in "../../base/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ)
         (only-in "../../base/phases/expand.rkt"
                  regist-vars/bind/alloc-name
                  id-seq id-kont id-snoc stx-nil
                  [==>p/Σ base:==>p/Σ])

         ;; Abstract version
         (only-in "../core/eval.rkt" -->c)
         (only-in "../core/expand.rkt" alloc-name alloc-scope)
         (only-in "syntax.rkt" bind resolve id=?)
         (only-in "parse.rkt" parse))
(provide (all-defined-out))

;; This is the same as the single-phase one, but with `ph`
;; threaded through to `add` & `bind`
;(: regist-vars : Ph Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define regist-vars (regist-vars/bind/alloc-name bind alloc-name))

;; (: ==>p : ζ -> (Setof ζ))
(define-extended-reduction-relation ==>p/Σ (base:==>p/Σ <-))

(define ==>p ((reducer-of ==>p/Σ)))

;(: expand : Ph Stx ξ Scps Σ -> (Setof (Cons Stx Σ)))
(define ((expand/==> ==>) ph stx ξ scps_p Σ)
  (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                 (apply-reduction-relation* ==> init-ζ)])
      (list->set (map cons stx_new Σ_new)))))

(define expand (expand/==> ==>p))
