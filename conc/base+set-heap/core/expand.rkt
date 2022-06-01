#lang racket
(require "../../base/set.rkt"
         "../../base/reduction.rkt"
         "../../base/core/struct.rkt"
         (only-in "../../base/core/syntax.rkt"
                  snoc zip unzip add flip empty-ctx in-hole)
         (only-in "../../base/core/eval.rkt" init-env init-store)
         (only-in "../../base/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ
                  regist-vars/bind/alloc-name
                  id-kont id-seq id-snoc stx-nil
                  [==>c/Σ base:==>c/Σ])

         ;; Abstract version
         (only-in "syntax.rkt" bind resolve id=?)
         (only-in "eval.rkt" -->c)
         (only-in "parse.rkt" parse))
(provide (all-defined-out))

;; Finite-domain allocation

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
(define regist-vars (regist-vars/bind/alloc-name bind alloc-name))

;; Revised reduction rules

;; (: ==>c : ζ -> (Setof ζ))
(define-extended-reduction-relation ==>c/Σ (base:==>c/Σ <-))

(define ==>c ((reducer-of ==>c/Σ)))

;(: expand : Stx ξ Σ -> (Setof (Cons Stx Σ))
(define ((expand/==> ==>) stx ξ Σ)
  (let ([init-ζ (ζ (Stxξ stx ξ) '∘ '• (init-Θ) Σ)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                 (apply-reduction-relation* ==> init-ζ)])
      (list->set (map cons stx_new Σ_new)))))

(define expand (expand/==> ==>c))
