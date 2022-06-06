#lang racket
(require "../../base/set.rkt"
         "../../base/reduction.rkt"
         "../../base/full/struct.rkt"
         "../../base/core/delta.rkt"
         (only-in "../../base/core/syntax.rkt" zip unzip snoc union)
         (only-in "../../base/phases/syntax.rkt"
                  empty-ctx add flip prune at-phase)
         (only-in "../../base/full/syntax.rkt" in-hole)
         (only-in "../../base/core/eval.rkt"
                  init-env update-env lookup-env init-store)
         (only-in "../../base/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ)
         (only-in "../../base/phases/expand.rkt"
                  id-seq id-kont id-snoc stx-nil)
         (only-in "../../base/full/expand-eval.rkt"
                  extend-ξ* unstop
                  [-->f/store base:-->f/store]
                  [==>f/Σ base:==>f/Σ]
                  eval/--> expand/==>)

         ;; Set-based version
         (only-in "../core/eval.rkt"
                  lookup-store update-store* alloc-loc* push-cont)
         (only-in "../core/expand.rkt" alloc-name alloc-scope)
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/expand.rkt" regist-vars)
         (only-in "../phases/syntax.rkt" bind resolve)
         (only-in "syntax.rkt" resolve*/resolve id=?))
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


(define-parameterized-extended-reduction-relation (-->f/store delta ==>f)
  (base:-->f/store delta ==>f <-))

(define-parameterized-extended-reduction-relation (==>f/Σ -->f)
  (base:==>f/Σ -->f <-))


(define-values (-->f ==>f)
  (letrec ([-->f (λ () ((reducer-of -->f/store) delta ==>f))]
           [==>f (λ () ((reducer-of ==>f/Σ) -->f))])
    (values (-->f) (==>f))))

;(: eval : Ph Ast MaybeScp ξ Σ* -> (Setof (Cons Val Σ*)))
(define ((eval/--> -->) ph ast maybe-scp_i ξ Σ*)
  (match-let ([(set `(,(? Val? val) • ,_store ,Σ*_2) ...)
               (apply-reduction-relation*
                --> `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                      • ,(init-store) ,Σ*))])
    (list->set (map cons val Σ*_2))))

;(: expand : Ph Stx ξ Σ* -> (Setof (Cons Stx Σ*)))
(define ((expand/==> ==>) ph stx ξ Σ*)
  (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• (init-Θ) Σ*)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ*_new) ...)
                 (apply-reduction-relation* ==> init-ζ)])
      (list->set (map cons stx_new Σ*_new)))))

(define eval (eval/--> -->f))
(define expand (expand/==> ==>f))
