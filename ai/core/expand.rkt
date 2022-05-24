#lang racket
(require (only-in "../../interp/reduction.rkt"
                  reducer-of
                  define-parameterized-extended-reduction-relation)
         "../../interp/core/struct.rkt"
         (only-in "../../interp/core/syntax.rkt"
                  snoc zip unzip add flip empty-ctx in-hole)
         (only-in "../../interp/core/eval.rkt" init-env init-store)
         (only-in "../../interp/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ
                  id-kont id-seq id-snoc stx-nil
                  [==>c/Σ interp:==>c/Σ] expand/==>)

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

;; Revised reduction rules

;; (: ==>c : ζ -> (Setof ζ))
(define-parameterized-extended-reduction-relation ==>c/Σ interp:==>c/Σ
  (bind resolve id=? alloc-name alloc-scope regist-vars parse -->c)

  ;; reference
  ['HOGEEEEE
   #f
   ex-var]
  #;
  [(ζ (Stxξ (and id (GenStx (Sym nam) ctx)) ξ) '∘ κ Θ Σ)
   (let ([all-transform (lookup-ξ ξ (resolve id Σ))])
     (match all-transform
       [(TVar id_new) (ζ id_new '• κ Θ Σ)]
       [_ (error '==>c "unbound identifier: ~a" nam)]))
   ex-var]

  )


(define ==>c ((reducer-of ==>c/Σ)
              ;; non-deterministic: resolve, parse
              bind resolve id=? alloc-name alloc-scope regist-vars parse -->c))

(define expand (expand/==> ==>c))


