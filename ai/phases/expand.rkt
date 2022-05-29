#lang racket
(require "../../interp/set.rkt"
         (only-in "../../interp/reduction.rkt"
                  reducer-of
                  define-parameterized-extended-reduction-relation
                  apply-reduction-relation*)
         "../../interp/phases/struct.rkt"
         (only-in "../../interp/core/syntax.rkt" zip unzip snoc union)
         (only-in "../../interp/phases/syntax.rkt"
                  empty-ctx in-hole add flip prune at-phase)
         (only-in "../../interp/core/eval.rkt" init-env init-store)
         (only-in "../../interp/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ)
         (only-in "../../interp/phases/expand.rkt"
                  regist-vars/bind/alloc-name
                  id-seq id-kont id-snoc stx-nil
                  [==>p/Σ interp:==>p/Σ])

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
(define-parameterized-extended-reduction-relation
  (==>p/Σ bind resolve id=? alloc-name alloc-scope regist-vars parse -->c)
  (interp:==>p/Σ bind resolve id=? alloc-name alloc-scope regist-vars parse -->c)

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body2 ξ scps_p2)) ctx) '∘ κ Θ Σ)
   #:when (and (id=? ph id_kont '#%kont     Σ)
               (id=? ph id_ls   'let-syntax Σ))
   #:with nam_new <- (resolve ph id_new Σ)
   #:with ast_exp <- (parse (add1 ph) stx_exp Σ)
   (InEval `(,(AstEnv ast_exp (init-env)) • ,(init-store))
           (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                         ,(Stxξ ph stx_body2 ξ scps_p2)) (empty-ctx))
               '∘ κ Θ Σ))
   ex-ls-eval]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ scps_p)
       '∘ κ Θ Σ)
   #:with nam_mac <- (resolve ph id_mac Σ)
   #:with val := (lookup-ξ ξ nam_mac)
   #:when (Val? val)
   (let*-values ([(scp_u Σ_1) (alloc-scope 'u Σ)]
                 [(scp_i Σ_2) (alloc-scope 'i Σ_1)])
     (InEval
      `(,(AstEnv (App val
                      (list (flip ph (add ph stx_macapp scp_u) scp_i)))
                 (init-env))
        • ,(init-store))
      (ζ (Stxξ ph (GenStx #f (list (cons ph (set scp_i))))
                 ξ (union (set scp_u) scps_p)) '∘ κ Θ Σ_2)))
   ex-macapp-eval]

  ;; application
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (Id? stx_fun)
   #:with name <- (resolve ph stx_fun Σ)
   #:when (let ([at (lookup-ξ ξ name)])
            (or (TVar? at)
                (and (eq? 'not-found at)
                     (not (member name
                                  '(lambda let quote syntax let-syntax if
                                     #%app #%kont #%seq #%ls-kont
                                     #%snoc))))))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-app]

  ;; primitive application (NEW)
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (not (Id? stx_fun))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-prim-app]

  ;; reference
  [(ζ (Stxξ ph (and id (GenStx (Sym nam) ctx)) ξ scps_p) '∘ κ Θ Σ)
   #:with nam <- (resolve ph id Σ)
   (let ([all-transform (lookup-ξ ξ nam)])
     (match all-transform
       [(TVar id_new) (ζ id_new '• κ Θ Σ)]
       [_ (error '==>p "unbound identifier: ~a" nam)]))
   ex-var])

(define ==>p ((reducer-of ==>p/Σ)
              bind resolve id=? alloc-name alloc-scope regist-vars parse -->c))

;(: expand : Ph Stx ξ Scps Σ -> (Setof (Cons Stx Σ)))
(define ((expand/==> ==>) ph stx ξ scps_p Σ)
  (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                 (apply-reduction-relation* ==> init-ζ)])
      (list->set (map cons stx_new Σ_new)))))

(define expand (expand/==> ==>p))
