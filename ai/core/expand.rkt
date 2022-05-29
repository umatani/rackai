#lang racket
(require "../../interp/set.rkt"
         (only-in "../../interp/reduction.rkt"
                  reducer-of
                  define-parameterized-extended-reduction-relation
                  apply-reduction-relation*)
         "../../interp/core/struct.rkt"
         (only-in "../../interp/core/syntax.rkt"
                  snoc zip unzip add flip empty-ctx in-hole)
         (only-in "../../interp/core/eval.rkt" init-env init-store)
         (only-in "../../interp/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ
                  regist-vars/bind/alloc-name
                  id-kont id-seq id-snoc stx-nil
                  [==>c/Σ interp:==>c/Σ])

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
(define-parameterized-extended-reduction-relation 
  (==>c/Σ bind resolve id=? alloc-name alloc-scope regist-vars parse -->c)
  (interp:==>c/Σ bind resolve id=? alloc-name alloc-scope regist-vars parse -->c)

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ stx_body2 ξ)) ctx) '∘ κ Θ Σ)
   #:when (and (id=? id_kont '#%kont     Σ)
               (id=? id_ls   'let-syntax Σ))
   #:with nam_new <- (resolve id_new Σ)
   #:with ast_exp <- (parse stx_exp Σ)
   (InEval `(,(AstEnv ast_exp (init-env)) • ,(init-store))
           (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                         ,(Stxξ stx_body2 ξ)) (empty-ctx))
               '∘ κ Θ Σ))
   ex-ls-eval]

  ;; macro invocation
  [(ζ (Stxξ (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ)
       '∘ κ Θ Σ)
   #:with nam_mac <- (resolve id_mac Σ)
   #:when (Val? (lookup-ξ ξ nam_mac))
   (let*-values ([(val) (lookup-ξ ξ nam_mac)]
                 [(scp_u Σ_1) (alloc-scope 'u Σ)]
                 [(scp_i Σ_2) (alloc-scope 'i Σ_1)])
     (InEval
      `(,(AstEnv (App val
                      (list (flip (add stx_macapp scp_u) scp_i))) (init-env))
        • ,(init-store))
      (ζ (Stxξ (GenStx #f (set scp_i)) ξ) '∘ κ Θ Σ_2)))
   ex-macapp-eval]

  ;; application
  [(ζ (Stxξ (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (Id? stx_fun)
   #:with name <- (resolve stx_fun Σ)
   #:when (let ([at (lookup-ξ ξ name)])
            (or (TVar? at)
                (and (eq? 'not-found at)
                     (not (member name
                                  '(lambda let quote syntax let-syntax if
                                     #%app #%kont #%seq #%ls-kont
                                     #%snoc))))))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-app]

  ;; primitive application (NEW)
  [(ζ (Stxξ (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (not (Id? stx_fun))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-prim-app]

  ;; reference
  [(ζ (Stxξ (and id (GenStx (Sym nam) ctx)) ξ) '∘ κ Θ Σ)
   #:with nam <- (resolve id Σ)
   (let ([all-transform (lookup-ξ ξ nam)])
     (match all-transform
       [(TVar id_new) (ζ id_new '• κ Θ Σ)]
       [_ (error '==>c "unbound identifier: ~a" nam)]))
   ex-var])


(define ==>c ((reducer-of ==>c/Σ)
              ;; non-deterministic: resolve, parse
              bind resolve id=? alloc-name alloc-scope regist-vars parse -->c))

;(: expand : Stx ξ Σ -> (Setof (Cons Stx Σ))
(define ((expand/==> ==>) stx ξ Σ)
  (let ([init-ζ (ζ (Stxξ stx ξ) '∘ '• (init-Θ) Σ)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                 (apply-reduction-relation* ==> init-ζ)])
      (list->set (map cons stx_new Σ_new)))))

(define expand (expand/==> ==>c))
