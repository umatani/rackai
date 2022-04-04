#lang racket
(require redex/reduction-semantics
         "common.rkt"
         (rename-in (except-in "core-model.rkt"
                               flip add
                               bind
                               parse
                               resolve)
                    [stl->seq core:stl->seq]
                    [unzip core:unzip]
                    [zip   core:zip]
                    [subst core:subst]
                    [substs core:substs]
                    [δ core:δ]
                    [strip core:strip]
                    [lookup-env core:lookup-env]
                    [extend-env core:extend-env]
                    [alloc-name core:alloc-name]
                    [alloc-scope core:alloc-scope]
                    [run core:run])
         (for-syntax racket/list))

(provide Lph stl->seq zip unzip
         subst substs
         δ
         eval
         flip add strip prune update-ctx
         bind at-phase
         resolve
         parse
         lookup-env extend-env
         alloc-scope alloc-name regist-vars
         expand expand*
         run phases:examples
         (all-from-out "core-model.rkt"))

(define-extended-language Lph L
  [ph integer]
  [ctx desc-ctx]
  [desc-ctx (Map [ph scps] ...)])

;; The redefinition of `ctx` changes the definition of `val`, so most
;; metafunctions need to be reinterpreted with respect to `Lph`.

(define-metafunction/extension core:stl->seq Lph
  stl->seq : stl -> (stx ...))

(define-metafunction/extension core:unzip Lph
  unzip : stl -> (values stl stl))

(define-metafunction/extension core:zip Lph
  zip : stl stl ctx -> stl)

;; ----------------------------------------
;; Non-capturing substitution for AST:

;; (define-metafunction/extension core:subst Lph
;;   subst : ast var ast -> ast)

;; (define-metafunction/extension core:substs Lph
;;   substs : ast (var ...) (ast ...) -> ast)

(define-metafunction Lph
  subst : ast var ast -> ast
  [(subst var var ast_v) ast_v]
  [(subst var_2 var ast_v) var_2]
  [(subst (App ast ...) var ast_v)
   (App (subst ast var ast_v) ...)]
  [(subst (If ast_1 ast_2 ast_3) var ast_v)
   (If (subst ast_1 var ast_v) (subst ast_2 var ast_v) (subst ast_3 var ast_v))]
  [(subst (Fun (var_0 ... var var_1 ...) ast) var ast_v)
   (Fun (var_0 ... var var_1 ...) ast)]
  [(subst (Fun (var_2 ...) ast) var ast_v)
   (Fun (var_3 ...) (subst (substs ast (var_2 ...) (var_3 ...))
                           var ast_v))
   (where ((Var nam_2) ...) (var_2 ...))
   (where (nam_3 ...) ,(variables-not-in (term ast_v) (term (nam_2 ...))))
   (where (var_3 ...) ((Var nam_3) ...))]
  [(subst atom var ast_v) atom]
  [(subst (Cons val_1 val_2) var ast_v) 
   (Cons (subst val_1 var ast_v) val_2)]
  [(subst stx var ast_v) stx])

(define-metafunction Lph
  substs : ast (var ...) (ast ...) -> ast
  [(substs ast () ()) ast]
  [(substs ast (var_0 var_1 ...) (ast_0 ast_1 ...))
   (subst (substs ast (var_1 ...) (ast_1 ...)) var_0 ast_0)])


;; ----------------------------------------
;; Implementation of primitives:

(define-metafunction/extension core:δ Lph
  δ : prim (val ...) -> val)

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction Lph
  eval : ast -> val
  [(eval (App ast_fun ast_arg ...))
   (eval (substs ast_body (var ...) (eval* () (ast_arg ...))))
   (where (Fun (var ...) ast_body) (eval ast_fun))]
  [(eval (App prim ast_arg ...))
   (δ prim ((eval ast_arg) ...))]
  [(eval (If ast_test ast_then ast_else))
   (eval ast_else)
   (where #f (eval ast_test))]
  [(eval (If ast_test ast_then ast_else))
   (eval ast_then)]
  [(eval val) val])

(define-metafunction Lph
  eval* : (val ...) (ast ...) -> (val ...)
  [(eval* (val ...) ()) (val ...)]
  [(eval* (val ...) (ast_0 ast_1 ...))
   (eval* (val ... (eval ast_0)) (ast_1 ...))])


;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction Lph
  add : ph stl scp -> stl
  ;; Similar to one-phase `add`, but must update context
  ;; at a given phase
  [(add ph (Stx atom ctx) scp) 
   (Stx atom (update-ctx ctx ph (union (Set scp) (at-phase ctx ph))))]
  [(add ph (Stx (Cons stx stl) ctx) scp)
   (Stx (Cons (add ph stx scp) (add ph stl scp))
        (update-ctx ctx ph (union (Set scp) (at-phase ctx ph))))]
  [(add ph () scp) ()]
  [(add ph (Cons stx stl) scp) (Cons (add ph stx scp) (add ph stl scp))])

(define-metafunction Lph
  flip : ph stl scp -> stl
  ;; Similar to one-phase `flip`, but must update context
  ;; at a given phase
  [(flip ph (Stx atom ctx) scp) 
   (Stx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
  [(flip ph (Stx (Cons stx stl) ctx) scp)
   (Stx (Cons (flip ph stx scp) (flip ph stl scp))
        (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
  [(flip ph () scp) ()]
  [(flip ph (Cons stx stl) scp) (Cons (flip ph stx scp) (flip ph stl scp))])

(define-metafunction/extension core:strip Lph
  strip : stl -> val)

(define-metafunction Lph
  prune : ph stl scps -> stl
  ;; Recursively removes a set of scopes from a syntax object
  ;; at a given phase
  [(prune ph (Stx atom ctx) scps_p)
   (Stx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
  [(prune ph (Stx (Cons stx stl) ctx) scps_p)
   (Stx (Cons stx_pruned stl_pruned)
        (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))
   (where stx_pruned (prune ph stx scps_p))
   (where stl_pruned (prune ph stl scps_p))]
  [(prune ph () scps) ()]
  [(prune ph (Cons stx stl) scps_p) (Cons (prune ph stx scps_p)
                                          (prune ph stl scps_p))])

(define-metafunction Lph
  update-ctx : ctx ph scps -> ctx
  ;; Updates the mapping of a `ctx` at a particular phase
  [(update-ctx (Map any_1 ... [ph scps_2] any_2 ...) ph scps_1)
   (Map any_1 ... [ph scps_1] any_2 ...)]
  [(update-ctx (Map any_1 ...) ph scps_1)
   (Map any_1 ... [ph scps_1])])

(define-metafunction Lph
  bind : ph Σ id nam -> Σ
  ;; Like one-phase `bind`, but extracts scopes at a given phase of
  ;; the identifier
  [(bind ph
         (Sto number
              (binds_1 ... [nam_1 (StoBind scps_2 nam_2) ...] binds_2 ...)
              boxes
              def-envs)
         (Stx (Sym nam_1) (Map _ ... [ph scps_1] _ ...))
         nam_3)
   (Sto number
        (binds_1 ... [nam_1 (StoBind scps_1 nam_3) (StoBind scps_2 nam_2) ...] binds_2 ...)
        boxes
        def-envs)]
  [(bind ph
         (Sto number (binds ...) boxes def-envs)
         (Stx (Sym nam_1) (Map _ ... [ph scps_1] _ ...))
         nam_3)
   (Sto number
        ([nam_1 (StoBind scps_1 nam_3)] binds ...)
        boxes
        def-envs)])

(define-metafunction Lph
  at-phase : ctx ph -> scps
  [(at-phase (Map any_1 ... [ph scps] any_2 ...) ph)
   scps]
  [(at-phase ctx ph)
   (Set)])

(define-metafunction Lph
  resolve : ph id Σ -> nam
  ;; Like the one-phase `resolve`, but at a particular phase
  [(resolve ph (Stx (Sym nam) ctx) Σ)
   nam_biggest
   (where (Set (StoBind scps_bind nam_bind) ...) (lookup-Σ Σ nam))
   (where scps_biggest (biggest-subset (at-phase ctx ph) (Set scps_bind ...)))
   (where nam_biggest (binding-lookup (Set (StoBind scps_bind nam_bind) ...) scps_biggest))]
  [(resolve ph (Stx (Sym nam) ctx) Σ)
   nam])

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define-metafunction Lph
  parse : ph stx Σ -> ast
  ;; This parse is the same as the single-phase one, but with `ph`
  ;; threaded through to `resolve`
  [; (lambda (id ...) stx_body)
   (parse ph (Stx (Cons id_lam (Cons (Stx stl_ids _)
                                     (Cons stx_body ()))) ctx) Σ)
   (Fun ((Var (resolve ph id Σ)) ...) (parse ph stx_body Σ))
   (where lambda (resolve ph id_lam Σ))
   (where (id ...) (stl->seq stl_ids))]
  [; (let ([id stx_rhs] ...) stx_body)
   (parse ph (Stx
              (Cons id_let
                    (Cons (Stx stl_binds ctx_1)
                          (Cons stx_body ()))) ctx_2) Σ)
   (App (Fun ((Var (resolve ph id Σ)) ...) (parse ph stx_body Σ))
        (parse ph stx_rhs Σ) ...)
   (where let (resolve ph id_let Σ))
   (where (values stl_ids stl_rhs) (unzip stl_binds))
   (where (id ...) (stl->seq stl_ids))
   (where (stx_rhs ...) (stl->seq stl_rhs))]
  [; (quote stx)
   (parse ph (Stx (Cons id_quote (Cons stx ())) ctx) Σ)
   (strip stx)
   (where quote (resolve ph id_quote Σ))]
  [; (syntax stx)
   (parse ph (Stx (Cons id_syntax (Cons stx ())) ctx) Σ)
   stx
   (where syntax (resolve ph id_syntax Σ))]
  [; (#%app stx_fun stx_arg ...) トップレベルがstx-pair (cdr部もstx)であることに注意
   (parse ph (Stx (Cons id_app (Stx (Cons stx_rator stl_rands) ctx_1)) ctx_2) Σ)
   (App (parse ph stx_rator Σ) ast_rand ...)
   (where #%app (resolve ph id_app Σ))
   (where (ast_rand ...) (parse* ph stl_rands Σ))]
  [; (if stx stx stx)
   (parse ph (Stx (Cons id_if (Cons stx_test (Cons stx_then (Cons stx_else ())))) ctx) Σ)
   (If (parse ph stx_test Σ) (parse ph stx_then Σ) (parse ph stx_else Σ))
   (where if (resolve ph id_if Σ))]
  [(parse ph id Σ)
   (Var (resolve ph id Σ))]
  [(parse ph (Stx atom ctx) Σ)
   atom])

(define-metafunction Lph
  parse* : ph stl Σ -> (ast ...)
  [(parse* ph stx Σ) (parse ph stx Σ)]
  [(parse* ph () Σ) ()]
  [(parse* ph (Cons stx stl) Σ)
   ((parse ph stx Σ) ast ...)
   (where (ast ...) (parse* ph stl Σ))])

;; ----------------------------------------
;; Expand-time environment operations:

(define-metafunction/extension core:lookup-env Lph
  lookup-env : env nam -> all-transform)

(define-metafunction/extension core:extend-env Lph
  extend-env : env nam all-transform -> env)

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

(define-metafunction/extension core:alloc-name Lph
  alloc-name : id Σ -> (values nam Σ))

(define-metafunction/extension core:alloc-scope Lph
  alloc-scope : Σ -> (values scp Σ))

(define-metafunction Lph
  regist-vars : ph scp stl env Σ -> (values stl env Σ)
  [(regist-vars ph scp () env Σ) (values () env Σ)]
  [(regist-vars ph scp (Cons id stl) env Σ)
   (values (Cons id_new stl_reg) env_2 Σ_3)
   (where (values stl_reg env_1 Σ_1) (regist-vars ph scp stl env Σ))
   (where (values nam_new Σ_2) (alloc-name id Σ_1))
   (where id_new (add ph id scp))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where env_2 (extend-env env_1 nam_new (TVar id_new)))])

;; ----------------------------------------
;; The expander:

(define-metafunction Lph
  expand : ph stx env scps Σ -> (values stx Σ)

  ;; lambda
  [;  (Stx (List id_lam (Stx (List id_arg) ctx_0) stx_body) ctx)
   (expand ph (Stx (Cons id_lam (Cons (Stx stl_args ctx_0)
                                      (Cons stx_body ()))) ctx) env scps_p Σ)
   (values    (Stx (Cons id_lam (Cons (Stx stl_args2 ctx_0)
                                      (Cons stx_body2 ()))) ctx) Σ_3)
   (where lambda (resolve ph id_lam Σ))
   (where (values scp_new Σ_1) (alloc-scope Σ))
   (where (values stl_args2 env_new Σ_2)
          (regist-vars ph scp_new stl_args env Σ_1))
   (where (values stx_body2 Σ_3)
          (expand ph (add ph stx_body scp_new) env_new (union (Set scp_new) scps_p) Σ_2))]

  ;; let
  [(expand ph (Stx (Cons id_let
                         (Cons (Stx stl_binds ctx_1)
                               (Cons stx_body ()))) ctx_2) env scps_p Σ)
   (values    (Stx (Cons id_let
                         (Cons (Stx (zip stl_vars2 stl_rhs2 ctx_1) ctx_1)
                               (Cons stx_body2 ()))) ctx_2) Σ_4)
   (where let (resolve ph id_let Σ))
   (where (values stl_vars stl_rhs) (unzip stl_binds))
   (where (values scp_new Σ_1) (alloc-scope Σ))
   (where (values stl_vars2 env_new Σ_2)
          (regist-vars ph scp_new stl_vars env Σ_1))
   (where (values stx_body2 Σ_3)
          (expand ph (add ph stx_body scp_new) env_new (union (Set scp_new) scps_p) Σ_2))
   (where (values stl_rhs2 Σ_4) (expand* ph stl_rhs env scps_p Σ_3))]

  ;; quote
  [;  (Stx (List id_quote stx) ctx)
   (expand ph (Stx (Cons id_quote (Cons stx ())) ctx) env scps_p Σ)
   (values    (Stx (Cons id_quote (Cons stx ())) ctx) Σ)
   (where quote (resolve ph id_quote Σ))]
  
  ;; syntax
  [;  (Stx (List id_syntax stx) ctx)
   (expand ph (Stx (Cons id_syntax (Cons stx ())) ctx) env scps_p Σ)
   (values    (Stx (Cons id_syntax (Cons stx_pruned ())) ctx) Σ)
   (where syntax (resolve ph id_syntax Σ))
   (where stx_pruned (prune ph stx scps_p))]
  
  ;; macro creation
  [#; (Stx (List id_ls
                (Stx (List (Stx (List id stx_rhs) _)) _)
                stx_body) ctx)
   (expand ph (Stx (Cons id_ls
                         (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ())) _) ()) _)
                               (Cons stx_body ()))) ctx) env scps_p Σ)
   (expand ph stx_body2 env_2 scps_p2 Σ_4)
   (where let-syntax (resolve ph id_ls Σ))
   (where (values nam_new Σ_1) (alloc-name id Σ))
   (where (values scp_new Σ_2) (alloc-scope Σ_1))
   (where id_new (add ph id scp_new))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where (values stx_exp Σ_4)
          (expand (plus ph 1) stx_rhs (primitives-env) (Set) Σ_3))
   (where env_2 (extend-env env nam_new (eval (parse (plus ph 1) stx_exp Σ_4))))
   (where stx_body2 (add ph stx_body scp_new))
   (where scps_p2 (union (Set scp_new) scps_p))]

  ;; macro invocation
  [(expand ph stx_macapp env scps_p Σ)
   (expand ph (flip ph stx_exp scp_i) env (union (Set scp_u) scps_p) Σ_2)
   (where (Stx (Cons id_mac stl_args) ctx) stx_macapp)
   (where val (lookup-env env (resolve ph id_mac Σ)))
   (where (values scp_u Σ_1) (alloc-scope Σ))
   (where (values scp_i Σ_2) (alloc-scope Σ_1))
   (where stx_exp (eval (App val (flip ph (add ph stx_macapp scp_u) scp_i))))]
  
  ;; if
  [(expand ph (Stx (Cons id_if (Cons stx_test (Cons stx_then (Cons stx_else ())))) ctx) env scps_p Σ)
   (values    (Stx (Cons id_if (Cons stx_test2 (Cons stx_then2 (Cons stx_else2 ())))) ctx) Σ_3)
   (where if (resolve ph id_if Σ))
   (where (values stx_test2 Σ_1) (expand ph stx_test env scps_p Σ))
   (where (values stx_then2 Σ_2) (expand ph stx_then env scps_p Σ_1))
   (where (values stx_else2 Σ_3) (expand ph stx_else env scps_p Σ_2))]

  ;; application (non-canonical #%app version)
  [;  (Stx (List id_app stx_rtor stx_rnd ...) ctx)
   (expand ph (Stx (Cons id_app      (Cons stx_rtor    stl_rnds)) ctx) env scps_p Σ)
   (values    (Stx (Cons id_app (Stx (Cons stx_exprtor stl_exprnds) ctx)) ctx) Σ_2)
   (where #%app (resolve ph id_app Σ))
   (where (values stx_exprtor Σ_1) (expand ph stx_rtor env scps_p Σ))
   (where (values stl_exprnds Σ_2) (expand* ph stl_rnds env scps_p Σ_1))]

  ;; application (canonical #%app version)
  [(expand ph (Stx (Cons id_app (Stx (Cons stx_rtor    stl_rnds)    ctx_1)) ctx) env scps_p Σ)
   (values    (Stx (Cons id_app (Stx (Cons stx_exprtor stl_exprnds) ctx_1)) ctx) Σ_2)
   (where #%app (resolve ph id_app Σ))
   (where (values stx_exprtor Σ_1) (expand ph stx_rtor env scps_p Σ))
   (where (values stl_exprnds Σ_2) (expand* ph stl_rnds env scps_p Σ_1))]

  ;; application
  [;  (Stx (List stx_rtor stx_rnd ...) ctx)
   (expand ph                (Stx (Cons stx_rtor    stl_rnds) ctx) env scps_p Σ)
   (values (Stx (Cons id_app (Stx (Cons stx_exprtor stl_exprnds) ctx)) ctx) Σ_2)
   (where id_app (Stx (Sym #%app) ctx))
   (where (values stx_exprtor Σ_1) (expand  ph stx_rtor env scps_p Σ))
   (where (values stl_exprnds Σ_2) (expand* ph stl_rnds env scps_p Σ_1))]

  ;; reference
  [(expand ph id env scps_p Σ)
   (values id_new Σ)
   (where (TVar id_new) (lookup-env env (resolve ph id Σ)))]

  ;; literal
  [(expand ph (Stx atom ctx) env scps_p Σ)
   (values (Stx (Cons (Stx (Sym quote) ctx) (Cons (Stx atom ctx) ())) ctx) Σ)]
  )

(define-metafunction Lph
  expand* : ph stl env scps Σ -> (values stl Σ)
  [(expand* ph stx            env scps_p Σ) (expand ph stx env scps_p Σ)]
  [(expand* ph ()             env scps_p Σ) (values () Σ)]
  [(expand* ph (Cons stx stl) env scps_p Σ)
   (values (Cons stx_exp stl_exp) Σ_2)
   (where (values stx_exp Σ_1) (expand  ph stx env scps_p Σ))
   (where (values stl_exp Σ_2) (expand* ph stl env scps_p Σ_1))])

;; ----------------------------------------
;; Drivers

(define-helpers Lph (Map)
  reader printer)

(define-metafunction Lph
  stripper : (values stx Σ) -> val
  [(stripper (values stx Σ)) (strip stx)])

(define-metafunction Lph
  expander : stx -> (values stx Σ)
  [(expander stx) (expand 0 stx (primitives-env) (Set) (init-Σ))])

(define-metafunction Lph
  parse/values : (values stx Σ) -> ast
  [(parse/values (values stx Σ)) (parse 0 stx Σ)])

(define-runner run
  reader
  expander
  stripper printer
  eval
  parse/values)

;; ----------------------------------------
;; Examples:

(define ex-prune
  ;; This example fails if we make `prune` a no-op
  '[prune
    (let-syntax ([x (lambda (stx)
                      (let ([id1 #'y])   ;; <-- pruned here
                        (let ([id2 #'y]) ;; <-- pruned here
                          (datum->syntax
                           stx
                           (list #'let-syntax
                                 (datum->syntax
                                  stx
                                  (list
                                   (datum->syntax
                                    stx
                                    (list
                                     #'f
                                     (datum->syntax
                                      stx
                                      (list
                                       #'lambda (datum->syntax stx (list id2))
                                       (datum->syntax
                                        stx
                                        (list #'second
                                              (datum->syntax
                                               stx
                                               (list #'syntax-e id1)))))))))) 
                                 #'(f '3))))))])
      (x))])
(define (raw-prune)
  (let-syntax ([x (lambda (stx)
                    (let ([id1 #'y])
                      (let ([id2 #'y])
                        #`(let-syntax ([f (lambda (#,id2)
                                            (second (syntax-e #,id1)))])
                            (f '3)))))])
    (x)))


(define ex-gen
  ;; This example works even without pruning, since
  ;; the extra scopes on `id1` and `id2` are at phase 1,
  ;; and the identifiers are resolved at phase 0
  '[gen
    (let-syntax ([x (lambda (stx)
                      (let ([id1 #'y])
                        (let ([id2 #'y])
                          (datum->syntax
                           stx
                           (list #'lambda
                                 (datum->syntax stx (list id2))
                                 id1)))))])
      ((x) 'FOO))])
(define (raw-gen)
  (let-syntax ([x (lambda (stx)
                    (let ([id1 #'y])
                      (let ([id2 #'y])
                        #`(lambda (#,id2) #,id1))))])
    (x)))

(define phases:examples
  (list ex-prune
        ex-gen))


;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           ;pict
           "config.rkt")
  (provide (all-defined-out))
  
  (define (make-expand-pict pos [contract? #f])
    (parameterize ([metafunction-cases (list pos)]
                   [linebreaks (and narrow-mode?
                                    (if contract?
                                        '(#f #t)
                                        '(#t)))])
      (WR (metafunction->pict expand #:contract? contract?))))
  (define expand-syntax-pict (make-expand-pict 2))
  (define expand-let-syntax-pict (make-expand-pict 3 #t))
  (define prune-pict
    (parameterize ([linebreaks (and narrow-mode?
                                    '(#f #f #t))])
      (WR (metafunction->pict prune #:contract? #t))))
  (define resolve-pict
    (WR (metafunction->pict resolve #:contract? #t)))
  
  (define new-nts '(ph))
  (define changed-nts '(ctx))
  (define language-delta-pict
    (WR (language->pict Lph
                        #:nts (append '(stx)
                                      changed-nts
                                      new-nts))))
  
  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Lph lw))))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view language-delta-pict
        expand-let-syntax-pict
        prune-pict
        resolve-pict))

(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-model.rkt" pict) all-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Multi-Phase"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lph #:nts (append all-nts new-nts))))
     (WR (metafunction->pict eval #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict core:strip #:contract? #t)))
     (WR (metafunction->pict expand #:contract? #t))
     (WR (metafunction->pict expand* #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict prune #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
