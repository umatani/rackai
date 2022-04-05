#lang racket
(require redex
         "common.rkt"
         (rename-in (except-in "core-machine.rkt"
                               flip add
                               bind
                               parse
                               resolve)
                    [stl->seq core:stl->seq]
                    [unzip core:unzip]
                    [zip   core:zip]
                    [snoc   core:snoc]
                    [lookup-σ core:lookup-σ]
                    [update-σ core:update-σ]
                    [update-σ* core:update-σ*]
                    [push-cont core:push-cont]
                    [alloc-loc core:alloc-loc]
                    [alloc-loc* core:alloc-loc*]

                    [δ core:δ]
                    [strip core:strip]
                    [lookup-env core:lookup-env]
                    [extend-env core:extend-env]
                    [alloc-name core:alloc-name]
                    [alloc-scope core:alloc-scope]
                    [run core:run])
         (for-syntax racket/list))

(provide Lph stl->seq unzip zip snoc
         δ
         alloc-loc*
         flip add strip prune update-ctx
         bind at-phase
         resolve
         parse
         lookup-env extend-env
         alloc-scope alloc-name regist-vars
         run phases:examples
         (all-from-out "core-machine.rkt"))

(define-extended-language Lph L
  [ph integer]
  [ctx desc-ctx]
  [desc-ctx (Map [ph scps] ...)]

  [stx∘ ::=
        stx
        (ph stx env scps) ;; to be expanded
        (Stx (Cons stx∘ stl∘) ctx)]
  [STX ::=
       hole
       (ph STX env scps)  ;; to be expanded
       (Stx (Cons STX stl∘) ctx)
       (Stx (Cons stx∘ STL) ctx)])

;; The redefinition of `ctx` changes the definition of `val`, so most
;; metafunctions need to be reinterpreted with respect to `Lph`.

(define-metafunction/extension core:stl->seq Lph
  stl->seq : stl -> (stx ...))

(define-metafunction/extension core:unzip Lph
  unzip : stl -> (values stl stl))

(define-metafunction/extension core:zip Lph
  zip : stl stl ctx -> stl)

(define-metafunction/extension core:snoc Lph
  snoc : stl stx -> stl)



;; ----------------------------------------
;; Implementation of primitives:

(define-metafunction/extension core:δ Lph
  δ : prim (val ...) -> val)

;; ----------------------------------------
;; Evaluating AST:

;; lookup-σ, update-σ, update-σ* push-cont, alloc-loc
;; に対して何故か define-metafunction/extension による
;; 再利用が機能しない

(define-metafunction Lph
  lookup-σ : σ loc -> u
  [(lookup-σ (Heap number _ ... [loc u] _ ...) loc) u])

(define-metafunction Lph
  update-σ : σ loc u -> σ
  [(update-σ (Heap number
                   [loc_0 u_0] ...
                   [loc u_old]
                   [loc_1 u_1] ...) loc u_new)
   (Heap number
         [loc_0 u_0] ...
         [loc u_new]
         [loc_1 u_1] ...)]
  [(update-σ (Heap number [loc_0 u_0] ...) loc u_new)
   (Heap number [loc u_new] [loc_0 u_0] ...)])

(define-metafunction Lph
  update-σ* : σ (loc u) ... -> σ
  [(update-σ* σ) σ]
  [(update-σ* σ (loc_0 u_0) (loc u) ...)
   (update-σ* (update-σ σ loc_0 u_0) (loc u) ...)])

(define-metafunction Lph
  push-cont : configuration continuation -> (values loc σ)
  [(push-cont configuration continuation)
   (values loc σ_1)
   (where (values loc σ) (alloc-loc configuration))
   (where σ_1 (update-σ σ loc continuation))])

(define-metafunction Lph
  alloc-loc : configuration -> (values loc σ)
  ;; for eval-time continuation
  [(alloc-loc ((App val ... clo_0 clo ...) cont (Heap number [loc u] ...)))
   (values ,(string->symbol (format "vapp:~a" (term number)))
           (Heap ,(add1 (term number)) [loc u] ...))]
  [(alloc-loc ((If clo_test clo_then clo_else) cont (Heap number [loc u] ...)))
   (values ,(string->symbol (format "vif:~a" (term number)))
           (Heap ,(add1 (term number)) [loc u] ...))]

  ;; for expand-time continuation
  [(alloc-loc (stx∘ ex? κ (Heap number [loc u] ...) Σ))
   (values ,(string->symbol (format "exp:~a" (term number)))
           (Heap ,(add1 (term number)) [loc u] ...))])

;; for eval-time value binding
(define-metafunction/extension core:alloc-loc* Lph
  alloc-loc* : (nam ...) σ -> (values (loc ...) σ))

;; same as core-machine
(define -->c
  (reduction-relation
   Lph #:domain state

   ;; propagate ρ into subterms
   (--> (((If ast_test ast_then ast_else) ρ) cont σ)
        ((If (ast_test ρ) (ast_then ρ) (ast_else ρ)) cont σ)
        ev-ρ-if)

   (--> (((App ast_fun ast_arg ...) ρ) cont σ)
        ((App (ast_fun ρ) (ast_arg ρ) ...) cont σ)
        ev-ρ-app)

   ;; value
   (--> ((val ρ) cont σ) (val cont σ) ev-val)

   ;; reference
   (--> ((var ρ) cont σ) (((lookup-σ σ (find ρ var)) ρ) cont σ) ev-x)

   ;; lambda
   (--> (((Fun (var ...) ast)   ρ) cont σ)
        (((Fun (var ...) ast ρ) ρ) cont σ)
        ev-lam)

   ;; application
   (--> (name s ((App val ... ser clo ...) cont σ))
        (ser (App val ... [] clo ... loc_new) σ_1)

        (where (values loc_new σ_1) (push-cont s cont))
        ev-push-app)

   (--> (val_0 (App val ... [] clo ... loc_cont) σ)
        ((App val ... val_0 clo ...) (lookup-σ σ loc_cont) σ)
        ev-pop-app)

   ;; β
   (--> ((App (Fun ((Var nam) ...) ast ρ) val ...) cont σ)
        ((ast ρ_new) cont σ_2)

        (where (values (loc ...) σ_1) (alloc-loc* (nam ...) σ))
        (where ρ_new (ext ρ ((Var nam) loc) ...))
        (where σ_2 (update-σ* σ_1 (loc val) ...))
        ev-β)

   ;; primitive application
   (--> ((App prim val ...) cont σ)
        ((δ prim (val ...)) cont σ)
        ev-δ)

   ;; if
   (--> (name s ((If ser_test clo_then clo_else) cont σ))
        (ser_test (If [] clo_then clo_else loc_new) σ_1)

        (where (values loc_new σ_1) (push-cont s cont))
        ev-push-if)

   (--> (val (If [] clo_then clo_else loc_cont) σ)
        ((If val clo_then clo_else) (lookup-σ σ loc_cont) σ)
        ev-pop-if)

   (--> ((If #f clo_then clo_else) cont σ)
        (clo_else cont σ)
        ev-if-#f)

   (--> ((If val clo_then clo_else) cont σ)
        (clo_then cont σ)

        (side-condition (not (equal? (term val) #f)))
        ev-if-#t)))

(define-metafunction Lph
  eval : ast -> val
  [(eval ast)
   val
   (where ((val • σ))
          ,(apply-reduction-relation* -->c (term ((ast ()) • (Heap 0)))))])

;; for debug

(define (trace--> form)
  (traces -->c (term ((,(run form 'parse) ()) • (Heap 0)))))

(define (eval--> form)
  (apply-reduction-relation* -->c (term ((,(run form 'parse) ()) • (Heap 0)))))


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
        (binds_1 ...
         [nam_1 (StoBind scps_1 nam_3) (StoBind scps_2 nam_2) ...]
         binds_2 ...)
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
   (where nam_biggest (binding-lookup (Set (StoBind scps_bind nam_bind) ...)
                                      scps_biggest))]
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
   (parse ph (Stx (Cons id_if
                        (Cons stx_test
                              (Cons stx_then (Cons stx_else ())))) ctx) Σ)
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
  ;; This is the same as the single-phase one, but with `ph`
  ;; threaded through to `add` & `bind`

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

(define-term id-kont (Stx (Sym #%kont) (Map)))
(define-term id-seq (Stx (Sym #%seq) (Map)))
(define-term id-snoc (Stx (Sym #%snoc) (Map)))

(define-term stx-nil (Stx () (Map)))

(define ==>c
  (reduction-relation
   Lph #:domain cfg #:arrow ==>

   ;; lambda
   (==> (name
         s
         ((ph (Stx (Cons id_lam (Cons (Stx stl_args ctx_0)
                                      (Cons stx_body ()))) ctx) env scps_p) ∘
          κ
          σ Σ))
        ((ph (add ph stx_body scp_new) env_new (union (Set scp_new) scps_p)) ∘
         ((Stx (Cons id_lam (Cons (Stx stl_args2 ctx_0)
                                  (Cons hole ()))) ctx) • loc_new)
         σ_1 Σ_2)

        (where lambda (resolve ph id_lam Σ))
        (where (values scp_new Σ_1) (alloc-scope Σ))
        (where (values stl_args2 env_new Σ_2)
               (regist-vars ph scp_new stl_args env Σ_1))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-lam-body)

   ;; let
   (==> (name
         s
         ((ph (Stx (Cons id_let
                         (Cons (Stx stl_binds ctx_1)
                               (Cons stx_body ()))) ctx) env scps_p) ∘
          κ
          σ Σ))
        ((ph (add ph stx_body scp_new) env_new (union (Set scp_new) scps_p)) ∘
         ((Stx (Cons id-kont
                     (Cons id_let
                           (Cons (Stx (Cons
                                       (Stx stl_vars2 ctx_1)
                                       (ph (Stx stl_rhs ctx_1) env scps_p))
                                      ctx_1)
                                 (Cons hole ())))) ctx)
          ∘ loc_new)
         σ_1 Σ_2)

        (where let (resolve ph id_let Σ))
        (where (values stl_vars stl_rhs) (unzip stl_binds))
        (where (values scp_new Σ_1) (alloc-scope Σ))
        (where (values stl_vars2 env_new Σ_2)
               (regist-vars ph scp_new stl_vars env Σ_1))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-let-body)

   (==> (name
         s
         ((Stx (Cons id_kont
                     (Cons id_let
                           (Cons (Stx (Cons
                                       (Stx stl_vars ctx_1)
                                       (ph (Stx stl_rhs ctx_1) env scps_p))
                                      ctx_1)
                                 (Cons stx_body ())))) ctx) ∘
          κ σ Σ))
        ((ph (Stx (Cons id-seq (Cons stx-nil stl_rhs)) ctx_1) env scps_p) ∘
         ((ph (Stx (Cons id_kont
                         (Cons id_let
                               (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                                hole) ctx_1)
                                     (Cons stx_body ())))) ctx) env scps_p)
          ∘ loc_new)
         σ_1 Σ)

        (where let (resolve ph id_let Σ))
        (where #%kont (resolve ph id_kont Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-let-rhs)

   (==> ((ph (Stx (Cons id_kont
                        (Cons id_let
                              (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                               (Stx val_rhs ctx_1)) ctx_1)
                                    (Cons stx_body ())))) ctx) env scps_p) ∘
         κ σ Σ)
        ((Stx (Cons id_let
                    (Cons (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                          (Cons stx_body ()))) ctx) •
         κ σ Σ)

        (where let (resolve ph id_let Σ))
        (where #%kont (resolve ph id_kont Σ))
        ex-let-rhs2)

   ;; quote
   (==> ((ph (Stx (Cons id_quote (Cons stx ())) ctx) env scps_p) ∘ κ σ Σ)
        ((Stx (Cons id_quote (Cons stx ())) ctx) • κ σ Σ)

        (where quote (resolve ph id_quote Σ))
        ex-quote)

   ;; syntax
   (==> ((ph (Stx (Cons id_syntax (Cons stx ())) ctx) env scps_p) ∘ κ σ Σ)
        ((Stx (Cons id_syntax (Cons stx_pruned ())) ctx) • κ σ Σ)

        (where syntax (resolve ph id_syntax Σ))
        (where stx_pruned (prune ph stx scps_p))
        ex-stx)

   ;; macro creation
   (==> ((ph (Stx (Cons id_ls
                        (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                              ctx_0) ()) ctx_1)
                              (Cons stx_body ()))) ctx) env scps_p) ∘ κ σ Σ)
        ((Stx (Cons id_ls
                    (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                          ctx_0) ()) ctx_1)
                          (Cons (ph stx_body env scps_p) ()))) ctx) ∘ κ σ Σ)

        (where let-syntax (resolve ph id_ls Σ))
        ex-env-ls)

   (==> (name
         s
         ((Stx (Cons id_ls
                     (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                           ctx_0) ()) ctx_1)
                           (Cons (ph stx_body env scps_p) ()))) ctx) ∘
          κ σ Σ))
        (((plus ph 1) stx_rhs (primitives-env) (Set)) ∘
         ((Stx (Cons
                id-kont
                (Cons id_ls
                      (Cons (Stx (Cons (Stx (Cons id_new (Cons hole ()))
                                            ctx_0) ()) ctx_1)
                            (Cons (ph stx_body2 env scps_p2) ())))) ctx)
          ∘ loc_new)
         σ_1 Σ_3)

        (where let-syntax (resolve ph id_ls Σ))
        (where (values nam_new Σ_1) (alloc-name id Σ))
        (where (values scp_new Σ_2) (alloc-scope Σ_1))
        (where id_new (add ph id scp_new))
        (where Σ_3 (bind ph Σ_2 id_new nam_new))
        (where stx_body2 (add ph stx_body scp_new))
        (where scps_p2 (union (Set scp_new) scps_p))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-ls-push-rhs)

   (==> ((Stx
          (Cons id_kont
                (Cons
                 id_ls
                 (Cons (Stx (Cons (Stx (Cons id_new (Cons stx_exp ()))
                                       ctx_0) ()) ctx_1)
                       (Cons (ph stx_body2 env scps_p2) ())))) ctx) ∘
         κ σ Σ)
        (in-eval (((parse (plus ph 1) stx_exp Σ) ()) • (Heap 0))
                 ((Stx (Cons (Stx (Sym nam_new) (Map))
                             (Cons (ph stx_body2 env scps_p2) ())) (Map))
                  ∘ κ σ Σ))

        (where let-syntax (resolve ph id_ls Σ))
        (where #%kont (resolve ph id_kont Σ))
        (where nam_new (resolve ph id_new Σ))
        ex-ls-eval)

   (==> (in-eval (val • σ_0)
                 ((Stx (Cons (Stx (Sym nam_new) (Map))
                             (Cons (ph stx_body2 env scps_p2) ())) (Map))
                  ∘ κ σ Σ))
        ((ph stx_body2 env_new scps_p2) ∘ κ σ Σ)

        (where env_new (extend-env env nam_new val))
        ex-ls-env)

   ;; macro invocation
   (==> ((ph stx_macapp env scps_p) ∘ κ σ Σ)
        (in-eval
         (((App val stx_macapp2) ())
          • (Heap 0))
         ((ph (Stx #f (Map [ph (Set scp_i)])) env (union (Set scp_u) scps_p)) ∘
          κ σ Σ_2))

        (where (Stx (Cons id_mac stl_args) ctx) stx_macapp)
        (where val (lookup-env env (resolve ph id_mac Σ)))
        (where (values scp_u Σ_1) (alloc-scope Σ))
        (where (values scp_i Σ_2) (alloc-scope Σ_1))
        (where stx_macapp2 (flip ph (add ph stx_macapp scp_u) scp_i))
        ex-macapp-eval)

   (==> (in-eval (stx_exp • σ_0)
                 ((ph (Stx #f (Map [ph (Set scp_i)])) env scps_p) ∘ κ σ Σ))
        ((ph (flip ph stx_exp scp_i) env scps_p) ∘ κ σ Σ)
        ex-macapp-flip)

   ;; if
   (==> (name
         s
         ((ph (Stx (Cons id_if stl_exps) ctx) env scps_p) ∘
          κ σ Σ))
        ((ph (Stx (Cons id-seq (Cons stx-nil stl_exps)) ctx) env scps_p) ∘
         ((ph (Stx (Cons id-kont (Cons id_if hole)) ctx) env scps_p)
          ∘ loc_new)
         σ_1 Σ)

        (where if (resolve ph id_if Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-if)

   (==> ((ph (Stx (Cons id_kont
                        (Cons id_if (Stx val_exps ctx))) ctx) env scps_p)
         ∘ κ σ Σ)
        ((Stx (Cons id_if val_exps) ctx) • κ σ Σ)

        (where #%kont (resolve ph id_kont Σ))
        (where if (resolve ph id_if Σ))
        ex-if-kont)

   ;; application (non-canonical #%app version)
   (==> (name
         s
         ((ph (Stx (Cons id_app (Cons stx_fun stl_args)) ctx) env scps_p) ∘
          κ σ Σ))
        ((ph (Stx (Cons id-seq
                        (Cons stx-nil
                              (Cons stx_fun stl_args))) ctx) env scps_p) ∘
         ((Stx (Cons id_app hole) ctx) • loc_new)
         σ_1 Σ)

        (where #%app (resolve ph id_app Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-#%app)

   ;; application (canonical #%app version)
   (==> (name
         s
         ((ph (Stx (Cons id_app
                         (Stx (Cons stx_fun stl_args) ctx_1)) ctx) env scps_p) ∘
          κ σ Σ))
        ((ph (Stx (Cons id-seq
                        (Cons stx-nil
                              (Cons stx_fun stl_args))) ctx) env scps_p) ∘
         ((Stx (Cons id_app hole) ctx) • loc_new)
         σ_1 Σ)

        (where #%app (resolve ph id_app Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-#%app2)

   ;; application
   (==> (name
         s
         ((ph (Stx (Cons stx_fun stl_args) ctx) env scps_p) ∘ κ σ Σ))
        ((ph (Stx (Cons id-seq
                        (Cons stx-nil
                              (Cons stx_fun stl_args))) ctx) env scps_p) ∘
         ((Stx (Cons id_app hole) ctx) • loc_new)
         σ_1 Σ)

        (side-condition
         (or (not (redex-match? Lph id (term stx_fun)))
             (let ([name (term (resolve ph stx_fun Σ))])
               (and (redex-match? Lph not-found (term (lookup-env env ,name)))
                    (not (member name
                                 '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))))
        (where id_app (Stx (Sym #%app) ctx))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-app)

   ;; reference
   (==> ((ph id env scps_p) ∘ κ σ Σ)
        (id_new • κ σ Σ)

        (where (TVar id_new) (lookup-env env (resolve ph id Σ)))
        ex-var)

   ;; literal
   (==> ((ph (Stx atom ctx) env scps_p) ∘ κ σ Σ)
        ((Stx (Cons (Stx (Sym quote) ctx) (Cons (Stx atom ctx) ())) ctx)
         • κ σ Σ)

        (side-condition (not (redex-match? Lph id (term (Stx atom ctx)))))
        ex-lit)

   ;; pop κ
   (==> (stx • (STX ex? loc) σ Σ)
        ((in-hole STX stx) ex? κ σ Σ)

        (where κ (lookup-σ σ loc))
        ex-pop-κ)

   ;; expression sequence
   ;;  (expand (seq (exped ...))) --> (exped ...)
   (==> ((ph (Stx (Cons id_seq
                        (Cons (Stx val_expeds (Map)) ())) ctx) env scps_p) ∘
         κ σ Σ)
        ((Stx val_expeds ctx) • κ σ Σ)

        (where #%seq (resolve ph id_seq Σ))
        ex-seq-nil)

   ;; (expand (seq (done ...) exp0 exp ...)) -->
   ;;   (expand (seq (done ... (expand exp0)) exp ...))
   (==> (name
         s
         ((ph (Stx (Cons id_seq
                         (Cons (Stx val_dones (Map))
                               (Cons stx_exp0 stl_exps))) ctx) env scps_p) ∘
          κ σ Σ))
        ((ph stx_exp0 env scps_p) ∘
         ((ph (Stx (Cons id-kont
                         (Cons id_seq
                               (Cons
                                (Stx (Cons id-snoc
                                           (Cons (Stx val_dones (Map)) hole))
                                     (Map))
                                     stl_exps))) ctx) env scps_p) ∘ loc_new)
         σ_1 Σ)

        (where #%seq (resolve ph id_seq Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-seq-cons)

   (==> ((ph (Stx (Cons id_kont
                        (Cons id_seq
                              (Cons (Stx (Cons id_snoc
                                               (Cons (Stx val_exps ctx_1)
                                                     (Stx val_exp ctx_2)))
                                         (Map))
                                    stl_exps))) ctx) env scps_p) ∘
         κ σ Σ)
        ((ph (Stx (Cons id_seq
                        (Cons (Stx val_exps2 ctx_1)
                              stl_exps)) ctx) env scps_p) ∘
         κ σ Σ)

        (where #%seq (resolve ph id_seq Σ))
        (where #%kont (resolve ph id_kont Σ))
        (where #%snoc (resolve ph id_snoc Σ))
        (where val_exps2 (snoc val_exps (Stx val_exp ctx_2)))
        ex-seq-snoc)


   ;; one-step eval (-->c)
   (-->c state
         state_new
         (where (state_new)
                ,(apply-reduction-relation -->c (term state))))

   with
   ((==> (in-eval s1 cfg) (in-eval s2 cfg))
    (-->c s1 s2))))

(define-metafunction Lph
  expand : ph stx env scps Σ -> (values stx Σ)
  [(expand ph stx env scps_p Σ)
   (values stx_new Σ_new)
   (where ((stx_new • • σ_new Σ_new))
          ,(apply-reduction-relation*
            ==>c
            (term ((ph stx env scps_p) ∘ • (Heap 0) Σ))))])

;; for debug

(define (step==> form)
  (stepper
   ==>c (term ((0 ,(run form 'read) (primitives-env) (Set))
               ∘ • (Heap 0) (init-Σ)))))

(define (trace==> form)
  (traces
   ==>c (term ((0 ,(run form 'read) (primitives-env) (Set))
               ∘ • (Heap 0) (init-Σ)))))

(define (expand==> form)
  (apply-reduction-relation*
   ==>c (term ((0 ,(run form 'read) (primitives-env) (Set))
               ∘ • (Heap 0) (init-Σ)))))

(define (expand&parse form)
  (let ([r (expand==> form)])
    (and (= (length r) 1)
         (term (parse/values (values ,(caar r) ,(fifth (car r))))))))


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

  #;
  (define (make-expand-pict pos [contract? #f])
    (parameterize ([metafunction-cases (list pos)]
                   [linebreaks (and narrow-mode?
                                    (if contract?
                                        '(#f #t)
                                        '(#t)))])
      (WR (metafunction->pict expand #:contract? contract?))))
  #;(define expand-syntax-pict (make-expand-pict 2))
  #;(define expand-let-syntax-pict (make-expand-pict 3 #t))

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

(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view language-delta-pict
        ;expand-let-syntax-pict
        prune-pict
        resolve-pict))

(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-machine.rkt" pict) all-nts))
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
     #;(WR (metafunction->pict expand #:contract? #t))
     #;(WR (metafunction->pict expand* #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict prune #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
