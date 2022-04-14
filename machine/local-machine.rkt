#lang racket
(require redex redex/parameter
         "common.rkt"
         slideshow/pict
         (only-in "core-machine.rkt"
                  plus biggest-subset
                  primitives-env init-Σ union
                  core:examples
                  [run core:run]
                  [lookup-σ core:lookup-σ]
                  [update-σ core:update-σ]
                  [update-σ* core:update-σ*]
                  [alloc-loc core:alloc-loc]
                  [alloc-loc* core:alloc-loc*]
                  [push-cont core:push-cont])
         (only-in "phases-machine.rkt"
                  Lph unzip zip snoc
                  δ flip add strip prune
                  bind resolve parse
                  lookup-env extend-env
                  alloc-scope alloc-name regist-vars
                  phases:examples
                  [run phases:run])
         (for-syntax racket/list))

(provide Lloc
         extend-env*
         unstop
         run local:examples
         (all-from-out "phases-machine.rkt"))

(define-extended-language Lloc Lph
  [maybe-scp ::= scp no-scope] ; new
  [Σ* ::= (Tup Σ scps scps)] ; new

  [cont ::=
        •
        (App (ph maybe-scp env) val ... [] clo ... loc) ; updated (ph maybe-scp env)
        (If [] clo clo loc)]
  [ser ::=
       (ph ast ρ maybe-scp env) ; updated (ph ρ maybe-scp env)
       (App (ph maybe-scp env) clo ...) ; updated (ph maybe-scp env)
       (If clo clo clo)
       (Seq clo ...) ; new
       ]
  [state ::=
         (clo cont σ Σ*) ; updated (Σ*)
         (in-expand cfg state) ; new
         ]

  [stx∘ ::=
        stx
        (ph stx env) ; updated (remove scps)
        (Stx (Cons stx∘ stl∘) ctx)]
  [STX ::=
       hole
       (ph STX env)  ; updated (remove scps)
       (Stx (Cons STX stl∘) ctx)
       (Stx (Cons stx∘ STL) ctx)]
  [κ ::=
     •
     (STX ex? Σ* loc) ; updated (Σ*)
     ]
  [cfg ::=
       (stx∘ ex? κ σ Σ*) ; updated (Σ -> Σ*)
       (in-eval state cfg)]
  )

;; ----------------------------------------
;; Evaluating AST:

(define-extended-metafunction* core:lookup-σ Lloc
  lookup-σ : σ loc -> u)

(define-extended-metafunction* core:update-σ Lloc
  update-σ : σ loc u -> σ)

(define-extended-metafunction* core:update-σ* Lloc
  update-σ* : σ (loc u) ... -> σ)

(define-extended-metafunction* core:alloc-loc Lloc
  alloc-loc : σ -> (values loc σ))

;; for eval-time value binding
(define-extended-metafunction* core:alloc-loc* Lloc
  alloc-loc* : (nam ...) σ -> (values (loc ...) σ))

(define-extended-metafunction* core:push-cont Lloc
  push-cont : σ continuation -> (values loc σ))

(define-metafunction Lloc
  extend-env* : env ((nam all-transform) ...) -> env
  [(extend-env* env ((nam all-transform) ...)) ((nam all-transform) ... . env)])

(define-metafunction Lloc
  unstop : all-transform -> all-transform
  [(unstop (TStop all-transform)) all-transform]
  [(unstop all-transform) all-transform])

(define-metafunction Lloc
  resolve* : ph val Σ -> (nam ...)
  [(resolve* ph () Σ) ()]
  [(resolve* ph (Cons id val) Σ)
   ((resolve ph id Σ) nam ...)
   (where (nam ...) (resolve* ph val Σ))])

(define-reduction-relation -->c
  Lloc #:domain state

  ;; propagate ρ into subterms
  (--> ((ph (If ast_test ast_then ast_else) ρ maybe-scp env) cont σ Σ*)
       ((If (ph ast_test ρ maybe-scp env)
            (ph ast_then ρ maybe-scp env)
            (ph ast_else ρ maybe-scp env)) cont σ Σ*)
       ev-ρ-if)

  (--> ((ph (App ast_fun ast_arg ...) ρ maybe-scp env) cont σ Σ*)
       ((App (ph maybe-scp env)
             (ph ast_fun ρ maybe-scp env)
             (ph ast_arg ρ maybe-scp env) ...) cont σ Σ*)
       ev-ρ-app)

  ;; local value
  (--> ((App (ph maybe-scp env)
             syntax-local-value id) cont σ Σ*)
       ((lookup-env env (resolve ph id Σ)) cont σ Σ*)
       (where (Tup Σ _ _) Σ*)
       ev-lval)

  ;; local expand
  (--> ((App (ph scp_i env)
             local-expand stx any_contextv val_idstops) cont σ Σ*)
       (in-expand ((ph (flip ph stx scp_i) env_stops) ∘ • (Heap 0) Σ*)
                  ((App (ph scp_i env) local-expand2) cont σ Σ*))

       (where env_unstops
              ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p)))))
                    (term env)))
       (where (Tup Σ _ _) Σ*)
       (where (nam_stop ...) (resolve* ph val_idstops Σ))
       (where env_stops
              (extend-env*
               env_unstops
               ((nam_stop (TStop (lookup-env env_unstops nam_stop)))
                ...)))
       ev-lexpand)

  (--> (in-expand (stx_exp • • σ_new Σ*)
                  ((App (ph scp_i env) local-expand2) cont σ _))
       ((flip ph stx_exp scp_i) cont σ Σ*)
       ev-lexpand2)

  ;; local binder
  (--> ((App (ph maybe-scp env)
             syntax-local-identifier-as-binding id) cont σ Σ*)
       ((prune ph id scps_u) cont σ Σ*)

       (where (Tup _ _ scps_u) Σ*)
       ev-lbinder)

  ;; value
  (--> ((ph val ρ maybe-scp env) cont σ Σ*)
       (val cont σ Σ*)
       ev-val)

  ;; reference
  (--> ((ph var ρ maybe-scp env) cont σ Σ*)
       ((ph (lookup-σ σ (find ρ var)) ρ maybe-scp env) cont σ Σ*)
       ev-x)

  ;; lambda
  (--> ((ph (Fun (var ...) ast) ρ maybe-scp env) cont σ Σ*)
       ((ph (Fun (var ...) ast ρ) ρ maybe-scp env) cont σ Σ*)
       ev-lam)

  ;; application
  (--> ((App (ph maybe-scp env)
             val ... ser clo ...) cont σ Σ*)
       (ser (App (ph maybe-scp env)
                 val ... [] clo ... loc_new) σ_1 Σ*)

       (where (values loc_new σ_1) (push-cont σ cont))
       ev-push-app)

  (--> (val_0 (App (ph maybe-scp env)
                   val ... [] clo ... loc_cont) σ Σ*)
       ((App (ph maybe-scp env)
             val ... val_0 clo ...) (lookup-σ σ loc_cont) σ Σ*)
       ev-pop-app)

  ;; β
  (--> ((App (ph maybe-scp env)
             (Fun ((Var nam) ...) ast ρ) val ...) cont σ Σ*)
       ((ph ast ρ_new maybe-scp env) cont σ_2 Σ*)

       (where (values (loc ...) σ_1) (alloc-loc* (nam ...) σ))
       (where ρ_new (ext ρ ((Var nam) loc) ...))
       (where σ_2 (update-σ* σ_1 (loc val) ...))
       ev-β)

  ;; primitive application
  (--> ((App (ph maybe-scp env)
             prim val ...) cont σ Σ*)
       ((δ prim (val ...)) cont σ Σ*)

       (side-condition (not (redex-match? Lloc stx-prim (term prim))))
       ev-δ)

  ;; if
  (--> ((If ser_test clo_then clo_else) cont σ Σ*)
       (ser_test (If [] clo_then clo_else loc_new) σ_1 Σ*)

       (where (values loc_new σ_1) (push-cont σ cont))
       ev-push-if)

  (--> (val (If [] clo_then clo_else loc_cont) σ Σ*)
       ((If val clo_then clo_else) (lookup-σ σ loc_cont) σ Σ*)
       ev-pop-if)

  (--> ((If #f clo_then clo_else) cont σ Σ*)
       (clo_else cont σ Σ*)
       ev-if-#f)

  (--> ((If val clo_then clo_else) cont σ Σ*)
       (clo_then cont σ Σ*)

       (side-condition (not (equal? (term val) #f)))
       ev-if-#t)

  ;; one-step eval (==>c)
  (==>c cfg cfg_new
        (where (cfg_new)
               ,(apply-reduction-relation ==>c (term cfg))))

  with
  ((--> (in-expand c1 state) (in-expand c2 state))
   (==>c c1 c2)))

(define-metafunction Lloc
  eval : ph ast maybe-scp env Σ* -> (values val Σ*)
  [(eval ph ast maybe-scp env Σ*)
   (values val Σ*_2)
   (where ((val • σ Σ*_2))
          ,(apply-reduction-relation*
            -->c
            (term ((ph ast () maybe-scp env) • (Heap 0) Σ*))))])

;; for debug

(define (trace--> form)
  (traces
   -->c
   (term ((0 ,(run form 'parse) () no-scope ())
          • (Heap 0) (Tup (init-Σ) (Set) (Set))))))

(define (eval--> form)
  (apply-reduction-relation*
   -->c
   (term ((0 ,(run form 'parse) () no-scope ())
          • (Heap 0) (Tup (init-Σ) (Set) (Set))))))


;; ----------------------------------------
;; The expander:

(define-term id-kont (Stx (Sym #%kont) (Map)))
(define-term id-seq (Stx (Sym #%seq) (Map)))
(define-term id-snoc (Stx (Sym #%snoc) (Map)))
(define-term stx-nil (Stx () (Map)))

(define-reduction-relation ==>c
  Lloc
  #:domain cfg #:arrow ==> ;; cfg = (stx∘ ex? κ σ Σ*)

  ;; stops
  (==> ((ph (Stx (Cons id_stop stl_args) ctx) env) ∘ κ σ Σ*)
       ((Stx (Cons id_stop stl_args) ctx) • κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where (TStop _) (lookup-env env (resolve ph id_stop Σ)))
       ex-stop)

  ;; lambda (unchanged)
  (==> ((ph (Stx (Cons id_lam (Cons (Stx stl_args ctx_0)
                                    (Cons stx_body ()))) ctx) env)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph (add ph stx_body scp_new) env_new)
        ∘
        ((Stx (Cons id_lam (Cons (Stx stl_args2 ctx_0)
                                 (Cons hole ()))) ctx)
         • (Tup Σ scps_p scps_u) loc_new) ;; Σ not used
        σ_1 Σ*_2)

       (where lambda (resolve ph id_lam Σ))
       (where (values scp_new Σ_1) (alloc-scope Σ))
       (where (values stl_args2 env_new Σ_2)
              (regist-vars ph scp_new stl_args env Σ_1))
       (where Σ*_2 (Tup Σ_2 (union (Set scp_new) scps_p) (Set)))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-lam-body)

  ;; let
  (==> ((ph (Stx (Cons id_let
                       (Cons (Stx stl_binds ctx_1)
                             (Cons stx_body ()))) ctx) env)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph (add ph stx_body scp_new) env_new)
        ∘
        ((Stx (Cons id-kont
                    (Cons id_let
                          (Cons (Stx (Cons
                                      (Stx stl_vars2 ctx_1)
                                      (ph (Stx stl_rhs ctx_1) env))
                                     ctx_1)
                                (Cons hole ())))) ctx)
         ∘ (Tup Σ scps_p scps_u) loc_new) ;; Σ not used
        σ_1 Σ*_2)

       (where let (resolve ph id_let Σ))
       (where (values stl_vars stl_rhs) (unzip stl_binds))
       (where (values scp_new Σ_1) (alloc-scope Σ))
       (where (values stl_vars2 env_new Σ_2)
              (regist-vars ph scp_new stl_vars env Σ_1))
       (where Σ*_2 (Tup Σ_2 (union (Set scp_new) scps_p) (Set)))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-let-body)

  (==> ((Stx (Cons id_kont
                   (Cons id_let
                         (Cons (Stx (Cons
                                     (Stx stl_vars ctx_1)
                                     (ph (Stx stl_rhs ctx_1) env))
                                    ctx_1)
                               (Cons stx_body ())))) ctx)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq (Cons stx-nil stl_rhs)) ctx_1) env)
        ∘
        ((ph (Stx (Cons id_kont
                        (Cons id_let
                              (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                               hole) ctx_1)
                                    (Cons stx_body ())))) ctx) env)
         ∘ (Tup Σ scps_p scps_u) loc_new)
        σ_1 (Tup Σ scps_p (Set)))

       (where let (resolve ph id_let Σ))
       (where #%kont (resolve ph id_kont Σ))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-let-rhs)

  (==> ((ph (Stx (Cons id_kont
                       (Cons id_let
                             (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                              (Stx val_rhs ctx_1)) ctx_1)
                                   (Cons stx_body ())))) ctx) env)
        ∘ κ σ Σ*)
       ((Stx (Cons id_let
                   (Cons (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                         (Cons stx_body ()))) ctx)
        • κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where let (resolve ph id_let Σ))
       (where #%kont (resolve ph id_kont Σ))
       ex-let-rhs2)

  ;; quote (unchanged)
  (==> ((ph (Stx (Cons id_quote (Cons stx ())) ctx) env) ∘ κ σ Σ*)
       ((Stx (Cons id_quote (Cons stx ())) ctx) • κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where quote (resolve ph id_quote Σ))
       ex-quote)

  ;; syntax (unchanged)
  (==> ((ph (Stx (Cons id_syntax (Cons stx ())) ctx) env) ∘ κ σ Σ*)
       ((Stx (Cons id_syntax (Cons stx_pruned ())) ctx) • κ σ Σ*)

       (where (Tup Σ scps_p scps_u) Σ*)
       (where syntax (resolve ph id_syntax Σ))
       (where stx_pruned (prune ph stx scps_p))
       ex-stx)

  ;; macro creation (eval gets more and updates store)
  (==> ((ph (Stx (Cons id_ls
                       (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                             ctx_0) ()) ctx_1)
                             (Cons stx_body ()))) ctx) env) ∘ κ σ Σ*)
       ((Stx (Cons id_ls
                   (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                         ctx_0) ()) ctx_1)
                         (Cons (ph stx_body env) ()))) ctx) ∘ κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where let-syntax (resolve ph id_ls Σ))
       ex-env-ls)

  (==> ((Stx (Cons id_ls
                   (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                         ctx_0) ()) ctx_1)
                         (Cons (ph stx_body env) ()))) ctx)
        ∘ κ σ (Tup Σ scps_p scps_u))
       (((plus ph 1) stx_rhs (primitives-env))
        ∘
        ((Stx (Cons
               id-kont
               (Cons id_ls
                     (Cons (Stx (Cons (Stx (Cons id_new (Cons hole ()))
                                           ctx_0) ()) ctx_1)
                           (Cons (ph stx_body env)
                                 (Stx #f (Map [ph (Set scp_new)])))))) ctx)
         ∘ (Tup Σ scps_p scps_u) loc_new) ;; Σ not used
        σ_1 (Tup Σ_3 (Set) (Set)))

       (where let-syntax (resolve ph id_ls Σ))
       (where (values nam_new Σ_1) (alloc-name id Σ))
       (where (values scp_new Σ_2) (alloc-scope Σ_1))
       (where id_new (add ph id scp_new))
       (where Σ_3 (bind ph Σ_2 id_new nam_new))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-ls-push-rhs)

  (==> ((Stx
         (Cons id_kont
               (Cons id_ls
                     (Cons (Stx (Cons (Stx (Cons id (Cons stx_exp ()))
                                           ctx_0) ()) ctx_1)
                           (Cons (ph stx_body env)
                                 (Stx #f (Map [ph (Set scp_new)])))))) ctx)
        ∘ κ σ (Tup Σ scps_p _))
       (in-eval ((ph (parse (plus ph 1) stx_exp Σ) () no-scope env)
                 • (Heap 0) (Tup Σ scps_p (Set)))
                ((Stx (Cons (Stx (Sym nam_new) (Map))
                            (Cons (ph stx_body env)
                                  (Stx #f (Map [ph (Set scp_new)])))) (Map))
                 ∘ κ σ (Tup Σ scps_p (Set)))) ;; Σ not used

       (where let-syntax (resolve ph id_ls Σ))
       (where #%kont (resolve ph id_kont Σ))
       (where nam_new (resolve ph id Σ))
       ex-ls-eval)

  (==> (in-eval (val • σ_0 (Tup Σ _ _))
                ((Stx (Cons (Stx (Sym nam_new) (Map))
                            (Cons (ph stx_body env)
                                  (Stx #f (Map [ph (Set scp_new)])))) (Map))
                 ∘ κ σ (Tup _ scps_p _)))
       ((ph stx_body2 env_new)
        ∘ κ σ (Tup Σ (union (Set scp_new) scps_p) (Set)))

       (where env_new (extend-env env nam_new val))
       (where stx_body2 (add ph stx_body scp_new))
       ex-ls-env)

  ;; macro invocation
  (==> ((ph stx_macapp env) ∘ κ σ (Tup Σ scps_p scps_u))
       (in-eval ((ph (App val stx_macapp2) () scp_i env) • (Heap 0) Σ*_2)
                ((ph (Stx #f (Map [ph (Set scp_i)])) env)
                 ∘ κ σ Σ*_2)) ;; Σ*_2 not used

       (where (Stx (Cons id_mac stl_args) ctx) stx_macapp)
       (where val (lookup-env env (resolve ph id_mac Σ)))
       (where (values scp_u Σ_1) (alloc-scope Σ))
       (where (values scp_i Σ_2) (alloc-scope Σ_1))
       (where Σ*_2 (Tup Σ_2
                         (union (Set scp_u) scps_p)
                         (union (Set scp_u) scps_u)))
       (where stx_macapp2 (flip ph (add ph stx_macapp scp_u) scp_i))
       ex-macapp-eval)

  (==> (in-eval (stx_exp • σ_0 Σ*)
                ((ph (Stx #f (Map [ph (Set scp_i)])) env) ∘ κ σ _))
       ((ph (flip ph stx_exp scp_i) env) ∘ κ σ Σ*)
       ex-macapp-flip)

  ;; if
  (==> ((ph (Stx (Cons id_if stl_exps) ctx) env)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq (Cons stx-nil stl_exps)) ctx) env)
        ∘
        ((ph (Stx (Cons id-kont (Cons id_if hole)) ctx) env)
         ∘ (Tup Σ scps_p scps_u) loc_new) ;; Σ not used
        σ_1 (Tup Σ scps_p (Set)))

       (where if (resolve ph id_if Σ))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-if)

  (==> ((ph (Stx (Cons id_kont
                       (Cons id_if (Stx val_exps ctx))) ctx) env)
        ∘ κ σ Σ*)
       ((Stx (Cons id_if val_exps) ctx) • κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where #%kont (resolve ph id_kont Σ))
       (where if (resolve ph id_if Σ))
       ex-if-kont)

  ;; application (non-canonical #%app version, unchanged)
  (==> ((ph (Stx (Cons id_app (Cons stx_fun stl_args)) ctx) env)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq
                       (Cons stx-nil
                             (Cons stx_fun stl_args))) ctx) env)
        ∘
        ((Stx (Cons id_app hole) ctx) • (Tup Σ scps_p scps_u) loc_new)
        σ_1 (Tup Σ scps_p (Set)))

       (where #%app (resolve ph id_app Σ))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-#%app)

  ;; application (canonical #%app version, unchanged)
  (==> ((ph (Stx (Cons id_app
                       (Stx (Cons stx_fun stl_args) ctx_1)) ctx) env)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq
                       (Cons stx-nil
                             (Cons stx_fun stl_args))) ctx) env)
        ∘
        ((Stx (Cons id_app hole) ctx) • (Tup Σ scps_p scps_u) loc_new)
        σ_1 (Tup Σ scps_p (Set)))

       (where #%app (resolve ph id_app Σ))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-#%app2)

  ;; application (unchanged)
  (==> ((ph (Stx (Cons stx_fun stl_args) ctx) env)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq
                       (Cons stx-nil
                             (Cons stx_fun stl_args))) ctx) env)
        ∘
        ((Stx (Cons id_app hole) ctx) • (Tup Σ scps_p scps_u) loc_new)
        σ_1 (Tup Σ scps_p (Set)))

       (side-condition
        (or (not (redex-match? Lloc id (term stx_fun)))
            (let ([name (term (resolve ph stx_fun Σ))])
              (and (redex-match? Lloc not-found (term (lookup-env env ,name)))
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))))
       (where id_app (Stx (Sym #%app) ctx))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-app)

  ;; reference (unchanged)
  (==> ((ph id env) ∘ κ σ Σ*)
       (id_new • κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where (TVar id_new) (lookup-env env (resolve ph id Σ)))
       ex-var)

  ;; literal (unchanged)
  (==> ((ph (Stx atom ctx) env) ∘ κ σ Σ*)
       ((Stx (Cons (Stx (Sym quote) ctx) (Cons (Stx atom ctx) ())) ctx)
        • κ σ Σ*)

       (side-condition (not (redex-match? Lloc id (term (Stx atom ctx)))))
       ex-lit)

  ;; pop κ (merge Σ*)
  (==> (stx • (STX ex? (Tup _ scps_p scps_u) loc) σ (Tup Σ _ _))
       ((in-hole STX stx) ex? κ σ (Tup Σ scps_p scps_u))

       (where κ (lookup-σ σ loc))
       ex-pop-κ)

  ;; expression sequence
  ;;  (expand (seq (exped ...))) --> (exped ...)
  (==> ((ph (Stx (Cons id_seq
                       (Cons (Stx val_expeds (Map)) ())) ctx) env)
        ∘ κ σ Σ*)
       ((Stx val_expeds ctx) • κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where #%seq (resolve ph id_seq Σ))
       ex-seq-nil)

  ;; (expand (seq (done ...) exp0 exp ...)) -->
  ;;   (expand (seq (done ... (expand exp0)) exp ...))
  (==> ((ph (Stx (Cons id_seq
                       (Cons (Stx val_dones (Map))
                             (Cons stx_exp0 stl_exps))) ctx) env)
        ∘ κ σ (Tup Σ scps_p scps_u))
       ((ph stx_exp0 env)
        ∘
        ((ph (Stx (Cons id-kont
                        (Cons id_seq
                              (Cons
                               (Stx (Cons id-snoc
                                          (Cons (Stx val_dones (Map)) hole))
                                    (Map))
                               stl_exps))) ctx) env)
         ∘ (Tup Σ scps_p scps_u) loc_new)
        σ_1 (Tup Σ scps_p (Set)))

       (where #%seq (resolve ph id_seq Σ))
       (where (values loc_new σ_1) (push-cont σ κ))
       ex-seq-cons)

  (==> ((ph (Stx (Cons id_kont
                       (Cons id_seq
                             (Cons (Stx (Cons id_snoc
                                              (Cons (Stx val_exps ctx_1)
                                                    (Stx val_exp ctx_2)))
                                        (Map))
                                   stl_exps))) ctx) env)
        ∘ κ σ Σ*)
       ((ph (Stx (Cons id_seq
                       (Cons (Stx val_exps2 ctx_1)
                             stl_exps)) ctx) env)
        ∘ κ σ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where #%seq (resolve ph id_seq Σ))
       (where #%kont (resolve ph id_kont Σ))
       (where #%snoc (resolve ph id_snoc Σ))
       (where val_exps2 (snoc val_exps (Stx val_exp ctx_2)))
       ex-seq-snoc)


  ;; one-step eval (-->c)
  (-->c state state_new
        (where (state_new) ,(apply-reduction-relation -->c (term state))))

  with
  ((==> (in-eval s1 cfg) (in-eval s2 cfg))
   (-->c s1 s2)))

(define-metafunction Lloc
  expand : ph stx env Σ* -> (values stx Σ*)
  [(expand ph stx env Σ*)
   (values stx_new Σ*_new)
   (where ((stx_new • • σ_new Σ*_new))
          ,(apply-reduction-relation*
            ==>c
            (term ((ph stx env) ∘ • (Heap 0) Σ*))))])

;; for debug

(define (step==> form)
  (stepper
   ==>c (term ((0 ,(run form 'read) (primitives-env))
               ∘ • (Heap 0) (Tup (init-Σ) (Set) (Set))))))

(define (trace==> form)
  (traces
   ==>c (term ((0 ,(run form 'read) (primitives-env))
               ∘ • (Heap 0) (Tup (init-Σ) (Set) (Set))))))

(define (expand==> form)
  (apply-reduction-relation*
   ==>c (term ((0 ,(run form 'read) (primitives-env))
               ∘ • (Heap 0) (Tup (init-Σ) (Set) (Set))))))

(define (expand&parse form)
  (let ([r (expand==> form)])
    (and (= (length r) 1)
         (term (parse/values (values ,(caar r) ,(fifth (car r))))))))


;; ----------------------------------------
;; Drivers

(define-helpers Lloc (Map)
  reader printer)

(define-metafunction Lloc
  stripper : (values stx Σ*) -> val
  [(stripper (values stx Σ*)) (strip stx)])

(define-metafunction Lloc
  expander : stx -> (values stx Σ*)
  [(expander stx) (expand 0 stx (primitives-env) (Tup (init-Σ) (Set) (Set)))])

(define-metafunction Lloc
  parse/values : (values stx Σ*) -> ast
  [(parse/values (values stx (Tup Σ _ _))) (parse 0 stx Σ)])

(define-metafunction Lloc
  evaluate : ast -> val
  [(evaluate ast)
   val
   (where (values val Σ*)
          (eval 0 ast no-scope (primitives-env) (Tup (init-Σ) (Set) (Set))))])

(define-runner run
  reader
  expander
  stripper printer
  evaluate
  parse/values)

;; ----------------------------------------
;; Examples:

(define ex-local-value
  '[local-value
    (let-syntax ([a '8])
      (let-syntax ([b '9])
        (let-syntax ([x (lambda (stx)
                          (datum->syntax
                           #'here
                           (list #'quote
                                 (datum->syntax
                                  #'here
                                  (syntax-local-value (second (syntax-e stx)))))))])
          (x a))))])


(define ex-local-expand
  '[local-expand
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx)
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (second (syntax-e (cdr (syntax-e (local-expand
                                                          (second (syntax-e stx))
                                                          'expression
                                                          '()))))))])
        (x (q))))])
(define (raw-local-expand)
  (let-syntax ([q (lambda (stx) #'(car 8))])
    (let-syntax ([x (lambda (stx)
                      ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                      (let ([stx2 (syntax-e (local-expand
                                             (second (syntax-e stx))
                                             'expression
                                             '()))])
                        (second (syntax-e (cdr stx2)))))])
      (x (q)))))


(define ex-local-expand-stop
  '[local-expand-stop
    (let-syntax ([p (lambda (stx) '0)])
      (let-syntax ([q (lambda (stx) #'(car 8))])
        (let-syntax ([x (lambda (stx)
                          ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                          (second (syntax-e (cdr (syntax-e (local-expand
                                                            (second (syntax-e stx))
                                                            'expression
                                                            (list #'p)))))))])
          (x (q)))))])
(define (raw-local-expand-stop)
  (let-syntax ([p (lambda (stx) '0)])
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx)
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (let ([stx2 (syntax-e (local-expand
                                               (second (syntax-e stx))
                                               'expression
                                               (list #'p)))])
                          (second (syntax-e (cdr stx2)))))])
        (x (q))))))


(define ex-nested-local-expand
  '[nested-local-expand
    (let-syntax ([z (lambda (stx) #''0)])
      (let-syntax ([a (lambda (stx)
                        ;; When `b' forces `a', then `a'
                        ;; drops `z' form the stop list, so it
                        ;; should expand to 0
                        (local-expand (second (syntax-e stx))
                                      'expression
                                      '()))])
        (let-syntax ([b (lambda (stx)
                          (datum->syntax
                           stx
                           (list
                            #'quote
                            (local-expand (second (syntax-e stx))
                                          'expression
                                          (list #'z)))))])
          (list (b (z)) (b (a (z)))))))])
(define (raw-nested-local-expand)
  (let-syntax ([z (lambda (stx) #''0)])
    (let-syntax ([a (lambda (stx)
                      ;; When `b' forces `a', then `a'
                      ;; drops `z' form the stop list, so it
                      ;; should expand to 0
                      (local-expand (second (syntax-e stx))
                                    'expression
                                    '()))])
      (let-syntax ([b (lambda (stx)
                        (datum->syntax
                         stx
                         (list
                          #'quote
                          (local-expand (second (syntax-e stx))
                                        'expression
                                        (list #'z)))))])
        (list (b (z)) (b (a (z))))))))


(define ex-local-binder
  '[local-binder
    (let-syntax ([q (lambda (stx)
                      ;; quotes its argument
                      (datum->syntax
                       stx
                       (list #'quote (second (syntax-e stx)))))])
      (let-syntax ([a (lambda (stx)
                        ;; expands first argument, expected quoted name
                        ;; to use as binder with second arguments body
                        (datum->syntax
                         stx
                         (list
                          #'lambda
                          (datum->syntax
                           stx
                           (list (syntax-local-identifier-as-binding
                                  (second (syntax-e
                                           (local-expand (second (syntax-e stx))
                                                         'expression
                                                         '()))))))
                          (third (syntax-e stx)))))])
        ;; removing the syntax-local-identifier-as-binding call above
        ;; leaves the second `x` as unbound:
        ;; TODO: 実装と不一致．取り除いても↓の実装では unbound にならない
        ((a (q x) x) 'FOOOO)))])
(define (raw-local-binder)
  (let-syntax ([q (lambda (stx)
                    ;; quotes its argument
                    (datum->syntax
                     stx
                     (list #'quote (second (syntax-e stx)))))])
    (let-syntax ([a (lambda (stx)
                      ;; expands first argument, expected quoted name
                      ;; to use as binder with second arguments body
                      (datum->syntax
                       stx
                       (list
                        #'lambda
                        (datum->syntax
                         stx
                         (list (syntax-local-identifier-as-binding
                                (second (syntax-e
                                         (local-expand (second (syntax-e stx))
                                                       'expression
                                                       '()))))))
                        (third (syntax-e stx)))))])
      ;; removing the syntax-local-identifier-as-binding call above
      ;; leaves the second `x` as unbound:
      ;; TODO: 実装と不一致．取り除いても unbound にならない
      (a (q x) x))))

(define local:examples
  (list ex-local-value
        ex-local-expand
        ex-local-expand-stop
        ex-nested-local-expand
        ex-local-binder))

(define main
  (let ([all-runs `([core ,core:run]
                    [phases ,phases:run]
                    [local ,run])]
        [all-examples (list core:examples
                            phases:examples
                            local:examples)])
    (run-all-examples all-runs all-examples)))


;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           ;pict
           "config.rkt")
  (provide (all-defined-out))

  #;
  (define eval-pict
    (if narrow-mode?
        ;; Independent form:
        (vl-append
         (metafunction-rule-gap-space)
         (parameterize ([metafunction-cases '(0)])
           (WR (metafunction->pict eval #:contract? #t)))
         (parameterize ([metafunction-cases '(1)])
           (WR (metafunction->pict eval)))
         (parameterize ([metafunction-cases '(2)])
           (WR (metafunction->pict eval))))
        ;; Table form:
        (parameterize ([metafunction-cases '(0 1 2)])
          (WR (metafunction->pict eval #:contract? #t)))))

  #;
  (define (make-expand-pict pos [contract? #f])
    (parameterize ([metafunction-cases (list pos)])
      (WR (metafunction->pict expand #:contract? contract?))))

  #;(define expand-stop-pict (make-expand-pict 0 #t))
  #;(define expand-macro-app-pict (make-expand-pict 5))
  #;
  (define expand-lambda-pict
    (parameterize ([linebreaks (list #t)])
      (make-expand-pict 1)))

  (define unstop-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict unstop #:contract? #t))))

  (define newer-nts '(maybe-scp Σ*))
  (define language-delta-pict
    (WR (language->pict Lloc #:nts newer-nts)))

  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Lloc lw))))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view language-delta-pict
        eval-pict
        expand-stop-pict
        expand-lambda-pict
        unstop-pict))

(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-machine.rkt" pict) all-nts)
           (only-in (submod "phases-machine.rkt" pict)
                    changed-nts
                    new-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Local-Expansion"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lloc #:nts (append newer-nts
                                              new-nts
                                              all-nts))))
     (WR (metafunction->pict eval #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict unstop #:contract? #t)))
     #;
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     #;
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict core:strip #:contract? #t)))
     #;
     (parameterize ([linebreaks '(#f #f #t #f #t #f #f #t #f)])
       (WR (metafunction->pict expand #:contract? #t)))
     #;
     (parameterize ([linebreaks '(#f #f #t)])
       (WR (metafunction->pict expand* #:contract? #t)))
     (WR (metafunction->pict prune #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
