#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt"  use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^ expander^)
 (only-in "../../../terms.rkt"
          App% Atom% Sym% Stx% List% Null% Pair% Hole%
          lst->list snoc id? prim?
          use-lst-form [#%term-forms tm:#%term-forms])
 (only-in "config.rkt" config^ [#%term-forms cfg:#%term-forms]))
(provide ==> expand/red@ expand@ expander@)

(define-syntax #%term-forms
  (append (syntax-local-value #'tm:#%term-forms)
          (syntax-local-value #'cfg:#%term-forms)))

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> --> :=<1>)
  #:within-signatures [(only config^
                             AstEnv% Stxξ% κ% ζ% Σ*% TVar% TStop% InEval%)
                       (only terms-extra^
                             val? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip union in-hole
                             alloc-scope prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name)
                       (only bind^
                             bind resolve id=?)
                       (only mcont^
                             push-κ)
                       (only parser^
                             parse)]

  #:do [(use-terms App Atom Sym Stx List Null Pair Hole
                   AstEnv Stxξ κ ζ Σ* TVar TStop InEval)
        (use-lst-form Lst List? Null Pair lst->list)
        
        ;; Constants:
        (define id-kont (Stx (Sym '#%kont) (empty-ctx)))
        (define id-seq  (Stx (Sym '#%seq)  (empty-ctx)))
        (define id-snoc (Stx (Sym '#%snoc) (empty-ctx)))
        (define stx-nil (Stx (Null)        (empty-ctx)))
        ;; This is the same as the single-phase one, but with `ph`
        ;; threaded through to `add` & `bind`
        ; regist-vars : Ph Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ)
        (define (regist-vars ph scp stl ξ Σ)
          (match stl
            [(Null) (values (Null) ξ Σ)]
            [(Pair (app (λ (stx) stx) id) stl)
             (let*-values ([(stl_reg ξ_1 Σ_1)
                            (regist-vars ph scp stl ξ Σ)]
                           [(nam_new Σ_2) (alloc-name id Σ_1)]
                           [(id_new) (add ph id scp)]
                           [(Σ_3) (bind #:phase ph Σ_2 id_new nam_new)]
                           [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
               (values (Pair id_new stl_reg) ξ_2 Σ_3))]))]

  ;; stops
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_stop)
                                    . stl_args)
                               ctx)) ξ) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam_stop :=<1> (resolve #:phase ph id_stop Σ)
   #:with      val :=<1> (lookup-ξ ξ nam_stop)
   #:when (TStop? val)
   (ζ stx '• κ Σ*_0)
   ex-stop]

  ;; lambda (same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_lam)
                                    (Stx (? proper-stl? stl_args) ctx_0)
                                    stx_body)
                               ctx))
              ξ) '∘ κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_lam 'lambda #:ξ ξ Σ)
   #:with                      scp_new := (alloc-scope 'lam)
   #:with (values stl_args2 ξ_new Σ_1) := (regist-vars ph scp_new
                                                         stl_args ξ Σ)
   #:with           (values 𝓁_new Σ_2) := (push-κ Σ_1 stx κ0)
   #:with                         Σ*_2 := (Σ* Σ_2
                                                (union (set scp_new) scps_p)
                                                (set))
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '∘
       (κ (Stx (Lst id_lam
                     (Stx stl_args2 ctx_0)
                     (Hole))
                ctx) '• Σ*_0 𝓁_new) Σ*_2)
   ex-lam-body]

  ;; let
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_let)
                                    (Stx (? proper-stl? stl_binds) ctx_1)
                                    stx_body)
                               ctx))
              ξ) '∘ κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_let 'let #:ξ ξ Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with                      scp_new := (alloc-scope 'let)
   #:with (values stl_vars2 ξ_new Σ_1) := (regist-vars ph scp_new
                                                         stl_vars ξ Σ)
   #:with           (values 𝓁_new Σ_2) := (push-κ Σ_1 stx κ0)
   #:with                         Σ*_2 := (Σ* Σ_2
                                                (union (set scp_new) scps_p)
                                                (set))
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '∘
       (κ (Stx (Lst id-kont
                     id_let
                     (Stxξ ph (Stx (Lst (Stx stl_vars2 ctx_1)
                                         (Stx stl_rhs ctx_1))
                                    ctx_1) ξ)
                     (Hole))
                ctx) '∘ Σ*_0 𝓁_new) Σ*_2)
   ex-let-body]
  
  [(ζ (and stx (Stx (Lst (? id? id_kont)
                          (? id? id_let)
                          (Stxξ ph (Stx
                                     (Lst (Stx (? proper-stl? stl_vars) _)
                                          (Stx (? proper-stl? stl_rhs) _))
                                     ctx_1)
                                 ξ)
                          stx_body)
                     ctx)) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (and (id=? #:phase ph id_kont '#%kont #:ξ ξ Σ)
               (id=? #:phase ph id_let  'let    #:ξ ξ Σ))
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_rhs)
                      ctx_1) ξ) '∘
       (κ (Stxξ ph (Stx (Lst id_kont
                               id_let
                               (Stx (Lst (Stx stl_vars ctx_1) (Hole)) ctx_1)
                               stx_body)
                          ctx)
                  ξ) '∘ Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-let-rhs]  

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont)
                           (? id? id_let)
                           (Stx (Lst (Stx (? proper-stl? stl_vars) _)
                                     (Stx (? proper-stl? val_rhs) _))
                                ctx_1)
                           stx_body)
                      ctx)
              ξ) '∘ κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? #:phase ph id_kont '#%kont #:ξ ξ Σ)
               (id=? #:phase ph id_let 'let     #:ξ ξ Σ))
   (ζ (Stx (Lst id_let (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                 stx_body)
            ctx) '• κ Σ*_0)
   ex-let-rhs2]

  ;; quote (same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_quote) _) _)) ξ) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_quote 'quote #:ξ ξ Σ)
   (ζ stx '• κ Σ*_0)
   ex-quote]

  ;; syntax (same as phases)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_syntax) stx) ctx) ξ) '∘
       κ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_syntax 'syntax #:ξ ξ Σ)
   #:with stx_pruned := (prune ph stx scps_p)
   (ζ (Stx (Lst id_syntax stx_pruned) ctx) '• κ Σ*_0)
   ex-stx]

  ;; macro creation (eval gets more and updates store)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_ls)
                           (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                           stx_body)
                      ctx) ξ) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_ls 'let-syntax #:ξ ξ Σ)
   ;(printf "start ls: ~a\n" id)
   (ζ (Stx (Lst id_ls
                 (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                 (Stxξ ph stx_body ξ))
            ctx) '∘ κ Σ*_0)
   ex-ξ-ls]

  [(ζ (and stx (Stx (Lst (? id? id_ls)
                          (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_0))
                               ctx_1)
                          (Stxξ ph stx_body ξ))
                     ctx)) '∘
       κ0 (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_ls 'let-syntax #:ξ ξ Σ)
   ;(printf "start2 ls: ~a\n" stx_body)
   #:with (values nam_new Σ_1) := (alloc-name id Σ)
   #:with              scp_new := (alloc-scope 'ls)
   #:with               id_new := (add ph id scp_new)
   #:with                  Σ_2 := (bind #:phase ph Σ_1 id_new nam_new)
   #:with   (values 𝓁_new Σ_3) := (push-κ Σ_2 stx κ0)
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ)) '∘
       (κ (Stx (Lst id-kont
                     id_ls
                     (Stx (Lst (Stx (Lst id_new (Hole)) ctx_0)) ctx_1)
                     (Stxξ ph stx_body ξ)
                     (Stx #f (list (cons ph (set scp_new)))))
                ctx)
           '∘ Σ*_0 𝓁_new)
       (Σ* Σ_3 (set) (set)))
   ex-ls-push-rhs]

  [(ζ (Stx (Lst (? id? id_kont)
                 (? id? id_ls)
                 (Stx (Lst (Stx (Lst (? id? id_new) stx_exp) ctx_0)) ctx_1)
                 (Stxξ ph stx_body ξ)
                 (Stx #f ctx_new))
            ctx) '∘ κ (Σ* Σ scps_p _))
   #:when (and (id=? #:phase ph id_kont '#%kont     #:ξ ξ Σ)
               (id=? #:phase ph id_ls   'let-syntax #:ξ ξ Σ))
   ;(printf "before resolve: ~a\n" (results(resolve #:phase ph id_new Σ)))
   #:with nam_new :=<1> (resolve #:phase ph id_new Σ)
   ;(printf "before parse: ~a\n" (results (parse #:phase (add1 ph) stx_exp Σ)))
   ;(printf "    stx_body: ~a\n" stx_body)
   #:with ast_exp :=<1> (parse #:phase (add1 ph) stx_exp Σ)
   (InEval (list (AstEnv ph ast_exp (init-env) 'no-scope ξ)
                 '• (init-store) (Σ* Σ scps_p (set)))
           (ζ (Stx (Lst (Stx (Sym nam_new) (empty-ctx))
                         (Stxξ ph stx_body ξ)
                         (Stx #f ctx_new))
                    (empty-ctx)) '∘
               κ (Σ* Σ scps_p (set))))
   ex-ls-eval]

  [(InEval (list (? val? val) '• store_0 (Σ* Σ _ _))
           (ζ (Stx (Lst (Stx (Sym nam_new) _)
                         (Stxξ ph stx_body ξ)
                         (Stx #f ctx_new))
                    _) '∘ κ (Σ* _ scps_p _)))
   ;(printf "after eval: ~a\n" val)
   #:with scp_new   := (car (set->list (at-phase ctx_new ph)))
   #:with ξ_new     := (extend-ξ ξ nam_new val)
   #:with stx_body2 := (add ph stx_body scp_new)
   ;(printf "    stx_body2: ~a\n" stx_body2)
   (ζ (Stxξ ph stx_body2 ξ_new) '∘
       κ (Σ* Σ (union (set scp_new) scps_p) (set)))
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (Stx (Lst (? id? id_mac) _ ...) ctx)) ξ) '∘
       κ (and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with    nam_mac :=<1> (resolve #:phase ph id_mac Σ)
   #:with        val :=<1> (lookup-ξ ξ nam_mac)
   #:when (val? val)
   #:with      scp_u :=    (alloc-scope 'u)
   #:with      scp_i :=    (alloc-scope 'i)
   #:with       Σ*_1 :=    (Σ* Σ
                                         (union (set scp_u) scps_p)
                                         (union (set scp_u) scps_u))
   #:with        stx_macapp2 :=    (flip ph (add ph stx_macapp scp_u) scp_i)
   (InEval
    (list (AstEnv ph (App (gensym 'mapp) ;; TODO: OK?
                          val (list stx_macapp2))
                  (init-env) scp_i ξ)
          '• (init-store) Σ*_1)
    (ζ (Stxξ ph (Stx #f (list (cons ph (set scp_i)))) ξ)
        '∘ κ Σ*_1)) ;; Σ*_1 not used
   ex-macapp-eval]

  [(InEval (list (? Stx? stx_exp) '• store_0 Σ*)
           (ζ (Stxξ ph (Stx #f ctx_i) ξ) '∘ κ _))
   #:with scp_i := (car (set->list (at-phase ctx_i ph)))
   (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ) '∘ κ Σ*)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_if) . stl_exps) ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_if 'if #:ξ ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_exps) ctx) ξ) '∘
       (κ (Stxξ ph (Stx (Lst id-kont id_if (Hole)) ctx) ξ)
           '∘ Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-if]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont)
                           (? id? id_if)
                           (Stx (? proper-stl? val_exps) ctx))
                      _)
              ξ) '∘ κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? #:phase ph id_kont '#%kont #:ξ ξ Σ)
               (id=? #:phase ph id_if   'if     #:ξ ξ Σ))
   (ζ (Stx (Lst id_if . val_exps) ctx) '• κ Σ*_0)
   ex-if-kont]

  ;; application (non-canonical #%app version, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_app)
                                    stx_fun . stl_args)
                               ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_app '#%app #:ξ ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-#%app]

  ;; application (canonical #%app version, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Pair (? id? id_app)
                                     (Stx (Lst stx_fun . stl_args)
                                          _))
                               ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_app '#%app #:ξ ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-#%app2]

  ;; application (bound var-ref, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id? stx_fun)
   #:with name :=<1> (resolve #:phase ph stx_fun Σ)
   #:with   at :=<1> (lookup-ξ ξ name)
   #:when (TVar? at)
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-app-bound-var]

  ;; application (free var-ref, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id? stx_fun)
   #:with name := (resolve #:phase ph stx_fun Σ)
   #:with   at := (lookup-ξ ξ name)
   #:when (and (eq? 'not-found at)
               (not (member name
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%ls-kont #%snoc))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-app-free-var]

  ;; application (primitive or lambda)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (not (id? stx_fun))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-app-prim-lambda]

  ;; reference (same as phases)
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam :=<1> (resolve #:phase ph id Σ)
   #:with  at :=    (lookup-ξ ξ nam)
   (match at
     [(TVar id_new) (ζ id_new '• κ Σ*_0)]
     [_ (error '==>f "unbound identifier: ~a" nam)])
   ex-var]

  ;; literal (same as phases)
  [(ζ (Stxξ ph (Stx (? Atom? atom) ctx) ξ) '∘ κ Σ*)
   #:when (not (id? (Stx atom ctx)))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx atom ctx)) ctx) '• κ Σ*)
   ex-lit]

  ;; primitive operator (same as phases)
  [(ζ (Stxξ ph (Stx (? prim? prim) ctx) ξ) '∘ κ Σ*)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx prim ctx)) ctx) '• κ Σ*)
   ex-prim-op]

  ;; pop κ (merge Σ*)
  [(ζ stx '• (κ stx_c ex? (Σ* _ scps_p scps_u) 𝓁) (Σ* Σ _ _))
   #;
   (let ([ks (results (lookup-Σ Σ 𝓁))])
     (for ([k (in-set ks)]
           #:when (not (eq? k '•)))
       (printf "pop κ: ~a\n" (κ-stx k))))
   #:with κ0 :=<1> (lookup-Σ Σ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 (Σ* Σ scps_p scps_u))
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_seq)
                                    (Stx (? proper-stl? val_dones) _)
                                    stx_exp0
                                    . stl_exps)
                               ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_seq '#%seq #:ξ ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph stx_exp0 ξ) '∘
       (κ (Stx (Lst (Stxξ ph id_seq ξ)
                     (Stx (Lst id-snoc (Stx val_dones (empty-ctx)) (Hole))
                          (empty-ctx))
                     . stl_exps)
                ctx) '∘ Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-seq-cons]

  [(ζ (Stx (Lst (Stxξ ph (? id? id_seq) ξ)
                 (Stx (Lst (? id? id_snoc)
                           (Stx (? proper-stl? val_dones) ctx_1)
                           (? Stx? stx_done))
                      _)
                 . stl_exps)
            ctx) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? #:phase ph id_seq  '#%seq  #:ξ ξ Σ)
               (id=? #:phase ph id_snoc '#%snoc #:ξ ξ Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ ph (Stx (Lst id_seq (Stx val_dones2 ctx_1)
                           . stl_exps)
                      ctx) ξ) '∘ κ Σ*_0)
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_seq)
                           (Stx (? proper-stl? val_dones) _))
                      ctx) ξ) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_seq '#%seq #:ξ ξ Σ)
   (ζ (Stx val_dones ctx) '• κ Σ*_0)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift ((-->) s1)) ;; extra call due to mut. rec. defs
   (InEval s2 ζ0)
   ex-in-eval])

(define-unit-from-reduction red@ ==>)

(define-mixed-unit expand/red@
  (import (only config^
                ζ% Stxξ%)
          (only eval^
                -->)
          (only red^
                reducer))
  (export expand^)
  (inherit)
  (use-terms ζ Stxξ)
  
  (define (==> delta) (λ () (reducer (--> delta) :=)))

  ; expand : Ph Stx ξ Σ* -> (Cons Stx Σ*)
  (define (expand delta ph stx ξ Σ*)
    (define ==>d (==> delta))
    (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• Σ*)])
      (match-let ([(set (ζ stx_new '• '• Σ*_new))
                   (apply-reduction-relation* (==>d) init-ζ)])
        (cons stx_new Σ*_new)))))

(define-compound-unit/infer expand@
  (import terms-extra^ config^ syntax^ env^ store^ eval^ menv^ mstore^ mcont^
          bind^ parser^)
  (export expand^)
  (link expand/red@ red@))

(define-unit expander@
  (import (only config^
                Σ*%)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only expand^
                expand))
  (export expander^)
  (use-terms Σ*)

  (define (expander delta stx)
    (expand delta 0 stx (init-ξ) (Σ* (init-Σ) (set) (set)))))
