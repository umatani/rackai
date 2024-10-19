#lang racket/base
(require
 racket/unit
 (only-in racket/match       match match-let)
 (only-in "../../set.rkt"    set ∅ set→list)
 (only-in "../../syntax.rkt" snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../../misc.rkt" union))
(provide ==> expand/red@ expand@)

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> --> :=<1>)
  #:within-signatures [(only domain^
                             val? stx? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip in-hole
                             prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name alloc-scope)
                       (only  bind^    bind resolve)
                       (only    id^    id=?)
                       (only mcont^    push-κ)
                       (only parse^    parse)]

  #:do [;; Constants:
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
                           [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                           [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
               (values (Pair id_new stl_reg) ξ_2 Σ_3))]))]

  ;; stops
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_stop)
                                    . stl_args)
                               ctx)) ξ) '◯
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam_stop :=<1> (resolve ph id_stop Σ)
   #:with      val :=<1> (lookup-ξ ξ nam_stop)
   #:when (TStop? val)
   (ζ stx '● κ Σ*_0)
   ex-stop]

  ;; lambda (same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_lam)
                                    (Stx (? proper-stl? stl_args) ctx_0)
                                    stx_body)
                               ctx))
              ξ) '◯ κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_lam 'lambda ξ Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars ph scp_new
                                                         stl_args ξ Σ_1)
   #:with           (values 𝓁_new Σ_3) := (push-κ Σ_2 stx κ0)
   #:with                         Σ*_3 := (Σ* Σ_3
                                              (union (set scp_new) scps_p)
                                              ∅)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '◯
       (κ (Stx (Lst id_lam
                     (Stx stl_args2 ctx_0)
                     (Hole))
                ctx) '● Σ*_0 𝓁_new) Σ*_3)
   ex-lam]

  ;; let
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_let)
                                    (Stx (? proper-stl? stl_binds) ctx_1)
                                    stx_body)
                               ctx))
              ξ) '◯ κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_let 'let ξ Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with         (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars ph scp_new
                                                         stl_vars ξ Σ_1)
   #:with           (values 𝓁_new Σ_3) := (push-κ Σ_2 stx κ0)
   #:with                         Σ*_3 := (Σ* Σ_3
                                              (union (set scp_new) scps_p)
                                              ∅)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '◯
       (κ (Stx (Lst id-kont
                     id_let
                     (Stxξ ph (Stx (Lst (Stx stl_vars2 ctx_1)
                                         (Stx stl_rhs ctx_1))
                                    ctx_1) ξ)
                     (Hole))
                ctx) '◯ Σ*_0 𝓁_new) Σ*_3)
   ex-let-body]
  
  [(ζ (and stx (Stx (Lst (? id? id_kont)
                          (? id? id_let)
                          (Stxξ ph (Stx
                                     (Lst (Stx (? proper-stl? stl_vars) _)
                                          (Stx (? proper-stl? stl_rhs) _))
                                     ctx_1)
                                 ξ)
                          stx_body)
                     ctx)) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (and (id=? ph id_kont '#%kont ξ Σ)
               (id=? ph id_let  'let    ξ Σ))
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_rhs)
                      ctx_1) ξ) '◯
       (κ (Stxξ ph (Stx (Lst id_kont
                               id_let
                               (Stx (Lst (Stx stl_vars ctx_1) (Hole)) ctx_1)
                               stx_body)
                          ctx)
                  ξ) '◯ Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-let-rhs]  

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont)
                           (? id? id_let)
                           (Stx (Lst (Stx (? proper-stl? stl_vars) _)
                                     (Stx (? proper-stl? val_rhs) _))
                                ctx_1)
                           stx_body)
                      ctx)
              ξ) '◯ κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? ph id_kont '#%kont ξ Σ)
               (id=? ph id_let 'let     ξ Σ))
   (ζ (Stx (Lst id_let (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                 stx_body)
            ctx) '● κ Σ*_0)
   ex-let]

  ;; quote (same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_quote) _) _)) ξ) '◯
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_quote 'quote ξ Σ)
   (ζ stx '● κ Σ*_0)
   ex-quote]

  ;; syntax (same as phases)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_syntax) stx) ctx) ξ) '◯
       κ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_syntax 'syntax ξ Σ)
   #:with stx_pruned := (prune ph stx scps_p)
   (ζ (Stx (Lst id_syntax stx_pruned) ctx) '● κ Σ*_0)
   ex-stx]

  ;; macro creation (eval gets more and updates store)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_ls)
                           (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                           stx_body)
                      ctx) ξ) '◯
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_ls 'let-syntax ξ Σ)
   ;(printf "start ls: ~a\n" id)
   (ζ (Stx (Lst id_ls
                 (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                 (Stxξ ph stx_body ξ))
            ctx) '◯ κ Σ*_0)
   ex-ls-ξ]

  [(ζ (and stx (Stx (Lst (? id? id_ls)
                          (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_0))
                               ctx_1)
                          (Stxξ ph stx_body ξ))
                     ctx)) '◯
       κ0 (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_ls 'let-syntax ξ Σ)
   ;(printf "start2 ls: ~a\n" stx_body)
   #:with (values nam_new Σ_1) := (alloc-name id Σ)
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add ph id scp_new)
   #:with                  Σ_3 := (bind ph Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Σ_4) := (push-κ Σ_3 stx κ0)
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ)) '◯
      (κ (Stx (Lst id-kont
                   id_ls
                   (Stx (Lst (Stx (Lst id_new (Hole)) ctx_0)) ctx_1)
                   (Stxξ ph stx_body ξ)
                   (add ph (Stx (Bool #f) (empty-ctx)) scp_new))
              ctx)
         '◯ Σ*_0 𝓁_new)
      (Σ* Σ_4 ∅ ∅))
   ex-ls-rhs]

  [(ζ (Stx (Lst (? id? id_kont)
                 (? id? id_ls)
                 (Stx (Lst (Stx (Lst (? id? id_new) stx_exp) ctx_0)) ctx_1)
                 (Stxξ ph stx_body ξ)
                 (Stx (Bool #f) ctx_new))
            ctx) '◯ κ (Σ* Σ scps_p _))
   #:when (and (id=? ph id_kont '#%kont     ξ Σ)
               (id=? ph id_ls   'let-syntax ξ Σ))
   ;(printf "before resolve: ~a\n" (results(resolve ph id_new Σ)))
   #:with nam_new :=<1> (resolve ph id_new Σ)
   ;(printf "before parse: ~a\n" (results (parse (add1 ph) stx_exp Σ)))
   ;(printf "    stx_body: ~a\n" stx_body)
   #:with ast_exp :=<1> (parse (add1 ph) stx_exp Σ)
   (InEval (list (AstEnv ph ast_exp (init-env) 'no-scope ξ)
                 '● (init-store) (Σ* Σ scps_p ∅))
           (ζ (Stx (Lst (Stx (Sym nam_new) (empty-ctx))
                        (Stxξ ph stx_body ξ)
                        (Stx (Bool #f) ctx_new))
                   (empty-ctx)) '◯
              κ (Σ* Σ scps_p ∅)))
   ex-ls-eval]

  [(InEval (list (? val? val) '● store_0 (Σ* Σ _ _))
           (ζ (Stx (Lst (Stx (Sym nam_new) _)
                         (Stxξ ph stx_body ξ)
                         (Stx (Bool #f) ctx_new))
                    _) '◯ κ (Σ* _ scps_p _)))
   ;(printf "after eval: ~a\n" val)
   #:with scp_new   := (car (set→list (at-phase ctx_new ph)))
   #:with ξ_new     := (extend-ξ ξ nam_new val)
   #:with stx_body2 := (add ph stx_body scp_new)
   ;(printf "    stx_body2: ~a\n" stx_body2)
   (ζ (Stxξ ph stx_body2 ξ_new) '◯
      κ (Σ* Σ (union (set scp_new) scps_p) ∅))
   ex-ls]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (Stx (Lst (? id? id_mac) _ ...) ctx)) ξ) '◯
       κ (and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with    nam_mac :=<1> (resolve ph id_mac Σ)
   #:with        val :=<1> (lookup-ξ ξ nam_mac)
   #:when (val? val)
   #:with (values scp_u Σ_1) := (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) := (alloc-scope 'i Σ_1)
   #:with               Σ*_1 := (Σ* Σ_2
                                    (union (set scp_u) scps_p)
                                    (union (set scp_u) scps_u))
   #:with        stx_macapp2 := (flip ph (add ph stx_macapp scp_u) scp_i)
   (InEval
    (list (AstEnv ph (App (gensym 'mapp) ;; TODO: OK?
                          val (list stx_macapp2))
                  (init-env) scp_i ξ)
          '● (init-store) Σ*_1)
    (ζ (Stxξ ph (add ph (Stx (Bool #f) (empty-ctx)) scp_i) ξ)
        '◯ κ Σ*_1)) ;; Σ*_1 not used
   ex-macapp-eval]

  [(InEval (list (? stx? stx_exp) '● store_0 Σ*)
           (ζ (Stxξ ph (Stx (Bool #f) ctx_i) ξ) '◯ κ _))
   #:with scp_i := (car (set→list (at-phase ctx_i ph)))
   (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ) '◯ κ Σ*)
   ex-macapp]

  ;; if
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_if) . stl_exps) ctx)) ξ) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_if 'if ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_exps) ctx) ξ) '◯
       (κ (Stxξ ph (Stx (Lst id-kont id_if (Hole)) ctx) ξ)
           '◯ Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-if-seq]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont)
                           (? id? id_if)
                           (Stx (? proper-stl? val_exps) ctx))
                      _)
              ξ) '◯ κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? ph id_kont '#%kont ξ Σ)
               (id=? ph id_if   'if     ξ Σ))
   (ζ (Stx (Lst id_if . val_exps) ctx) '● κ Σ*_0)
   ex-if]

  ;; application (canonical #%app version, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Pair (? id? id_app)
                                     (Stx (Lst stx_fun . stl_args)
                                          _))
                               ctx)) ξ) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_app '#%app ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-#%app]

  ;; application (non-canonical #%app version, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_app)
                                    stx_fun . stl_args)
                               ctx)) ξ) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_app '#%app ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-#%app′]

  ;; application (lambda or primitive)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (not (id? stx_fun))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-app]

  ;; application (bound var ref, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id? stx_fun)
   #:with name :=<1> (resolve ph stx_fun Σ)
   #:with   at :=<1> (lookup-ξ ξ name)
   #:when (TVar? at)
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-app-bound]

  ;; application (free var ref, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id? stx_fun)
   #:with name := (resolve ph stx_fun Σ)
   #:with   at := (lookup-ξ ξ name)
   #:when (and (eq? 'not-found at)
               (not (member name
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-app-free]

  ;; reference (same as phases)
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ) '◯
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam :=<1> (resolve ph id Σ)
   #:with  at :=    (lookup-ξ ξ nam)
   (match at
     [(TVar id_new) (ζ id_new '● κ Σ*_0)]
     [_ (error '==>f "unbound identifier: ~a" nam)])
   ex-var]

  ;; literal (same as phases)
  [(ζ (Stxξ ph (Stx (? Atom? atom) ctx) ξ) '◯ κ Σ*)
   #:when (not (id? (Stx atom ctx)))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx atom ctx)) ctx) '● κ Σ*)
   ex-lit]

  ;; primitive operator (same as phases)
  [(ζ (Stxξ ph (Stx (? prim? prim) ctx) ξ) '◯ κ Σ*)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx prim ctx)) ctx) '● κ Σ*)
   ex-prim]

  ;; pop κ (merge Σ*)
  [(ζ stx '● (κ stx_c ex? (Σ* _ scps_p scps_u) 𝓁) (Σ* Σ _ _))
   #;
   (let ([ks (results (lookup-Σ Σ 𝓁))])
     (for ([k (in-set ks)]
           #:when (not (eq? k '●)))
       (printf "pop κ: ~a\n" (κ-stx k))))
   #:with κ0 :=<1> (lookup-Σ Σ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 (Σ* Σ scps_p scps_u))
   ex-pop-κ]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift ((-->) s1)) ;; extra call due to mut. rec. defs
   (InEval s2 ζ0)
   ex-in-eval]

  ;;;; expression sequence

  ;; (#%seq (d ...) e₀ e ...) ==> (#%seq (d ... (expand e₀)) e ...)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_seq)
                                    (Stx val_dones _)
                                    stx_exp0
                                    . stl_exps)
                               ctx)) ξ) '◯
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_seq '#%seq ξ Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph stx_exp0 ξ) '◯
       (κ (Stx (Lst (Stxξ ph id_seq ξ)
                     (Stx (Lst id-snoc (Stx val_dones (empty-ctx)) (Hole))
                          (empty-ctx))
                     . stl_exps)
                ctx) '◯ Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p ∅))
   ex-seq-cons]

  [(ζ (Stx (Lst (Stxξ ph (? id? id_seq) ξ)
                 (Stx (Lst (? id? id_snoc)
                           (Stx val_dones ctx_1)
                           (? val? stx_done))
                      _)
                 . stl_exps)
            ctx) '◯
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? ph id_seq  '#%seq  ξ Σ)
               (id=? ph id_snoc '#%snoc ξ Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ ph (Stx (Lst id_seq (Stx val_dones2 ctx_1)
                           . stl_exps)
                      ctx) ξ) '◯ κ Σ*_0)
   ex-seq-snoc]

  ;; (#%seq (d ...)) ==> (d ...)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_seq)
                           (Stx val_dones _))
                      ctx) ξ) '◯
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_seq '#%seq ξ Σ)
   (ζ (Stx val_dones ctx) '● κ Σ*_0)
   ex-seq-nil])


(define-unit-from-reduction red@ ==>)

(define-unit expand/red@
  (import (only eval^    -->)
          (only  red^    reducer))
  (export expand^)
  
  ;; δ → → ζ → (Setof ζ)
  (define (==> δ) (λ () (reducer (--> δ) :=)))

  ;; expand : δ Ph Stx ξ Σ* → (Cons Stx Σ*)
  (define (expand δ ph stx ξ Σ*)
    (define ==>δ (==> δ))
    (define ζᵢ (ζ (Stxξ ph stx ξ) '◯ '● Σ*))

    (match-let ([(set (ζ stx′ '● '● Σ*′)) (apply-reduction* (==>δ) ζᵢ)])
      (cons stx′ Σ*′))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ bind^ id^ parse^)
  (export expand^)
  (link expand/red@ red@))

