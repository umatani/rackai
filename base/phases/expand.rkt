#lang racket/base
(require
 racket/unit
 (only-in racket/match     match match-let)
 (only-in "../../set.rkt"  set ∅ set→list)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../../misc.rkt" union))
(provide ==> red@ expand/red@ expand@)

;; ----------------------------------------
;; The expander:

;; ==> :  ζ -> (Setof ζ)
(define-reduction (==> :=<1> -->)
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

        ;; regist-vars : Ph Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)
        ;;   This is the same as the single-phase one, but with `ph`
        ;;   threaded through to `add` & `bind`
        (define (regist-vars ph scp stl ξ Σ)
          (match stl
            [(Null) (values (Null) ξ Σ)]
            [(Pair id stl)
             (let*-values ([(stl_reg ξ_1 Σ_1)
                            (regist-vars ph scp stl ξ Σ)]
                           [(nam_new Σ_2) (alloc-name id Σ_1)]
                           [(id_new) (add ph id scp)]
                           [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                           [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
               (values (Pair id_new stl_reg) ξ_2 Σ_3))]))]

  ;; lambda
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_lam)
                                    (Stx (? proper-stl? stl_args) ctx_0)
                                    stx_body) ctx))
              ξ scps_p) '◯ κ0 Σ)
   #:when (id=? ph id_lam 'lambda Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars ph scp_new stl_args ξ Σ_1)
   #:with           (values 𝓁_new Σ_3) := (push-κ Σ_2 stx κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
       '◯
       (κ (Stx (Lst id_lam (Stx stl_args2 ctx_0)
                     (Hole)) ctx)
           '● 𝓁_new) Σ_3)
   ex-lam]

  ;; let
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_let)
                                 (Stx (? proper-stl? stl_binds) ctx_1)
                                 stx_body)
                               ctx)) ξ scps_p) '◯ κ0 Σ)
   #:when (id=? ph id_let 'let Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with         (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars ph scp_new
                                                         stl_vars ξ Σ_1)
   #:with           (values 𝓁_new Σ_3) := (push-κ Σ_2 stx κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
       '◯
       (κ (Stx (Lst id-kont
                     id_let
                     (Stxξ ph (Stx (Lst (Stx stl_vars2 ctx_1)
                                         (Stx stl_rhs ctx_1))
                                    ctx_1) ξ scps_p)
                     (Hole))
                ctx) '◯ 𝓁_new) Σ_3)
   ex-let-body]

  [(ζ (and stx (Stx (Lst (? id? id_kont)
                          (? id? id_let)
                          (Stxξ ph (Stx
                                     (Lst (Stx (? proper-stl? stl_vars) _)
                                          (Stx (? proper-stl? stl_rhs) _))
                                     ctx_1)
                                 ξ scps_p)
                          stx_body)
                     ctx)) '◯ κ0 Σ)
   #:when (and (id=? ph id_kont '#%kont Σ)
               (id=? ph id_let  'let    Σ))
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_rhs) ctx_1) ξ scps_p)
       '◯
       (κ
        (Stxξ ph (Stx (Lst id_kont
                            id_let
                            (Stx (Lst (Stx stl_vars ctx_1) (Hole))
                                 ctx_1)
                            stx_body)
                       ctx)
               ξ scps_p) '◯ 𝓁_new) Σ_1)
   ex-let-rhs]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont)
                           (? id? id_let)
                           (Stx (Lst (Stx (? proper-stl? stl_vars) _)
                                     (Stx (? proper-stl? val_rhs) _)) ctx_1)
                           stx_body)
                      ctx) ξ scps_p) '◯ κ0 Σ)
   #:when (and (id=? ph id_kont '#%kont Σ)
               (id=? ph id_let  'let    Σ))
   (ζ (Stx (Lst id_let (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                 stx_body)
            ctx) '● κ0 Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_quote) _) _)) _ _) '◯ κ0 Σ)
   #:when (id=? ph id_quote 'quote Σ)
   (ζ stx '● κ0 Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (Stx (Lst (? id? id_syntax) stx) ctx) _ scps_p) '◯ κ0 Σ)
   #:when (id=? ph id_syntax 'syntax Σ)
   #:with stx_pruned := (prune ph stx scps_p)
   (ζ (Stx (Lst id_syntax stx_pruned) ctx) '● κ0 Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ ph (Stx (Lst (? id? id_ls)
                           (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                           stx_body)
                      ctx) ξ scps_p) '◯ κ0 Σ)
   #:when (id=? ph id_ls 'let-syntax Σ)
   (ζ (Stx (Lst id_ls
                 (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                 (Stxξ ph stx_body ξ scps_p))
            ctx) '◯ κ0 Σ)
   ex-ξ-ls]

  [(ζ (and stx (Stx (Lst (? id? id_ls)
                          (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_0)) ctx_1)
                          (Stxξ ph stx_body ξ scps_p)) ctx)) '◯ κ0 Σ)
   #:when (id=? ph id_ls 'let-syntax Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ) 
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add ph id scp_new)
   #:with                  Σ_3 := (bind ph Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Σ_4) := (push-κ Σ_3 stx κ0)
   #:with            stx_body2 := (add ph stx_body scp_new)
   #:with              scps_p2 := (union (set scp_new) scps_p)
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ) ∅)
       '◯
       (κ (Stx (Lst id-kont
                     id_ls
                     (Stx (Lst (Stx (Lst id_new (Hole)) ctx_0)) ctx_1)
                     (Stxξ ph stx_body2 ξ scps_p2))
                ctx) '◯ 𝓁_new)
       Σ_4)
   ex-ls-push-rhs]

  [(ζ (Stx (Lst (? id? id_kont)
                 (? id? id_ls)
                 (Stx (Lst (Stx (Lst (? id? id_new) stx_exp) ctx_0)) ctx_1)
                 (Stxξ ph stx_body2 ξ scps_p2))
            ctx) '◯ κ0 Σ)
   #:when (and (id=? ph id_kont '#%kont     Σ)
               (id=? ph id_ls   'let-syntax Σ))
   #:with nam_new :=<1> (resolve ph id_new Σ)
   #:with ast_exp :=<1> (parse (add1 ph) stx_exp Σ)
   (InEval (list (AstEnv ast_exp (init-env)) '● (init-store))
           (ζ (Stx (Lst (Stx (Sym nam_new) (empty-ctx))
                         (Stxξ ph stx_body2 ξ scps_p2))
                    (empty-ctx))
               '◯ κ0 Σ))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _)
           (ζ (Stx (Lst (Stx (Sym nam_new) _)
                         (Stxξ ph stx_body2 ξ scps_p2))
                    _) '◯ κ0 Σ))
   #:with ξ_new := (extend-ξ ξ nam_new val)
   (ζ (Stxξ ph stx_body2 ξ_new scps_p2) '◯ κ0 Σ)
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (Stx (Lst (? id? id_mac) _ ...)
                                      ctx)) ξ scps_p)
       '◯ κ0 Σ)
   #:with    nam_mac :=<1> (resolve ph id_mac Σ)
   #:with        val :=<1> (lookup-ξ ξ nam_mac)
   #:when (val? val)
   #:with (values scp_u Σ_1) := (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) := (alloc-scope 'i Σ_1)
   (InEval
    (list (AstEnv (App (gensym 'mapp) ;; TODO: OK?
                       val (list (flip ph (add ph stx_macapp scp_u) scp_i)))
                  (init-env))
          '● (init-store))
    (ζ (Stxξ ph (add ph (Stx (Bool #f) (empty-ctx)) scp_i)
               ξ (union (set scp_u) scps_p)) '◯ κ0 Σ_2))
   ex-macapp-eval]

  [(InEval (list (? stx? stx_exp) '● store_0)
           (ζ (Stxξ ph (Stx (Bool #f) ctx_i) ξ scps_p) '◯ κ0 Σ))
   #:with scp_i := (car (set→list (at-phase ctx_i ph)))
   (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ scps_p) '◯ κ0 Σ)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_if) . stl_exps) ctx))
              ξ scps_p) '◯ κ0 Σ)
   #:when (id=? ph id_if 'if Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_exps) ctx) ξ scps_p)
       '◯
       (κ (Stxξ ph (Stx (Lst id-kont id_if (Hole))
                          ctx) ξ scps_p)
           '◯ 𝓁_new) Σ_1)
   ex-if]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont)
                           (? id? id_if)
                           (Stx (? proper-stl? val_exps) ctx))
                      _)
              ξ scps_p) '◯ κ0 Σ)
   #:when (and (id=? ph id_kont '#%kont Σ)
               (id=? ph id_if   'if     Σ))
   (ζ (Stx (Lst id_if . val_exps) ctx) '● κ0 Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_app)
                                    stx_fun . stl_args) ctx))
              ξ scps_p) '◯ κ0 Σ)
   #:when (id=? ph id_app '#%app Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args)
                      ctx) ξ scps_p)
       '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new) Σ_1)
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (and stx (Stx
                           (Pair (? id? id_app)
                                 (Stx (Lst stx_fun . stl_args)
                                      _))
                           ctx))
              ξ scps_p)
       '◯ κ0 Σ)
   #:when (id=? ph id_app '#%app Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args)
                      ctx) ξ scps_p)
       '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new)
       Σ_1)
   ex-#%app2]

  ;; application (bound var-ref)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args)
                               ctx)) ξ scps_p) '◯ κ0 Σ)
   #:when (id? stx_fun)
   #:with name :=<1> (resolve ph stx_fun Σ)
   #:with   at :=<1> (lookup-ξ ξ name)
   #:when (TVar? at)
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args)
                      ctx) ξ scps_p) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new) Σ_1)
   ex-app-bound-var]

  ;; application (free var-ref)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args)
                               ctx)) ξ scps_p) '◯ κ0 Σ)
   #:when (id? stx_fun)
   #:with name := (resolve ph stx_fun Σ)
   #:with   at := (lookup-ξ ξ name)
   #:when (and (eq? 'not-found at)
               (not (member name
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%ls-kont #%snoc))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args)
                      ctx) ξ scps_p) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new) Σ_1)
   ex-app-free-var]

  ;; application (primitive or lambda)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args)
                               ctx)) ξ scps_p) '◯ κ0 Σ)
   #:when (not (id? stx_fun))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args)
                      ctx) ξ scps_p) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new) Σ_1)
   ex-app-prim-lambda]

  ;; reference
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ scps_p) '◯ κ0 Σ)
   #:with           nam := (resolve ph id Σ)
   #:with all-transform := (lookup-ξ ξ nam)
   (match all-transform
     [(TVar id_new) (ζ id_new '● κ0 Σ)]
     [_ (error '==>p "unbound identifier: ~a" nam)])
   ex-var]

  ;; literal
  [(ζ (Stxξ ph (Stx (? Atom? atom) ctx) ξ scps_p) '◯ κ0 Σ)
   #:when (not (id? (Stx atom ctx)))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx atom ctx))
            ctx) '● κ0 Σ)
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ ph (Stx (? prim? prim) ctx) ξ scps_p) '◯ κ0 Σ)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx prim ctx))
            ctx) '● κ0 Σ)
   ex-prim-op]

  ;; pop κ
  [(ζ stx '● (κ stx_c ex? 𝓁) Σ)
   #:with κ0 :=<1> (lookup-Σ Σ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 Σ)
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (and stx (Stx (Lst (? id? id_seq)
                                    (Stx val_dones _)
                                    stx_exp0
                                    . stl_exps)
                               ctx))
              ξ scps_p) '◯ κ0 Σ)
   #:when (id=? ph id_seq '#%seq Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph stx_exp0 ξ scps_p) '◯
       (κ (Stx
            (Lst (Stxξ ph id_seq ξ scps_p)
                 (Stx (Lst id-snoc (Stx val_dones (empty-ctx)) (Hole))
                      (empty-ctx))
                 . stl_exps)
            ctx) '◯ 𝓁_new) Σ_1)
   ex-seq-cons]

  [(ζ (Stx (Lst (Stxξ ph (? id? id_seq) ξ scps_p)
                 (Stx (Lst (? id? id_snoc)
                           (Stx val_dones ctx_1)
                           (? val? stx_done)) _)
                 . stl_exps)
            ctx) '◯ κ0 Σ)
   #:when (and (id=? ph id_seq  '#%seq  Σ)
               (id=? ph id_snoc '#%snoc Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ ph (Stx (Lst id_seq (Stx val_dones2 ctx_1)
                           . stl_exps)
                      ctx) ξ scps_p) '◯ κ0 Σ)
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_seq)
                           (Stx val_dones _))
                      ctx) ξ scps_p)
       '◯ κ0 Σ)
   #:when (id=? ph id_seq '#%seq Σ)
   (ζ (Stx val_dones ctx) '● κ0 Σ)
   ex-seq-nil]

  ;; in eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift (--> s1))
   (InEval s2 ζ0)
   ex-in-eval])

(define-unit-from-reduction red@ ==>)

(define-unit expand/red@
  (import (only eval^    -->)
          (only  red^    reducer))
  (export expand^)

  ;; δ → ζ → (Setof ζ)
  (define (==> δ) (reducer := (--> δ)))

  ;; expand : δ Ph Stx ξ Scps Σ → (Cons Stx Σ)
  (define (expand δ ph stx ξ scpsₚ Σ)
    (define ==>δ (==> δ))
    (define ζᵢ (ζ (Stxξ ph stx ξ scpsₚ) '◯ '● Σ))

    (match-let ([(set (ζ stx′ '● '● Σ′)) (apply-reduction* ==>δ ζᵢ)])
      (cons stx′ Σ′))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ bind^ id^ parse^)
  (export expand^)
  (link expand/red@ red@))

