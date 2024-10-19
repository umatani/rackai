#lang racket/base
(require
 (only-in racket/match match match-let)
 racket/unit
 "../../reduction.rkt"
 (only-in "../../set.rkt" set set→list)
 "../../signatures.rkt"
 "terms.rkt")
(provide ==> red@ expand/red@ expand@)

;; ----------------------------------------
;; The expander:
;;   ζ ∷= ⟨⟨Stx, ξ⟩, ex?, κ, Σ⟩

;; ==> : ζ → (Setof ζ)
(define-reduction (==> --> :=<1>)
  #:within-signatures [(only domain^    val? stx? proper-stl?)
                       (only syntax^    empty-ctx zip unzip in-hole add flip)
                       (only    env^    init-env)
                       (only  store^    init-store)
                       (only   menv^    init-ξ lookup-ξ extend-ξ)
                       (only mstore^    lookup-Σ alloc-name alloc-scope)
                       (only   bind^    bind resolve)
                       (only     id^    id=?)
                       (only  mcont^    push-κ)
                       (only  parse^    parse)]
  #:do [;; Constants:
        (define id-kont (Stx (Sym '#%kont) (empty-ctx)))
        (define id-seq  (Stx (Sym '#%seq)  (empty-ctx)))
        (define id-snoc (Stx (Sym '#%snoc) (empty-ctx)))
        (define stx-nil (Stx (Null)        (empty-ctx)))
        
        ; regist-vars : Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)
        (define (regist-vars scp stl ξ Σ)
          (match stl
            [(Null) (values (Null) ξ Σ)]
            [(Pair (app (λ (stx) stx) id) stl)
             (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars scp stl ξ Σ)]
                           [(nam_new Σ_2) (alloc-name id Σ_1)]
                           [(id_new) (add id scp)]
                           [(Σ_3) (bind Σ_2 id_new nam_new)]
                           [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
               (values (Pair id_new stl_reg) ξ_2 Σ_3))]))]

  ;; lambda
  [(ζ (Stxξ (and (Stx (Lst (? id? id_lam)
                           (Stx (? proper-stl? stl_args) ctx_args)
                           stx_body)
                      ctx) stx)
            ξ) '◯ κ₀ Σ₀)
   #:when (id=? id_lam 'lambda Σ₀)
   #:with             (values scp Σ₁) := (alloc-scope 'lam Σ₀)
   #:with    (values stl_args′ ξ′ Σ₂) := (regist-vars scp stl_args ξ Σ₁)
   #:with               (values 𝓁 Σ₃) := (push-κ Σ₂ stx κ₀)
   (ζ (Stxξ (add stx_body scp) ξ′)
      '◯ (κ (Stx (Lst id_lam (Stx stl_args′ ctx_args)
                      (Hole)) ctx) '● 𝓁) Σ₃)
   ex-lam]

  ;; let
  [(ζ (Stxξ (and stx (Stx (Lst (? id? id_let)
                                 (Stx (? proper-stl? stl_binds) ctx_1)
                                 stx_body)
                            ctx)) ξ) '◯ κ0 Σ)
   #:when (id=? id_let 'let Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with        (values scp_new Σ_1)  := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars scp_new stl_vars ξ Σ_1)
   #:with           (values 𝓁_new Σ_3) := (push-κ Σ_2 stx κ0)
   (ζ (Stxξ (add stx_body scp_new) ξ_new)
       '◯
       (κ (Stx (Lst id-kont
                     id_let
                     (Stxξ (Stx (Lst (Stx stl_vars2 ctx_1)
                                      (Stx stl_rhs ctx_1))
                                 ctx_1) ξ)
                     (Hole))
                ctx) '◯ 𝓁_new)
       Σ_3)
   ex-let-body]

  [(ζ (and stx (Stx (Lst (? id? id_kont)
                          (? id? id_let)
                          (Stxξ (Stx
                                  (Lst (Stx (? proper-stl? stl_vars) _)
                                       (Stx (? proper-stl? stl_rhs) _))
                                  ctx_1) ξ)
                          stx_body)
                     ctx)) '◯ κ0 Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_let 'let Σ))
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil . stl_rhs) ctx_1) ξ)
       '◯
       (κ (Stx (Lst id_kont
                     id_let
                     (Stx (Lst (Stx stl_vars ctx_1) (Hole))
                          ctx_1)
                     stx_body)
                ctx) '◯ 𝓁_new)
       Σ_1)
   ex-let-rhs]

  [(ζ (Stx (Lst (? id? id_kont)
                 (? id? id_let)
                 (Stx (Lst (Stx (? proper-stl? stl_vars) _)
                           (Stx (? proper-stl? val_rhs) _))
                      ctx_1)
                 stx_body)
            ctx) '◯ κ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_let 'let Σ))
   (ζ (Stx (Lst id_let (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                 stx_body)
            ctx) '● κ Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ (and stx (Stx (Lst (? id? id_quote) _) _)) _) '◯ κ Σ)
   #:when (id=? id_quote 'quote Σ)
   (ζ stx '● κ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ (and stx (Stx (Lst (? id? id_syntax) _) _)) _) '◯ κ Σ)
   #:when (id=? id_syntax 'syntax Σ)
   (ζ stx '● κ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ (Stx (Lst (? id? id_ls)
                        (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                        stx_body)
                   ctx) ξ) '◯ κ Σ)
   #:when (id=? id_ls 'let-syntax Σ)
   (ζ (Stx (Lst id_ls
                 (Stx (Lst (Stx (Lst id stx_rhs) ctx_0)) ctx_1)
                 (Stxξ stx_body ξ))
            ctx) '◯ κ Σ)
   ex-ξ-ls]

  [(ζ (and stx (Stx (Lst (? id? id_ls)
                          (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_0))
                               ctx_1)
                          (Stxξ stx_body ξ))
                     ctx)) '◯ κ0 Σ)
   #:when (id=? id_ls 'let-syntax Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ)
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add id scp_new)
   #:with                  Σ_3 := (bind Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Σ_4) := (push-κ Σ_3 stx κ0)
   #:with            stx_body2 := (add stx_body scp_new)
   (ζ (Stxξ stx_rhs (init-ξ))
       '◯
       (κ (Stx (Lst id-kont
                     id_ls
                     (Stx (Lst (Stx (Lst id_new (Hole)) ctx_0)) ctx_1)
                     (Stxξ stx_body2 ξ))
                ctx) '◯ 𝓁_new) Σ_4)
   ex-ls-push-rhs]

  [(ζ (Stx (Lst (? id? id_kont)
                 (? id? id_ls)
                 (Stx (Lst (Stx (Lst (? id? id_new) stx_exp) ctx_0)) ctx_1)
                 (Stxξ stx_body2 ξ))
            ctx) '◯ κ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_ls 'let-syntax Σ))
   #:with nam_new :=<1> (resolve id_new Σ)
   #:with ast_exp :=<1> (parse stx_exp Σ)
   (InEval (list (AstEnv ast_exp (init-env)) '● (init-store))
           (ζ (Stx (Lst (Stx (Sym nam_new) (empty-ctx))
                         (Stxξ stx_body2 ξ))
                    (empty-ctx))
               '◯ κ Σ))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _)
           (ζ (Stx (Lst (Stx (Sym nam_new) _)
                         (Stxξ stx_body2 ξ))
                    _) '◯ κ Σ))
   #:with ξ_new := (extend-ξ ξ nam_new val)
   (ζ (Stxξ stx_body2 ξ_new) '◯ κ Σ)
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ (and stx_macapp (Stx (Lst (? id? id_mac) _ ...) ctx)) ξ)
       '◯ κ Σ)
   #:with    nam_mac :=<1> (resolve id_mac Σ)
   #:with        val :=<1> (lookup-ξ ξ nam_mac)
   #:when (val? val)
   #:with (values scp_u Σ_1) := (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) := (alloc-scope 'i Σ_1)
   (InEval
    (list (AstEnv (App (gensym 'mapp)  ;; TODO: OK?
                       val
                       (list (flip (add stx_macapp scp_u) scp_i))) (init-env))
          '● (init-store))
    (ζ (Stxξ (Stx #f (set scp_i)) ξ) '◯ κ Σ_2))
   ex-macapp-eval]

  [(InEval (list (? stx? stx_exp) '● store_0)
           (ζ (Stxξ (Stx #f scps) ξ) '◯ κ Σ))
   #:with scp_i := (car (set→list scps))
   (ζ (Stxξ (flip stx_exp scp_i) ξ) '◯ κ Σ)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ (and stx (Stx (Lst (? id? id_if) . stl_exps) ctx)) ξ) '◯ κ0 Σ)
   #:when (id=? id_if 'if Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil . stl_exps) ctx) ξ)
       '◯
       (κ (Stx (Lst id-kont id_if (Hole)) ctx) '◯ 𝓁_new)
       Σ_1)
   ex-if]

  [(ζ (Stx (Lst (? id? id_kont)
                 (? id? id_if)
                 (Stx (? proper-stl? val_exps) ctx))
            _) '◯ κ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_if 'if Σ))
   (ζ (Stx (Lst id_if . val_exps) ctx) '● κ Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ (and stx (Stx (Lst (? id? id_app)
                                 stx_fun . stl_args)
                            ctx)) ξ) '◯ κ0 Σ)
   #:when (id=? id_app '#%app Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ)
       '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new)
       Σ_1)
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ (Stx (and stx (Pair (? id? id_app)
                                  (Stx (Lst stx_fun . stl_args) _))) ctx) ξ)
       '◯ κ0 Σ)
   #:when (id=? id_app '#%app Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ)
       '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new)
       Σ_1)
   ex-#%app2]

  ;; application (bound var-ref)
  [(ζ (Stxξ (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '◯ κ0 Σ)
   #:when (id? stx_fun)
   #:with name :=<1> (resolve stx_fun Σ)
   #:with   at :=<1> (lookup-ξ ξ name)
   #:when (TVar? at)
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new)
       Σ_1)
   ex-app-bound-var]

  ;; application (free var-ref)
  [(ζ (Stxξ (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '◯ κ0 Σ)
   #:when (id? stx_fun)
   #:with name := (resolve stx_fun Σ)
   #:with   at := (lookup-ξ ξ name)
   #:when (and (eq? 'not-found at)
               (not (member name
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%ls-kont #%snoc))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new)
       Σ_1)
   ex-app-free-var]

  ;; application (primitive or lambda)
  [(ζ (Stxξ (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '◯ κ0 Σ)
   #:when (not (id? stx_fun))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new)
       Σ_1)
   ex-app-prim-lambda]

  ;; reference
  [(ζ (Stxξ (and id (Stx (Sym nam) ctx)) ξ) '◯ κ Σ)
   #:with           nam := (resolve id Σ)
   #:with all-transform := (lookup-ξ ξ nam)
    (match all-transform
     [(TVar id_new) (ζ id_new '● κ Σ)]
     [_ (error '==> "unbound identifier: ~a" nam)])
   ex-var]
  
  ;; literal
  [(ζ (Stxξ (Stx (? Atom? atom) ctx) ξ) '◯ κ Σ)
   #:when (not (id? (Stx atom ctx)))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx atom ctx)) ctx) '● κ Σ)
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ (Stx (? prim? prim) ctx) ξ) '◯ κ Σ)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) (Stx prim ctx)) ctx) '● κ Σ)
   ex-prim-op]

  ;; pop κ
  [(ζ stx '● (κ stx_c ex? 𝓁) Σ)
   #:with κ0 :=<1> (lookup-Σ Σ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 Σ)
   ex-pop-κ]

  ;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ (and stx (Stx (Lst (? id? id_seq)
                                 (Stx val_dones _)
                                 stx_exp0 . stl_exps) ctx)) ξ) '◯ κ0 Σ)
   #:when (id=? id_seq '#%seq Σ)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ stx_exp0 ξ) '◯
       (κ
        (Stx
         (Lst (Stxξ id_seq ξ)
              (Stx (Lst id-snoc (Stx val_dones (empty-ctx)) (Hole))
                   (empty-ctx))
              . stl_exps)
         ctx) '◯ 𝓁_new)
       Σ_1)
   ex-seq-cons]

  [(ζ (Stx (Lst (Stxξ (? id? id_seq) ξ)
                 (Stx (Lst (? id? id_snoc)
                           (Stx val_dones ctx_1)
                           (? val? stx_done)) _)
                 . stl_exps)
            ctx) '◯ κ Σ)
   #:when (and (id=? id_seq '#%seq Σ) (id=? id_snoc '#%snoc Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ (Stx (Lst id_seq (Stx val_dones2 ctx_1)
                        . stl_exps) ctx) ξ) '◯ κ Σ)
   ex-seq-snoc]
  
  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ (Stx (Lst (? id? id_seq)
                        (Stx val_dones _)) ctx) ξ) '◯ κ Σ)
   #:when (id=? id_seq '#%seq Σ)
   (ζ (Stx val_dones ctx) '● κ Σ)
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
  (define (==> δ) (reducer (--> δ) :=))

  ;; expand : δ Stx ξ Σ → (Cons Stx Σ)
  (define (expand δ stx ξ Σ)
    (define ==>δ (==> δ))
    (define ζᵢ (ζ (Stxξ stx ξ) '◯ '● Σ))

    (match-let ([(set (ζ stx′ '● '● Σ′)) (apply-reduction* ==>δ ζᵢ)])
      (cons stx′ Σ′))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ bind^ id^ parse^)
  (export expand^)
  (link   expand/red@ red@))
