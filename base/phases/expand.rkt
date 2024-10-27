#lang racket/base
(require
 racket/unit
 (only-in racket/match       match match-let)
 (only-in "../../set.rkt"    set ∅ set-add set→list)
 (only-in "../../mix.rkt"    define-mixed-unit inherit)
 (only-in "../../syntax.rkt" snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt")
(provide ==> expand@)

;; ----------------------------------------
;; The expander:
;;   ζ ∷= ⟨⟨Ph, Stx, ξ, Scps⟩ ∪ Stx, κ, Σ⟩
;;   κ ∷= ⟨⟨Ph, Stx, ξ, Scps⟩ ∪ Stx, 𝓁⟩

;; ==> :  ζ -> (Setof ζ)
(define-reduction (==> --> :=<1>)
  #:import [(only domain^    val? stx? proper-stl?)
            (only syntax^    empty-ctx zip unzip add flip in-hole prune at-phase)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only     id^    id=?)
            (only  mcont^    push-κ)
            (only  parse^    parse)]

  #:do [;; Constants
        (define id-kont (Stx (Sym '#%kont) (empty-ctx)))
        (define id-seq  (Stx (Sym '#%seq)  (empty-ctx)))
        (define id-snoc (Stx (Sym '#%snoc) (empty-ctx)))
        (define stx-nil (Stx (Null)        (empty-ctx)))

        ;; lookup-κ : Σ 𝓁 → κ
        (define (lookup-κ Σ 𝓁)
          (lookup-Σ Σ 𝓁))

        ;; regist-vars : Ph Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)
        ;;   This is the same as the single-phase one, but with `ph`
        ;;   threaded through to `add` & `bind`
        (define (regist-vars ph scp ids₀ ξ₀ Σ₀)
          (match ids₀
            [(Null)
             (values (Null) ξ₀ Σ₀)]
            [(Pair id ids)
             (let*-values ([(ids′ ξ₁ Σ₁) (regist-vars ph scp ids ξ₀ Σ₀)]
                           [(nam Σ₂)     (alloc-name id Σ₁)]
                           [(id′)        (add ph id scp)]
                           [(Σ₃)         (bind ph Σ₂ id′ nam)]
                           [(ξ₂)         (extend-ξ ξ₁ nam (TVar id′))])
               (values (Pair id′ ids′) ξ₂ Σ₃))]))]

  ;; lambda
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_lam)
                              (Stx (? proper-stl? stl_params) ctx_params)
                              stx_body) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id=? ph id_lam 'lambda Σ₀)
   (:= (values scp Σ₁)            (alloc-scope 'lam Σ₀))
   (:= (values stl_params′ ξ′ Σ₂) (regist-vars ph scp stl_params ξ Σ₁))
   (:= (values 𝓁 Σ₃)              (push-κ Σ₂ stx κ₀))
   (ζ (Stxξ ph (add ph stx_body scp) ξ′ (set-add scpsₚ scp))
      (κ (Stx (Lst id_lam (Stx stl_params′ ctx_params)
                   (Hole)) ctx) 𝓁) Σ₃)
   ex-lam]

  ;; let
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_let)
                              (Stx (? proper-stl? stl_binds) ctx_binds)
                              stx_body) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id=? ph id_let 'let Σ₀)
   (:= (values stl_vars stl_rhs) (unzip stl_binds))
   (:= (values scp Σ₁)           (alloc-scope 'let Σ₀))
   (:= (values stl_vars′ ξ′ Σ₂)  (regist-vars ph scp stl_vars ξ Σ₁))
   (:= (values 𝓁 Σ₃)             (push-κ Σ₂ stx κ₀))
   (ζ (Stxξ ph (add ph stx_body scp) ξ′ (set-add scpsₚ scp))
      (κ (Stxξ ph (Stx (Lst id-kont id_let
                            (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                      (Stx stl_rhs   (empty-ctx)))
                                 ctx_binds)
                            (Hole)) ctx) ξ scpsₚ) 𝓁) Σ₃)
   ex-let-body]

  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_kont) (? id? id_let)
                              (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                        (Stx (? proper-stl? stl_rhs  ) _))
                                   ctx_binds)
                              stx_body′) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (and (id=? ph id_kont '#%kont Σ₀)
               (id=? ph id_let  'let    Σ₀))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_rhs) ctx_binds) ξ scpsₚ)
      (κ (Stxξ ph (Stx (Lst id_kont id_kont id_let
                            (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                      (Hole))
                                 ctx_binds)
                            stx_body′) ctx) ξ scpsₚ) 𝓁) Σ₁)
   ex-let-rhs]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_kont′) (? id? id_let)
                         (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                   (Stx (? proper-stl? stl_rhs′ ) _))
                              ctx_binds)
                         stx_body′) ctx) _ξ _scpsₚ)
      κ Σ)
   #:when (and (id=? ph id_kont  '#%kont Σ)
               (id=? ph id_kont′ '#%kont Σ)
               (id=? ph id_let   'let    Σ))
   (ζ (Stx (Lst id_let (Stx (zip stl_vars′ stl_rhs′ (empty-ctx)) ctx_binds)
                stx_body′) ctx)
      κ Σ)
   ex-let]

  ;; quote
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_quote) _) _ctx) stx) _ξ _scpsₚ)
      κ Σ)
   #:when (id=? ph id_quote 'quote Σ)
   (ζ stx
      κ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (Stx (Lst (? id? id_syntax) stx) ctx) _ξ scpsₚ)
      κ Σ)
   #:when (id=? ph id_syntax 'syntax Σ)
   (:= stx′ (prune ph stx scpsₚ))
   (ζ (Stx (Lst id_syntax stx′) ctx)
      κ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_ls)
                              (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_bind))
                                   ctx_binds)
                              stx_body) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id=? ph id_ls 'let-syntax Σ₀)
   (:= (values nam Σ₁) (alloc-name   id Σ₀)) 
   (:= (values scp Σ₂) (alloc-scope 'ls Σ₁))
   (:= id′             (add ph id scp))
   (:= Σ₃              (bind ph Σ₂ id′ nam))
   (:= (values 𝓁 Σ₄)   (push-κ Σ₃ stx κ₀))
   (:= stx_body′       (add ph stx_body scp))
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ) ∅)
      (κ (Stxξ ph (Stx (Lst id-kont id_ls
                            (Stx (Lst (Stx (Lst id′ (Hole)) ctx_bind))
                                 ctx_binds)
                            stx_body′) ctx) ξ (set-add scpsₚ scp)) 𝓁) Σ₄)
   ex-ls-rhs]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_ls)
                         (Stx (Lst (Stx (Lst (? id? id′) stx_rhs′) _ctx_bind))
                              _ctx_binds)
                         stx_body′) ctx) ξ scpsₚ′)
      κ Σ)
   #:when (and (id=? ph id_kont '#%kont     Σ)
               (id=? ph id_ls   'let-syntax Σ))
   (<- ast (parse (add1 ph) stx_rhs′ Σ))
   (InEval (list (AstEnv ast (init-env)) '● (init-store))
           (ζ (Stxξ ph (Stx (Lst id′ stx_body′) (empty-ctx)) ξ scpsₚ′)
              κ Σ))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _sto)
           (ζ (Stxξ ph (Stx (Lst (? id? id′) stx_body′) _ctx) ξ scpsₚ′)
              κ Σ))
   (:=<1> nam (resolve ph id′ Σ))
   (:=    ξ′  (extend-ξ ξ nam val))
   (ζ (Stxξ ph stx_body′ ξ′ scpsₚ′)
      κ Σ)
   ex-ls]

  ;; macro invocation
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id) _stx ...) ctx) stx) ξ scpsₚ)
      κ Σ₀)
   (:=<1> nam (resolve ph id Σ₀))
   (:=<1> val (lookup-ξ ξ nam))
   #:when (val? val)
   (:= (values scpᵤ Σ₁) (alloc-scope 'u Σ₀))
   (:= (values scpᵢ Σ₂) (alloc-scope 'i Σ₁))
   (InEval
    (list (AstEnv (App (gensym 'macapp) ;; TODO: OK?
                       val
                       (list (flip ph (add ph stx scpᵤ) scpᵢ)))
                  (init-env))
          '● (init-store))
    (ζ (Stxξ ph (add ph (Stx (Bool #f) (empty-ctx)) scpᵢ) ξ
             (set-add scpsₚ scpᵤ))
       κ Σ₂))
   ex-macapp-eval]

  [(InEval (list (? stx? stx) '● _sto)
           (ζ (Stxξ ph (Stx (Bool #f) ctxᵢ) ξ scpsₚ)
              κ Σ))
   (:= (set scpᵢ) (at-phase ctxᵢ ph))
   (ζ (Stxξ ph (flip ph stx scpᵢ) ξ scpsₚ)
      κ Σ)
   ex-macapp]

  ;; if
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_if) . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id=? ph id_if 'if Σ₀)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl) ctx) ξ scpsₚ)
      (κ (Stxξ ph (Stx (Lst id-kont id_if (Hole)) (empty-ctx)) ξ scpsₚ) 𝓁) Σ₁)
   ex-if-seq]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_if)
                         (Stx (? proper-stl? stl′) ctx)) _ctx) _ξ _scpsₚ)
      κ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ)
               (id=? ph id_if   'if     Σ))
   (ζ (Stx (Lst id_if . stl′) ctx)
      κ Σ)
   ex-if]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Pair (? id? id_app)
                               (Stx (Lst stx_f . stl) ctx_seq)) ctx) stx) ξ
            scpsₚ)
      κ₀ Σ₀)
   #:when (id=? ph id_app '#%app Σ₀)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx_seq) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_app) stx_f . stl) ctx) stx) ξ
            scpsₚ)
      κ₀ Σ₀)
   #:when (id=? ph id_app '#%app Σ₀)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app′]

  ;; application (lambda or primitive)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (not (id? stx_f))
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app]

  ;; application (bound var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   (:=<1> nam (resolve ph stx_f Σ₀))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (TVar? at)
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-bound]

  ;; application (free var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   (:=<1> nam (resolve ph stx_f Σ₀))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (and (eq? 'not-found at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (? id? id) ξ _scpsₚ)
      κ Σ)
   (:=<1> nam (resolve ph id Σ))
   (:=<1> at  (lookup-ξ ξ nam))
   (match at
     [(TVar id′) (ζ id′ κ Σ)]
     [_ (error '==>p "unbound identifier: ~a" nam)])
   ex-var]

  ;; literal
  [(ζ (Stxξ _ph (and (Stx (? Atom? atom) ctx) stx) _ξ _scpsₚ)
      κ Σ)
   #:when (not (Sym? atom))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ _ph (and (Stx (? prim?) ctx) stx) _ξ _scpsₚ)
      κ Σ)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-prim]

  ;; pop κ
  [(ζ (? Stx? stx)
      (κ (Stxξ ph stxₖ ξ scpsₚ) 𝓁) Σ)
   (:=<1> κ₀ (lookup-κ Σ 𝓁))
   (ζ (Stxξ ph (in-hole stxₖ stx) ξ scpsₚ)
      κ₀ Σ)
   ex-pop-κ]

  ;; pop κ′
  [(ζ (? Stx? stx)
      (κ (? (compose1 not Stxξ?) stxₖ) 𝓁) Σ)
   (:=<1> κ₀ (lookup-κ Σ 𝓁))
   (ζ (in-hole stxₖ stx)
      κ₀ Σ)
   ex-pop-κ′]

  ;; in eval
  [(InEval s ζ)
   (<- s′ (lift (--> s)))
   (InEval s′ ζ)
   ex-in-eval]

  ;;;; expression sequences

  ;; (#%seq (d ...) e₀ e ...) ==> (#%seq (d ... (expand e₀)) e ...)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_seq)
                              (? Stx? stx′)
                              stx₀ . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id=? ph id_seq '#%seq Σ₀)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph stx₀ ξ scpsₚ)
      (κ (Stxξ ph (Stx (Lst id-kont id_seq
                            (Stx (Lst id-snoc stx′ (Hole)) (empty-ctx))
                            . stl) ctx) ξ scpsₚ) 𝓁) Σ₁)
   ex-seq-car]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_seq)
                         (Stx (Lst (? id? id_snoc)
                                   (Stx stl′ _ctx′) (? stx? stx₀′)) _ctx)
                         . stl) ctx) ξ scpsₚ)
      κ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ)
               (id=? ph id_seq  '#%seq  Σ)
               (id=? ph id_snoc '#%snoc Σ))
   (ζ (Stxξ ph (Stx (Lst id_seq
                         (Stx (snoc stl′ stx₀′) (empty-ctx))
                         . stl) ctx) ξ scpsₚ)
      κ Σ)
   ex-seq-snoc]

  ;; (#%seq (d ...)) ==> (d ...)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_seq) (Stx stl′ _ctx′)) ctx) _ξ _scpsₚ)
      κ Σ)
   #:when (id=? ph id_seq '#%seq Σ)
   (ζ (Stx stl′ ctx)
      κ Σ)
   ex-seq])


(define-unit-from-reduction red@ ==>)

(define-mixed-unit expand@
  (import  domain^ syntax^ env^ store^ eval^
           menv^ mstore^ mcont^ bind^ id^ parse^)
  (export  expand^)
  (inherit [red@    reducer])

  ;; δ → ζ → (Setof ζ)
  (define (==> δ) (reducer (--> δ) :=)))
