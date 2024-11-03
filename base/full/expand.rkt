#lang racket/base
(require
 racket/unit
 (only-in racket/match       match match-let)
 (only-in "../../set.rkt"    set ∅ set-add set→list)
 (only-in "../../mix.rkt"    define-mixed-unit inherit)
 (only-in "../../syntax.rkt" snoc stx→datum)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt")
(provide ==> expand@)

;; ----------------------------------------
;; The expander:
;;   ζ ∷= ⟨⟨Ph, Stx, ξ⟩ ∪ Stx, κ, ⟨Σ, Scps, Scps⟩⟩
;;   κ ∷= ⟨⟨Ph, Stx, ξ⟩ ∪ Stx, Scps, Scps, 𝓁⟩

;; ==> : ζ → (Setof ζ)
(define-reduction (==> --> :=<1>)
  #:import [(only common^    push-κ regist-vars)
            (only domain^    val? stx? proper-stl?)
            (only syntax^    empty-ctx zip unzip add flip in-hole prune at-phase)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope lookup-κ)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; Constants
        (define id-kont (Stx (Sym '#%kont) (empty-ctx)))
        (define id-seq  (Stx (Sym '#%seq)  (empty-ctx)))
        (define id-snoc (Stx (Sym '#%snoc) (empty-ctx)))
        (define stx-nil (Stx (Null)        (empty-ctx)))

        ;; id=? : Ph Id Nam ξ Σ → Boolean
        (define (id=? ph id nam ξ Σ)
          (let ([nam′ (resolve ph id Σ)])
            (and (eq? nam nam′) (not (TStop? (lookup-ξ ξ nam))))))]

  #:default [(ζ (Stxξ ph stx ξ) κ Σ̂) ;; for debug
             (if (id? stx)
               (printf "expand: unbound identifier: ~a\n"
                       (Sym-nam (Stx-e stx)))
               (printf "expand: unknown form ~a\n"
                       (lst→list/recur (stx→datum stx))))]

  ;; stops
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id) . _stl) _ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> nam (resolve ph id Σ₀))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (TStop? at)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ stx
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-stop]

  ;; lambda
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_lam)
                              (Stx (? proper-stl? stl_params) ctx_params)
                              stx_body) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> lambda? (id=? ph id_lam 'lambda ξ Σ₀))
   #:when lambda?
   (:= (values scp Σ₁)            (alloc-scope 'lam Σ₀))
   (:= (values stl_params′ ξ′ Σ₂) (regist-vars ph scp stl_params ξ Σ₁))
   (:= (values 𝓁 Σ₃)              (push-κ Σ₂ stx κ₀))
   (ζ (Stxξ ph (add ph stx_body scp) ξ′)
      (κ (Stx (Lst id_lam (Stx stl_params′ ctx_params)
                   (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₃ (set-add scpsₚ scp) ∅))
   ex-lam]

  ;; let
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_let)
                              (Stx (? proper-stl? stl_binds) ctx_binds)
                              stx_body) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> let? (id=? ph id_let 'let ξ Σ₀))
   #:when let?
   (:= (values stl_vars stl_rhs) (unzip stl_binds))
   (:= (values scp Σ₁)           (alloc-scope 'let Σ₀))
   (:= (values stl_vars′ ξ′ Σ₂)  (regist-vars ph scp stl_vars ξ Σ₁))
   (:= (values 𝓁 Σ₃)             (push-κ Σ₂ stx κ₀))
   (ζ (Stxξ ph (add ph stx_body scp) ξ′)
      (κ (Stxξ ph (Stx (Lst id-kont id_let
                            (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                      (Stx stl_rhs   (empty-ctx)))
                                 ctx_binds)
                            (Hole)) ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₃ (set-add scpsₚ scp) ∅))
   ex-let-body]
  
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_kont) (? id? id_let)
                              (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                        (Stx (? proper-stl? stl_rhs  ) _))
                                   ctx_binds)
                              stx_body′) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> kont? (id=? ph id_kont '#%kont ξ Σ₀))
   (:=<1> let?  (id=? ph id_let  'let    ξ Σ₀))
   #:when (and kont? let?)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_rhs) ctx_binds) ξ)
      (κ (Stxξ ph (Stx (Lst id_kont id_kont id_let
                            (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                      (Hole))
                                 ctx_binds)
                            stx_body′) ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-let-rhs]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_kont′) (? id? id_let)
                         (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                   (Stx (? proper-stl? stl_rhs′ ) _))
                              ctx_binds)
                         stx_body′) ctx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> kont?  (id=? ph id_kont  '#%kont ξ Σ₀))
   (:=<1> kont′? (id=? ph id_kont′ '#%kont ξ Σ₀))
   (:=<1> let?   (id=? ph id_let   'let    ξ Σ₀))
   #:when (and kont? kont′? let?)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx_body′ κ₀))
   (ζ (Stx (Lst id_let (Stx (zip stl_vars′ stl_rhs′ (empty-ctx)) ctx_binds)
                stx_body′) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-let]

  ;; quote
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_quote) _) _ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> quote? (id=? ph id_quote 'quote ξ Σ₀) )
   #:when quote?
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ stx
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (Stx (Lst (? id? id_syntax) stx) ctx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> syntax? (id=? ph id_syntax 'syntax ξ Σ₀))
   #:when syntax?
   (:= stx′          (prune ph stx scpsₚ))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stx (Lst id_syntax stx′) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-stx]

  ;; macro creation (eval gets more and updates store)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_ls)
                              (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_bind))
                                   ctx_binds)
                              stx_body) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> let-syntax? (id=? ph id_ls 'let-syntax ξ Σ₀))
   #:when let-syntax?
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ))
      (κ (Stxξ ph (Stx (Lst id-kont id_ls
                            (Stx (Lst (Stx (Lst id (Hole)) ctx_bind))
                                 ctx_binds)
                            stx_body) ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-ls-rhs]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_ls)
                         (Stx (Lst (Stx (Lst (? id? id) stx_rhs′) _ctx_bind))
                              _ctx_binds)
                         stx_body) ctx) ξ)
      κ
      (Σ̂ Σ scpsₚ scpsᵤ))
   (:=<1> kont?       (id=? ph id_kont '#%kont     ξ Σ))
   (:=<1> let-syntax? (id=? ph id_ls   'let-syntax ξ Σ))
   #:when (and kont? let-syntax?)
   (<- ast (parse (add1 ph) stx_rhs′ Σ))
   (InEval (list (AstEnv ph ast (init-env) 'no-scope ξ)
                 '● (init-store)
                 (Σ̂ Σ scpsₚ ∅))
           (ζ (Stxξ ph (Stx (Lst id stx_body) (empty-ctx)) ξ)
              κ
              (Σ̂ Σ scpsₚ scpsᵤ)))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _sto (Σ̂ Σ₀ _scpsₚ _scpsᵤ))
           (ζ (Stxξ ph (Stx (Lst (? id? id) stx_body) _ctx) ξ)
              κ₀
              (Σ̂ _Σ scpsₚ scpsᵤ)))
   (:= (values nam Σ₁) (alloc-name   id Σ₀))
   (:= (values scp Σ₂) (alloc-scope 'ls Σ₁))
   (:= id′             (add ph id scp))
   (:= Σ₃              (bind ph Σ₂ id′ nam))
   (:= ξ′              (extend-ξ ξ nam val))
   (:= stx_body′       (add ph stx_body scp))
   (:= (values 𝓁 Σ₄)   (push-κ Σ₃ stx_body κ₀))
   (ζ (Stxξ ph stx_body′ ξ′)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₄ (set-add scpsₚ scp) ∅))
   ex-ls]

  ;; macro invocation
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id) _stx ...) ctx) stx) ξ)
      κ
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> nam (resolve ph id Σ₀))
   (:=<1> val (lookup-ξ ξ nam))
   #:when (val? val)
   (:= (values scpᵤ Σ₁) (alloc-scope 'u Σ₀))
   (:= (values scpᵢ Σ₂) (alloc-scope 'i Σ₁))
   (InEval
    (list (AstEnv ph (App (gensym 'macapp) ;; TODO: OK?
                          val
                          (list (flip ph (add ph stx scpᵤ) scpᵢ)))
                  (init-env) scpᵢ ξ)
          '● (init-store)
          (Σ̂ Σ₂ (set-add scpsₚ scpᵤ) (set-add scpsᵤ scpᵤ)))
    (ζ (Stxξ ph (add ph (Stx (Bool #f) (empty-ctx)) scpᵢ) ξ)
       κ
       (Σ̂ Σ₀ ∅ ∅))) ;; not used
   ex-macapp-eval]

  [(InEval (list (? stx? stx) '● _sto Σ̂)
           (ζ (Stxξ ph (Stx (Bool #f) ctxᵢ) ξ)
              κ
              _Σ̂))
   (:= (set scpᵢ) (at-phase ctxᵢ ph))
   (ζ (Stxξ ph (flip ph stx scpᵢ) ξ)
      κ
      Σ̂)
   ex-macapp]

  ;; if
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_if) . stl) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> if? (id=? ph id_if 'if ξ Σ₀))
   #:when if?
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl) ctx) ξ)
      (κ (Stxξ ph (Stx (Lst id-kont id_if (Hole)) (empty-ctx)) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-if-seq]

  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_kont) (? id? id_if)
                              (Stx (? proper-stl? stl′) ctx)) _ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> kont? (id=? ph id_kont '#%kont ξ Σ₀))
   (:=<1> if?   (id=? ph id_if   'if     ξ Σ₀))
   #:when (and kont? if?)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stx (Lst id_if . stl′) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-if]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Pair (? id? id_app)
                               (Stx (Lst stx_f . stl) ctx_seq)) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> app? (id=? ph id_app '#%app ξ Σ₀))
   #:when app?
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx_seq) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-#%app]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_app) stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> app? (id=? ph id_app '#%app ξ Σ₀))
   #:when app?
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-#%app′]

  ;; application (lambda or primitive)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:when (not (id? stx_f))
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-app]

  ;; application (bound var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:when (id? stx_f)
   (:=<1> nam (resolve ph stx_f Σ₀))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (TVar? at)
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-app-bound]

  ;; application (free var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:when (id? stx_f)
   (:=<1> nam (resolve ph stx_f Σ₀))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (and (eq? 'not-found at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (? id? id) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> nam           (resolve ph id Σ₀))
   (:=<1> at            (lookup-ξ ξ nam))
   #:when (TVar? at)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ id κ₀))
   (ζ (TVar-id at)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-var]

  ;; literal
  [(ζ (Stxξ _ph (and (Stx (? Atom? atom) ctx) stx) _ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:when (not (Sym? atom))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ _ph (and (Stx (? prim?) ctx) stx) _ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-prim]

  ;; pop κ
  [(ζ (? Stx? stx)
      (κ (Stxξ ph stxₖ ξ) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ _scpsₚ _scpsᵤ))
   (:=<1> κ₀ (lookup-κ Σ 𝓁))
   (ζ (Stxξ ph (in-hole stxₖ stx) ξ)
      κ₀
      (Σ̂ Σ scpsₚ scpsᵤ))
   ex-pop-κ]

  ;; pop κ′
  [(ζ (? Stx? stx)
      (κ (? (compose1 not Stxξ?) stxₖ) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ _scpsₚ _scpsᵤ))
   (:=<1> κ₀ (lookup-κ Σ 𝓁))
   (ζ (in-hole stxₖ stx)
      κ₀
      (Σ̂ Σ scpsₚ scpsᵤ))
   ex-pop-κ′]

  ;; in-eval
  [(InEval s ζ)
   (<- s′ (lift ((-->) s)))
   (InEval s′ ζ)
   ex-in-eval]

  ;;;; expression sequence

  ;; (#%seq (d ...) e₀ e ...) ==> (#%seq (d ... (expand e₀)) e ...)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_seq)
                              (? Stx? stx′)
                              stx₀ . stl) ctx) stx) ξ)
      κ₀
      (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (:=<1> seq? (id=? ph id_seq '#%seq ξ Σ₀))
   #:when seq?
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph stx₀ ξ)
      (κ (Stxξ ph (Stx (Lst id-kont id_seq
                            (Stx (Lst id-snoc stx′ (Hole)) (empty-ctx))
                            . stl) ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-seq-car]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_seq)
                         (Stx (Lst (? id? id_snoc)
                                   (Stx stl′ _ctx′) (? stx? stx₀′)) _ctx)
                         . stl) ctx) ξ)
      κ
      (Σ̂ Σ scpsₚ scpsᵤ))
   (:=<1> kont? (id=? ph id_kont '#%kont ξ Σ))
   (:=<1> seq?  (id=? ph id_seq  '#%seq  ξ Σ))
   (:=<1> snoc? (id=? ph id_snoc '#%snoc ξ Σ))
   #:when (and kont? seq? snoc?)
   (ζ (Stxξ ph (Stx (Lst id_seq
                         (Stx (snoc stl′ stx₀′) (empty-ctx))
                         . stl) ctx) ξ)
      κ
      (Σ̂ Σ scpsₚ scpsᵤ))
   ex-seq-snoc]

  ;; (#%seq (d ...)) ==> (d ...)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_seq) (Stx stl′ _ctx′)) ctx) ξ)
      κ
      (Σ̂ Σ _scpsₚ _scpsᵤ))
   (:=<1> seq? (id=? ph id_seq '#%seq ξ Σ))
   #:when seq?
   (ζ (Stx stl′ ctx)
      κ
      (Σ̂ Σ ∅ ∅))
   ex-seq])


(define-unit-from-reduction red@ ==>)

(define-mixed-unit expand@
  (import  domain^ syntax^ env^ store^ eval^
           menv^ mstore^ bind^ parse^)
  (export  expand^)
  (inherit [red@    reducer])
  
  ;; δ → → ζ → (Setof ζ)
  (define (==> δ) (λ () (reducer (--> δ) :=))))
