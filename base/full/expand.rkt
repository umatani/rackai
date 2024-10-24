#lang racket/base
(require
 racket/unit
 (only-in racket/match       match match-let)
 (only-in "../../set.rkt"    set ∅ set-add set→list)
 (only-in "../../syntax.rkt" snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt")
(provide ==> expand/red@ expand@)

;; ----------------------------------------
;; The expander:
;;   ζ ∷= ⟨⟨Ph, Stx, ξ⟩ ∪ Stx, κ, ⟨Σ, Scps, Scps⟩⟩
;;   κ ∷= ⟨⟨Ph, Stx, ξ⟩ ∪ Stx, Scps, Scps, 𝓁⟩

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> --> :=<1>)
  #:within-signatures [(only domain^    val? stx? proper-stl?)
                       (only syntax^    empty-ctx zip unzip add flip in-hole
                                        prune at-phase)
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

        ;; regist-vars : Ph Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)
        ;;   (same as phases)
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

  ;; stops
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id) . _stl) _ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:with           nam :=<1> (resolve ph id Σ₀)
   #:with            at :=<1> (lookup-ξ ξ nam)
   #:when (TStop? at)
   #:with (values 𝓁 Σ₁) :=    (push-κ Σ₀ stx κ₀)
   (ζ stx
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ ∅ ∅))
   ex-stop]

  ;; lambda
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_lam)
                              (Stx (? proper-stl? stl_params) ctx_params)
                              stx_body) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_lam 'lambda ξ Σ₀)
   #:with            (values scp Σ₁) := (alloc-scope 'lam Σ₀)
   #:with (values stl_params′ ξ′ Σ₂) := (regist-vars ph scp stl_params ξ Σ₁)
   #:with              (values 𝓁 Σ₃) := (push-κ Σ₂ stx κ₀)
   (ζ (Stxξ ph (add ph stx_body scp) ξ′)
      (κ (Stx (Lst id_lam (Stx stl_params′ ctx_params)
                   (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₃ (set-add scpsₚ scp) ∅))
   ex-lam]

  ;; let
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_let)
                              (Stx (? proper-stl? stl_binds) ctx_binds)
                              stx_body) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_let 'let ξ Σ₀)
   #:with (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with           (values scp Σ₁) := (alloc-scope 'let Σ₀)
   #:with  (values stl_vars′ ξ′ Σ₂) := (regist-vars ph scp stl_vars ξ Σ₁)
   #:with             (values 𝓁 Σ₃) := (push-κ Σ₂ stx κ₀)
   (ζ (Stxξ ph (add ph stx_body scp) ξ′)
      (κ (Stxξ ph (Stx (Lst id-kont id_let
                            (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                      (Stx stl_rhs   (empty-ctx)))
                                 ctx_binds)
                            (Hole)) ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₃ (set-add scpsₚ scp) ∅))
   ex-let-body]
  
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_kont) (? id? id_let)
                              (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                        (Stx (? proper-stl? stl_rhs  ) _))
                                   ctx_binds)
                              stx_body′) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (and (id=? ph id_kont '#%kont ξ Σ₀)
               (id=? ph id_let  'let    ξ Σ₀))
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl_rhs) ctx_binds) ξ)
      (κ (Stxξ ph (Stx (Lst id_kont id_kont id_let
                            (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                      (Hole))
                                 ctx_binds)
                            stx_body′) ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-let-rhs]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_kont′) (? id? id_let)
                         (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                   (Stx (? proper-stl? stl_rhs′ ) _))
                              ctx_binds)
                         stx_body′) ctx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (and (id=? ph id_kont  '#%kont ξ Σ₀)
               (id=? ph id_kont′ '#%kont ξ Σ₀)
               (id=? ph id_let   'let    ξ Σ₀))
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx_body′ κ₀)
   (ζ (Stx (Lst id_let (Stx (zip stl_vars′ stl_rhs′ (empty-ctx)) ctx_binds)
                stx_body′) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ ∅ ∅))
   ex-let]

  ;; quote
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_quote) _) _ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_quote 'quote ξ Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ stx
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ ∅ ∅))
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (Stx (Lst (? id? id_syntax) stx) ctx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_syntax 'syntax ξ Σ₀)
   #:with stx′ := (prune ph stx scpsₚ)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stx (Lst id_syntax stx′) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ ∅ ∅))
   ex-stx]

  ;; macro creation (eval gets more and updates store)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_ls)
                              (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_bind))
                                   ctx_binds)
                              stx_body) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_ls 'let-syntax ξ Σ₀)
   #:with (values nam Σ₁) := (alloc-name   id Σ₀)
   #:with (values scp Σ₂) := (alloc-scope 'ls Σ₁)
   #:with             id′ := (add ph id scp)
   #:with              Σ₃ := (bind ph Σ₂ id′ nam)
   #:with   (values 𝓁 Σ₄) := (push-κ Σ₃ stx κ₀)
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ))
      (κ (Stxξ ph (Stx (Lst id-kont id_ls
                            (Stx (Lst (Stx (Lst id′ (Hole)) ctx_bind))
                                 ctx_binds)
                            stx_body (add ph (Stx (Bool #f) (empty-ctx)) scp))
                       ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₄ ∅ ∅))
   ex-ls-rhs]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_ls)
                         (Stx (Lst (Stx (Lst (? id? id′) stx_rhs′) _ctx_bind))
                              _ctx_binds)
                         stx_body (Stx (Bool #f) ctx_scp))
                    ctx) ξ)
      κ
      (Σ* Σ scpsₚ scpsᵤ))
   #:when (and (id=? ph id_kont '#%kont     ξ Σ)
               (id=? ph id_ls   'let-syntax ξ Σ))
   #:with ast :=<1> (parse (add1 ph) stx_rhs′ Σ)
   (InEval (list (AstEnv ph ast (init-env) 'no-scope ξ)
                 '● (init-store)
                 (Σ* Σ scpsₚ ∅))
           (ζ (Stxξ ph (Stx (Lst id′ stx_body (Stx (Bool #f) ctx_scp))
                            (empty-ctx)) ξ)
              κ
              (Σ* Σ scpsₚ scpsᵤ)))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _sto (Σ* Σ₀ _scpsₚ _scpsᵤ))
           (ζ (Stxξ ph (Stx (Lst (? id? id′) stx_body
                                 (Stx (Bool #f) ctx_scp))
                            _ctx) ξ)
              κ₀
              (Σ* _Σ scpsₚ scpsᵤ)))
   #:with       nam     :=<1> (resolve ph id′ Σ₀)
   #:with (set scp)     :=    (at-phase ctx_scp ph)
   #:with        ξ′     :=    (extend-ξ ξ nam val)
   #:with stx_body′     :=    (add ph stx_body scp)
   #:with (values 𝓁 Σ₁) :=    (push-κ Σ₀ stx_body κ₀)
   (ζ (Stxξ ph stx_body′ ξ′)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ (set-add scpsₚ scp) ∅))
   ex-ls]

  ;; macro invocation
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id) _stx ...) ctx) stx) ξ)
      κ
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:with              nam :=<1> (resolve ph id Σ₀)
   #:with              val :=<1> (lookup-ξ ξ nam)
   #:when (val? val)
   #:with (values scpᵤ Σ₁) :=    (alloc-scope 'u Σ₀)
   #:with (values scpᵢ Σ₂) :=    (alloc-scope 'i Σ₁)
   (InEval
    (list (AstEnv ph (App (gensym 'macapp) ;; TODO: OK?
                          val
                          (list (flip ph (add ph stx scpᵤ) scpᵢ)))
                  (init-env) scpᵢ ξ)
          '● (init-store)
          (Σ* Σ₂ (set-add scpsₚ scpᵤ) (set-add scpsᵤ scpᵤ)))
    (ζ (Stxξ ph (add ph (Stx (Bool #f) (empty-ctx)) scpᵢ) ξ)
       κ
       (Σ* Σ₀ ∅ ∅))) ;; not used
   ex-macapp-eval]

  [(InEval (list (? stx? stx) '● _sto Σ*)
           (ζ (Stxξ ph (Stx (Bool #f) ctxᵢ) ξ)
              κ
              _Σ*))
   #:with (set scpᵢ) := (at-phase ctxᵢ ph)
   (ζ (Stxξ ph (flip ph stx scpᵢ) ξ)
      κ
      Σ*)
   ex-macapp]

  ;; if
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_if) . stl) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_if 'if ξ Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl) ctx) ξ)
      (κ (Stxξ ph (Stx (Lst id-kont id_if (Hole)) (empty-ctx)) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-if-seq]

  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_kont) (? id? id_if)
                              (Stx (? proper-stl? stl′) ctx)) _ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (and (id=? ph id_kont '#%kont ξ Σ₀)
               (id=? ph id_if   'if     ξ Σ₀))
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stx (Lst id_if . stl′) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ ∅ ∅))
   ex-if]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Pair (? id? id_app)
                               (Stx (Lst stx_f . stl) ctx_seq)) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_app '#%app ξ Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx_seq) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-#%app]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_app) stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_app '#%app ξ Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-#%app′]

  ;; application (lambda or primitive)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (not (id? stx_f))
   #:with        id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-app]

  ;; application (bound var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id? stx_f)
   #:with           nam :=<1> (resolve ph stx_f Σ₀)
   #:with            at :=<1> (lookup-ξ ξ nam)
   #:when (TVar? at)
   #:with        id_app :=    (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) :=    (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-app-bound]

  ;; application (free var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id? stx_f)
   #:with           nam :=<1> (resolve ph stx_f Σ₀)
   #:with            at :=<1> (lookup-ξ ξ nam)
   #:when (and (eq? 'not-found at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:with        id_app :=    (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) :=    (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (? id? id) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:with           nam :=<1> (resolve ph id Σ₀)
   #:with            at :=<1> (lookup-ξ ξ nam)
   #:with (values 𝓁 Σ₁) :=    (push-κ Σ₀ id κ₀)
   (match at
     [(TVar id′)
      (ζ id′
         (κ (Hole) scpsₚ scpsᵤ 𝓁)
         (Σ* Σ₁ ∅ ∅))]
     [_ (error '==>f "unbound identifier: ~a" nam)])
   ex-var]

  ;; literal
  [(ζ (Stxξ _ph (and (Stx (? Atom? atom) ctx) stx) _ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (not (Sym? atom))
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ ∅ ∅))
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ _ph (and (Stx (? prim?) ctx) stx) _ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ ∅ ∅))
   ex-prim]

  ;; pop κ
  [(ζ (? Stx? stx)
      (κ (Stxξ ph stxₖ ξ) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ _scpsₚ _scpsᵤ))
   #:with κ₀ :=<1> (lookup-Σ Σ 𝓁)
   (ζ (Stxξ ph (in-hole stxₖ stx) ξ)
      κ₀
      (Σ* Σ scpsₚ scpsᵤ))
   ex-pop-κ]

  ;; pop κ′
  [(ζ (? Stx? stx)
      (κ (? (compose1 not Stxξ?) stxₖ) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ _scpsₚ _scpsᵤ))
   #:with κ₀ :=<1> (lookup-Σ Σ 𝓁)
   (ζ (in-hole stxₖ stx)
      κ₀
      (Σ* Σ scpsₚ scpsᵤ))
   ex-pop-κ′]

  ;; in-eval
  [(InEval s ζ)
   #:with s′ <- (lift ((-->) s))
   (InEval s′ ζ)
   ex-in-eval]

  ;;;; expression sequence

  ;; (#%seq (d ...) e₀ e ...) ==> (#%seq (d ... (expand e₀)) e ...)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_seq)
                              (? Stx? stx′)
                              stx₀ . stl) ctx) stx) ξ)
      κ₀
      (Σ* Σ₀ scpsₚ scpsᵤ))
   #:when (id=? ph id_seq '#%seq ξ Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph stx₀ ξ)
      (κ (Stxξ ph (Stx (Lst id-kont id_seq
                            (Stx (Lst id-snoc stx′ (Hole)) (empty-ctx))
                            . stl) ctx) ξ) scpsₚ scpsᵤ 𝓁)
      (Σ* Σ₁ scpsₚ ∅))
   ex-seq-car]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_seq)
                         (Stx (Lst (? id? id_snoc)
                                   (Stx stl′ _ctx′) (? stx? stx₀′)) _ctx)
                         . stl) ctx) ξ)
      κ
      (Σ* Σ scpsₚ scpsᵤ))
   #:when (and (id=? ph id_kont '#%kont ξ Σ)
               (id=? ph id_seq  '#%seq  ξ Σ)
               (id=? ph id_snoc '#%snoc ξ Σ))
   (ζ (Stxξ ph (Stx (Lst id_seq
                         (Stx (snoc stl′ stx₀′) (empty-ctx))
                         . stl) ctx) ξ)
      κ
      (Σ* Σ scpsₚ scpsᵤ))
   ex-seq-snoc]

  ;; (#%seq (d ...)) ==> (d ...)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_seq) (Stx stl′ _ctx′)) ctx) ξ)
      κ
      (Σ* Σ _scpsₚ _scpsᵤ))
   #:when (id=? ph id_seq '#%seq ξ Σ)
   (ζ (Stx stl′ ctx)
      κ
      (Σ* Σ ∅ ∅))
   ex-seq])


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
    (define ζᵢ (ζ (Stxξ ph stx ξ) '● Σ*))

    (match-let ([(set (ζ stx′ '● Σ*′)) (apply-reduction* (==>δ) ζᵢ)])
      (cons stx′ Σ*′))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ bind^ id^ parse^)
  (export expand^)
  (link expand/red@ red@))

