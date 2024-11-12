#lang racket/base
(require
 racket/unit
 (only-in racket/match       match match-let)
 (only-in "../../set.rkt"    set ∅ set-add set→list)
 (only-in "../../mix.rkt"    define-mixed-unit inherit)
 (only-in "../../syntax.rkt" stx→datum snoc zip unzip prune at-phase)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt")
(provide ==> expand@)

;; ----------------------------------------
;; The expander:
;;   ζ ∷= ⟨⟨Ph, Stx, ξ, Scps⟩ ∪ Stx, κ, Σ⟩
;;   κ ∷= ⟨⟨Ph, Stx, ξ, Scps⟩ ∪ Stx, 𝓁⟩

;; ==> :  ζ -> (SetM ζ)
(define-reduction (==> --> :=<1>)
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only domain^    val? stx?)
            (only syntax^    empty-ctx add flip in-hole proper-stl?)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; Constants
        (define id-kont (Stx (Sym '#%kont) (empty-ctx)))
        (define id-seq  (Stx (Sym '#%seq)  (empty-ctx)))
        (define id-snoc (Stx (Sym '#%snoc) (empty-ctx)))
        (define stx-nil (Stx (Null)        (empty-ctx)))

        ;; id=? : Ph Id Nam Σ → Boolean
        (define (id=? ph id nam Σ)
          (eq? (resolve ph id Σ) nam))]

  #:default [(ζ (Stxξ ph stx ξ scpsₚ) κ Σ)
             #:abort-if (id? stx) (format "expand: unbound identifier: ~a\n"
                                          (Sym-nam (Stx-e stx)))
             #:abort (format "expand: unknown form ~a\n"
                             (lst→list/recur (stx→datum stx)))]

  ;; lambda
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_lam)
                              (Stx (? proper-stl? stl_params) ctx_params)
                              stx_body) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   lambda? :=<1> (id=? ph id_lam 'lambda Σ₀)
   #:when lambda?
   #:checkpoint (printf "ex-lam\n")
              (values scp Σ₁) := (alloc-scope 'lam Σ₀)
   (values stl_params′ ξ′ Σ₂) := (regist-vars ph scp stl_params ξ Σ₁)
                (values 𝓁 Σ₃) := (push-κ Σ₂ stx κ₀)
   (ζ (Stxξ ph (add ph stx_body scp) ξ′ (set-add scpsₚ scp))
      (κ (Stx (Lst id_lam (Stx stl_params′ ctx_params)
                   (Hole)) ctx) 𝓁) Σ₃)
   ex-lam]

  ;; let
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_let)
                              (Stx (? proper-stl? stl_binds) ctx_binds)
                              stx_body) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   let? :=<1> (id=? ph id_let 'let Σ₀)
   #:when let?
   #:checkpoint (printf "ex-let-body\n")
   (values stl_vars stl_rhs) := (unzip stl_binds)
             (values scp Σ₁) := (alloc-scope 'let Σ₀)
    (values stl_vars′ ξ′ Σ₂) := (regist-vars ph scp stl_vars ξ Σ₁)
               (values 𝓁 Σ₃) := (push-κ Σ₂ stx κ₀)
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
   kont? :=<1> (id=? ph id_kont '#%kont Σ₀)
    let? :=<1> (id=? ph id_let  'let    Σ₀)
   #:when (and kont? let?)
   #:checkpoint (printf "ex-let-rhs\n")
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
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
    kont? :=<1> (id=? ph id_kont  '#%kont Σ)
   kont′? :=<1> (id=? ph id_kont′ '#%kont Σ)
     let? :=<1> (id=? ph id_let   'let    Σ)
   #:when (and kont? kont′? let?)
   #:checkpoint (printf "ex-let\n")
   (ζ (Stx (Lst id_let (Stx (zip stl_vars′ stl_rhs′ (empty-ctx)) ctx_binds)
                stx_body′) ctx)
      κ Σ)
   ex-let]

  ;; quote
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_quote) _) _ctx) stx) _ξ _scpsₚ)
      κ Σ)
   quote? :=<1> (id=? ph id_quote 'quote Σ)
   #:when quote?
   #:checkpoint (printf "ex-quote\n")
   (ζ stx
      κ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (Stx (Lst (? id? id_syntax) stx) ctx) _ξ scpsₚ)
      κ Σ)
   syntax? :=<1> (id=? ph id_syntax 'syntax Σ)
   #:when syntax?
   #:checkpoint (printf "ex-stx\n")
   stx′ := (prune ph stx scpsₚ)
   (ζ (Stx (Lst id_syntax stx′) ctx)
      κ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_ls)
                              (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_bind))
                                   ctx_binds)
                              stx_body) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   let-syntax? :=<1> (id=? ph id_ls 'let-syntax Σ₀)
   #:when let-syntax?
   #:checkpoint (printf "ex-ls-rhs\n")
   (values nam Σ₁) := (alloc-name   id Σ₀) 
   (values scp Σ₂) := (alloc-scope 'ls Σ₁)
               id′ := (add ph id scp)
                Σ₃ := (bind ph Σ₂ id′ nam)
     (values 𝓁 Σ₄) := (push-κ Σ₃ stx κ₀)
    stx_body′ :=      (add ph stx_body scp)
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
         kont? :=<1> (id=? ph id_kont '#%kont     Σ)
   let-syntax? :=<1> (id=? ph id_ls   'let-syntax Σ)
   #:when (and kont? let-syntax?)
   #:checkpoint (printf "ex-ls-eval\n")
   ast <- (parse (add1 ph) stx_rhs′ Σ)
   (InEval (list (AstEnv ast (init-env)) '● (init-store))
           (ζ (Stxξ ph (Stx (Lst id′ stx_body′) (empty-ctx)) ξ scpsₚ′)
              κ Σ))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _sto)
           (ζ (Stxξ ph (Stx (Lst (? id? id′) stx_body′) _ctx) ξ scpsₚ′)
              κ Σ))
   #:checkpoint (printf "ex-ls\n")
   nam :=<1> (resolve ph id′ Σ)
    ξ′ :=    (extend-ξ ξ nam val)
   (ζ (Stxξ ph stx_body′ ξ′ scpsₚ′)
      κ Σ)
   ex-ls]

  ;; macro invocation
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id) _stx ...) ctx) stx) ξ scpsₚ)
      κ Σ₀)
   nam :=<1> (resolve ph id Σ₀)
   val :=<1> (lookup-ξ ξ nam)
   #:when (val? val)
   #:checkpoint (printf "ex-macapp-eval\n")
   (values scpᵤ Σ₁) := (alloc-scope 'u Σ₀)
   (values scpᵢ Σ₂) := (alloc-scope 'i Σ₁)
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
   #:checkpoint (printf "ex-macapp\n")
   (set scpᵢ) := (at-phase ctxᵢ ph)
   (ζ (Stxξ ph (flip ph stx scpᵢ) ξ scpsₚ)
      κ Σ)
   ex-macapp]

  ;; if
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_if) . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   if? :=<1> (id=? ph id_if 'if Σ₀)
   #:when if?
   #:checkpoint (printf "ex-if-seq\n")
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil . stl) ctx) ξ scpsₚ)
      (κ (Stxξ ph (Stx (Lst id-kont id_if (Hole)) (empty-ctx)) ξ scpsₚ) 𝓁) Σ₁)
   ex-if-seq]

  [(ζ (Stxξ ph (Stx (Lst (? id? id_kont) (? id? id_if)
                         (Stx (? proper-stl? stl′) ctx)) _ctx) _ξ _scpsₚ)
      κ Σ)
   kont? :=<1> (id=? ph id_kont '#%kont Σ)
     if? :=<1> (id=? ph id_if   'if     Σ)
   #:when (and kont? if?)
   #:checkpoint (printf "ex-if\n")
   (ζ (Stx (Lst id_if . stl′) ctx)
      κ Σ)
   ex-if]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Pair (? id? id_app)
                               (Stx (Lst stx_f . stl) ctx_seq)) ctx) stx) ξ
            scpsₚ)
      κ₀ Σ₀)
   app? :=<1> (id=? ph id_app '#%app Σ₀)
   #:when app?
   #:checkpoint (printf "ex-#%app\n")
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx_seq) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_app) stx_f . stl) ctx) stx) ξ
            scpsₚ)
      κ₀ Σ₀)
   app? :=<1> (id=? ph id_app '#%app Σ₀)
   #:when app?
   #:checkpoint (printf "ex-#%app′\n")
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app′]

  ;; application (lambda or primitive)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (not (id? stx_f))
   #:checkpoint (printf "ex-app\n")
          id_app := (Stx (Sym '#%app) ctx)
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app]

  ;; application (bound var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   nam :=<1> (resolve ph stx_f Σ₀)
    at :=<1> (lookup-ξ ξ nam)
   #:when (TVar? at)
   #:checkpoint (printf "ex-app-bound\n")
          id_app := (Stx (Sym '#%app) ctx)
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-bound]

  ;; application (free var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   nam :=<1> (resolve ph stx_f Σ₀)
    at :=<1> (lookup-ξ ξ nam)
   #:when (and (eq? 'not-found at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:checkpoint (printf "ex-app-free\n")
          id_app := (Stx (Sym '#%app) ctx)
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (? id? id) ξ _scpsₚ)
      κ Σ)
   nam :=<1> (resolve ph id Σ)
    at :=<1> (lookup-ξ ξ nam)
   #:when (TVar? at)
   #:checkpoint (printf "ex-var\n")
   (ζ (TVar-id at)
      κ Σ)
   ex-var]

  ;; literal
  [(ζ (Stxξ _ph (and (Stx (? Atom? atom) ctx) stx) _ξ _scpsₚ)
      κ Σ)
   #:when (not (Sym? atom))
   #:checkpoint (printf "ex-lit\n")
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ _ph (and (Stx (? prim?) ctx) stx) _ξ _scpsₚ)
      κ Σ)
   #:checkpoint (printf "ex-prim\n")
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-prim]

  ;; pop κ
  [(ζ (? Stx? stx)
      (κ (Stxξ ph stxₖ ξ scpsₚ) 𝓁) Σ)
   κ₀ :=<1> (lookup-κ Σ 𝓁)
   (ζ (Stxξ ph (in-hole stxₖ stx) ξ scpsₚ)
      κ₀ Σ)
   ex-pop-κ]

  ;; pop κ′
  [(ζ (? Stx? stx)
      (κ (? (compose1 not Stxξ?) stxₖ) 𝓁) Σ)
   κ₀ :=<1> (lookup-κ Σ 𝓁)
   (ζ (in-hole stxₖ stx)
      κ₀ Σ)
   ex-pop-κ′]

  ;; in eval
  [(InEval s ζ)
   s′ <- (--> s)
   (InEval s′ ζ)
   ex-in-eval]

  ;;;; expression sequences

  ;; (#%seq (d ...) e₀ e ...) ==> (#%seq (d ... (expand e₀)) e ...)
  [(ζ (Stxξ ph (and (Stx (Lst (? id? id_seq)
                              (? Stx? stx′)
                              stx₀ . stl) ctx) stx) ξ scpsₚ)
      κ₀ Σ₀)
   seq? :=<1> (id=? ph id_seq '#%seq Σ₀)
   #:when seq?
   #:checkpoint (printf "ex-seq-car\n")
   (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
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
   kont? :=<1> (id=? ph id_kont '#%kont Σ)
    seq? :=<1> (id=? ph id_seq  '#%seq  Σ)
   snoc? :=<1> (id=? ph id_snoc '#%snoc Σ)
   #:when (and kont? seq? snoc?)
   #:checkpoint (printf "ex-seq-snoc\n")
   (ζ (Stxξ ph (Stx (Lst id_seq
                         (Stx (snoc stl′ stx₀′) (empty-ctx))
                         . stl) ctx) ξ scpsₚ)
      κ Σ)
   ex-seq-snoc]

  ;; (#%seq (d ...)) ==> (d ...)
  [(ζ (Stxξ ph (Stx (Lst (? id? id_seq) (Stx stl′ _ctx′)) ctx) _ξ _scpsₚ)
      κ Σ)
   seq? :=<1> (id=? ph id_seq '#%seq Σ)
   #:when seq?
   #:checkpoint (printf "ex-seq\n")
   (ζ (Stx stl′ ctx)
      κ Σ)
   ex-seq])


(define-unit-from-reduction red@ ==>)

(define-mixed-unit expand@
  (import  domain^ syntax^ env^ store^ eval^
           menv^ mstore^ bind^ parse^)
  (export  expand^)
  (inherit [red@    reducer])

  ;; δ → ζ → (SetM ζ)
  (define (==> δ) (reducer (--> δ) :=)))
