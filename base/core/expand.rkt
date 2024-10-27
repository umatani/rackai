#lang racket/base
(require
 racket/unit
 (only-in racket/match       match match-let)
 (only-in "../../set.rkt"    set)
 (only-in "../../mix.rkt"    define-mixed-unit inherit)
 (only-in "../../syntax.rkt" snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt")
(provide ==> expand@)

;; ----------------------------------------
;; The expander:
;;   ζ ∷= ⟨⟨Stx, ξ⟩ ∪ Stx, κ, Σ⟩
;;   κ ∷= ⟨⟨Stx, ξ⟩ ∪ Stx, 𝓁⟩

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
        (define (regist-vars scp ids₀ ξ₀ Σ₀)
          (match ids₀
            [(Null)
             (values (Null) ξ₀ Σ₀)]
            [(Pair id ids)
             (let*-values ([(ids′ ξ₁ Σ₁) (regist-vars scp ids ξ₀ Σ₀)]
                           [(nam Σ₂)     (alloc-name id Σ₁)]
                           [(id′)        (add id scp)]
                           [(Σ₃)         (bind Σ₂ id′ nam)]
                           [(ξ₂)         (extend-ξ ξ₁ nam (TVar id′))])
               (values (Pair id′ ids′) ξ₂ Σ₃))]))]

  ;; lambda
  [(ζ (Stxξ (and (Stx (Lst (? id? id_lam)
                           (Stx (? proper-stl? stl_params) ctx_params)
                           stx_body) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id=? id_lam 'lambda Σ₀)
   #:with            (values scp Σ₁) := (alloc-scope 'lam Σ₀)
   #:with (values stl_params′ ξ′ Σ₂) := (regist-vars scp stl_params ξ Σ₁)
   #:with              (values 𝓁 Σ₃) := (push-κ Σ₂ stx κ₀)
   (ζ (Stxξ (add stx_body scp) ξ′)
      (κ (Stx (Lst id_lam (Stx stl_params′ ctx_params)
                   (Hole)) ctx) 𝓁) Σ₃)
   ex-lam]

  ;; let
  [(ζ (Stxξ (and (Stx (Lst (? id? id_let)
                           (Stx (? proper-stl? stl_binds) ctx_binds)
                           stx_body) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id=? id_let 'let Σ₀)
   #:with (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with           (values scp Σ₁) := (alloc-scope 'let Σ₀)
   #:with  (values stl_vars′ ξ′ Σ₂) := (regist-vars scp stl_vars ξ Σ₁)
   #:with             (values 𝓁 Σ₃) := (push-κ Σ₂ stx κ₀)
   (ζ (Stxξ (add stx_body scp) ξ′)
      (κ (Stxξ (Stx (Lst id-kont id_let
                         (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                   (Stx stl_rhs   (empty-ctx)))
                              ctx_binds)
                         (Hole)) ctx) ξ) 𝓁) Σ₃)
   ex-let-body]

  [(ζ (Stxξ (and (Stx (Lst (? id? id_kont) (? id? id_let)
                           (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                     (Stx (? proper-stl? stl_rhs  ) _))
                                ctx_binds)
                           stx_body′) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (and (id=? id_kont '#%kont Σ₀)
               (id=? id_let  'let    Σ₀))
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil . stl_rhs) ctx_binds) ξ)
      (κ (Stxξ (Stx (Lst id_kont id_kont id_let
                         (Stx (Lst (Stx stl_vars′ (empty-ctx))
                                   (Hole))
                              ctx_binds)
                         stx_body′) ctx) ξ) 𝓁) Σ₁)
   ex-let-rhs]

  [(ζ (Stxξ (Stx (Lst (? id? id_kont) (? id? id_kont′) (? id? id_let)
                      (Stx (Lst (Stx (? proper-stl? stl_vars′) _)
                                (Stx (? proper-stl? stl_rhs′ ) _))
                           ctx_binds)
                      stx_body′) ctx) _ξ)
      κ Σ)
   #:when (and (id=? id_kont  '#%kont Σ)
               (id=? id_kont′ '#%kont Σ)
               (id=? id_let   'let    Σ))
   (ζ (Stx (Lst id_let (Stx (zip stl_vars′ stl_rhs′ (empty-ctx)) ctx_binds)
                stx_body′) ctx)
      κ Σ)
   ex-let]

  ;; quote
  [(ζ (Stxξ (and (Stx (Lst (? id? id_quote) _) _ctx) stx) _ξ)
      κ Σ)
   #:when (id=? id_quote 'quote Σ)
   (ζ stx
      κ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ (and (Stx (Lst (? id? id_syntax) _) _ctx) stx) _ξ)
      κ Σ)
   #:when (id=? id_syntax 'syntax Σ)
   (ζ stx
      κ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ (and (Stx (Lst (? id? id_ls)
                           (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_bind))
                                ctx_binds)
                           stx_body) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id=? id_ls 'let-syntax Σ₀)
   #:with (values nam Σ₁) := (alloc-name   id Σ₀)
   #:with (values scp Σ₂) := (alloc-scope 'ls Σ₁)
   #:with             id′ := (add id scp)
   #:with              Σ₃ := (bind Σ₂ id′ nam)
   #:with   (values 𝓁 Σ₄) := (push-κ Σ₃ stx κ₀)
   #:with       stx_body′ := (add stx_body scp)
   (ζ (Stxξ stx_rhs (init-ξ))
      (κ (Stxξ (Stx (Lst id-kont id_ls
                         (Stx (Lst (Stx (Lst id′ (Hole)) ctx_bind))
                              ctx_binds)
                         stx_body′) ctx) ξ) 𝓁) Σ₄)
   ex-ls-rhs]

  [(ζ (Stxξ (Stx (Lst (? id? id_kont) (? id? id_ls)
                      (Stx (Lst (Stx (Lst (? id? id′) stx_rhs′) _ctx_bind))
                           _ctx_binds)
                      stx_body′) ctx) ξ)
      κ Σ)
   #:when (and (id=? id_kont '#%kont     Σ)
               (id=? id_ls   'let-syntax Σ))
   #:with ast <- (parse stx_rhs′ Σ)
   (InEval (list (AstEnv ast (init-env)) '● (init-store))
           (ζ (Stxξ (Stx (Lst id′ stx_body′) (empty-ctx)) ξ)
              κ Σ))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _sto)
           (ζ (Stxξ (Stx (Lst (? id? id′) stx_body′) _ctx) ξ)
              κ Σ))
   #:with nam :=<1> (resolve id′ Σ)
   #:with  ξ′ :=    (extend-ξ ξ nam val)
   (ζ (Stxξ stx_body′ ξ′)
      κ Σ)
   ex-ls]

  ;; macro invocation
  [(ζ (Stxξ (and (Stx (Lst (? id? id) _stx ...) ctx) stx) ξ)
      κ Σ₀)
   #:with nam              :=<1> (resolve id Σ₀)
   #:with val              :=<1> (lookup-ξ ξ nam)
   #:when (val? val)
   #:with (values scpᵤ Σ₁) :=    (alloc-scope 'u Σ₀)
   #:with (values scpᵢ Σ₂) :=    (alloc-scope 'i Σ₁)
   (InEval
    (list (AstEnv (App (gensym 'macapp)  ;; TODO: OK?
                       val
                       (list (flip (add stx scpᵤ) scpᵢ)))
                  (init-env))
          '● (init-store))
    (ζ (Stxξ (Stx (Bool #f) (set scpᵢ)) ξ)
       κ Σ₂))
   ex-macapp-eval]

  [(InEval (list (? stx? stx) '● _sto)
           (ζ (Stxξ (Stx (Bool #f) (set scpᵢ)) ξ)
              κ Σ))
   (ζ (Stxξ (flip stx scpᵢ) ξ)
      κ Σ)
   ex-macapp]

  ;; if
  [(ζ (Stxξ (and (Stx (Lst (? id? id_if) . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id=? id_if 'if Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil . stl) ctx) ξ)
      (κ (Stxξ (Stx (Lst id-kont id_if (Hole)) (empty-ctx)) ξ) 𝓁) Σ₁)
   ex-if-seq]

  [(ζ (Stxξ (Stx (Lst (? id? id_kont) (? id? id_if)
                      (Stx (? proper-stl? stl′) ctx)) _ctx) _ξ)
      κ Σ)
   #:when (and (id=? id_kont '#%kont Σ)
               (id=? id_if   'if     Σ))
   (ζ (Stx (Lst id_if . stl′) ctx)
      κ Σ)
   ex-if]

  ;; application (canonical #%app version)
  [(ζ (Stxξ (and (Stx (Pair (? id? id_app)
                            (Stx (Lst stx_f . stl) ctx_seq)) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id=? id_app '#%app Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx_seq) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ (and (Stx (Lst (? id? id_app) stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id=? id_app '#%app Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app′]

  ;; application (lambda or primitive)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (not (id? stx_f))
   #:with        id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app]

  ;; application (bound var ref)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   #:with           nam :=<1> (resolve stx_f Σ₀)
   #:with            at :=<1> (lookup-ξ ξ nam)
   #:when (TVar? at)
   #:with        id_app :=    (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) :=    (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-bound]

  ;; application (free var ref)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   #:with           nam :=<1> (resolve stx_f Σ₀)
   #:with            at :=<1> (lookup-ξ ξ nam)
   #:when (and (eq? 'not-found at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:with        id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ (? id? id) ξ)
      κ Σ)
   #:with nam :=<1> (resolve id Σ)
   #:with  at :=<1> (lookup-ξ ξ nam)
   (match at
     [(TVar id′) (ζ id′ κ Σ)]
     [_ (error '==> "unbound identifier: ~a" nam)])
   ex-var]
  
  ;; literal
  [(ζ (Stxξ (and (Stx (? Atom? atom) ctx) stx) _ξ)
      κ Σ)
   #:when (not (Sym? atom))
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ (and (Stx (? prim?) ctx) stx) _ξ)
      κ Σ)
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-prim]

  ;; pop κ
  [(ζ (? Stx? stx)
      (κ (Stxξ stxₖ ξ) 𝓁) Σ)
   #:with κ₀ :=<1> (lookup-Σ Σ 𝓁)
   (ζ (Stxξ (in-hole stxₖ stx) ξ)
      κ₀ Σ)
   ex-pop-κ]

  ;; pop κ′
  [(ζ (? Stx? stx)
      (κ (? (compose1 not Stxξ?) stxₖ) 𝓁) Σ)
   #:with κ₀ :=<1> (lookup-Σ Σ 𝓁)
   (ζ (in-hole stxₖ stx)
      κ₀ Σ)
   ex-pop-κ′]

  ;; in eval
  [(InEval s ζ)
   #:with s′ <- (lift (--> s))
   (InEval s′ ζ)
   ex-in-eval]

  ;;;; expression sequences

  ;; (#%seq (d ...) e₀ e ...) ==> (#%seq (d ... (expand e₀)) e ...)
  [(ζ (Stxξ (and (Stx (Lst (? id? id_seq)
                           (? Stx? stx′)
                           stx₀ . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id=? id_seq '#%seq Σ₀)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ stx₀ ξ)
      (κ (Stxξ (Stx (Lst id-kont id_seq
                         (Stx (Lst id-snoc stx′ (Hole)) (empty-ctx))
                         . stl) ctx) ξ) 𝓁) Σ₁)
   ex-seq-car]

  [(ζ (Stxξ (Stx (Lst (? id? id_kont) (? id? id_seq)
                      (Stx (Lst (? id? id_snoc)
                                (Stx stl′ _ctx′) (? stx? stx₀′)) _ctx)
                      . stl) ctx) ξ)
      κ Σ)
   #:when (and (id=? id_kont '#%kont Σ)
               (id=? id_seq  '#%seq  Σ)
               (id=? id_snoc '#%snoc Σ))
   (ζ (Stxξ (Stx (Lst id_seq
                      (Stx (snoc stl′ stx₀′) (empty-ctx))
                      . stl) ctx) ξ)
      κ Σ)
   ex-seq-snoc]
  
  ;; (#%seq (d ...)) ==> (d ...)
  [(ζ (Stxξ (Stx (Lst (? id? id_seq) (Stx stl′ _ctx′)) ctx) _ξ)
      κ Σ)
   #:when (id=? id_seq '#%seq Σ)
   (ζ (Stx stl′ ctx)
      κ Σ)
   ex-seql])


(define-unit-from-reduction red@ ==>)

(define-mixed-unit expand@
  (import  domain^ syntax^ env^ store^ eval^
           menv^ mstore^ mcont^ bind^ id^ parse^)
  (export  expand^)
  (inherit [red@    reducer])

  ;; δ → ζ → (Setof ζ)
  (define (==> δ) (reducer (--> δ) :=)))
