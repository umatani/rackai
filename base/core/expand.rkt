#lang racket/base
(require
 racket/unit
 (only-in racket/match       match match-let)
 (only-in "../../set.rkt"    set)
 (only-in "../../mix.rkt"    define-mixed-unit inherit)
 (only-in "../../syntax.rkt" stx→datum snoc zip unzip)
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
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only domain^    val? stx?)
            (only syntax^    empty-ctx in-hole add flip proper-stl?)
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

        ;; id=? : Id Nam Σ → Boolean
        (define (id=? id nam Σ)
          (eq? (resolve id Σ) nam))]

  #:default [(ζ (Stxξ stx ξ) κ Σ) ;; for debug
             #:abort-if (id? stx) (format "expand: unbound identifier: ~a\n"
                                          (Sym-nam (Stx-e stx)))
             (printf "expand: unknown form ~a\n"
                     (lst→list/recur (stx→datum stx)))]

  ;; lambda
  [(ζ (Stxξ (and (Stx (Lst (? id? id_lam)
                           (Stx (? proper-stl? stl_params) ctx_params)
                           stx_body) ctx) stx) ξ)
      κ₀ Σ₀)
   (:=<1> lambda? (id=? id_lam 'lambda Σ₀))
   #:when lambda?
   #:checkpoint (printf "ex-lam\n")
   (:= (values scp Σ₁)            (alloc-scope 'lam Σ₀))
   (:= (values stl_params′ ξ′ Σ₂) (regist-vars scp stl_params ξ Σ₁))
   (:= (values 𝓁 Σ₃)              (push-κ Σ₂ stx κ₀))
   (ζ (Stxξ (add stx_body scp) ξ′)
      (κ (Stx (Lst id_lam (Stx stl_params′ ctx_params)
                   (Hole)) ctx) 𝓁) Σ₃)
   ex-lam]

  ;; let
  [(ζ (Stxξ (and (Stx (Lst (? id? id_let)
                           (Stx (? proper-stl? stl_binds) ctx_binds)
                           stx_body) ctx) stx) ξ)
      κ₀ Σ₀)
   (:=<1> let? (id=? id_let 'let Σ₀))
   #:when let?
   #:checkpoint (printf "ex-let-body\n")
   (:= (values stl_vars stl_rhs) (unzip stl_binds))
   (:= (values scp Σ₁)           (alloc-scope 'let Σ₀))
   (:= (values stl_vars′ ξ′ Σ₂)  (regist-vars scp stl_vars ξ Σ₁))
   (:= (values 𝓁 Σ₃)             (push-κ Σ₂ stx κ₀))
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
   (:=<1> kont? (id=? id_kont '#%kont Σ₀))
   (:=<1> let?  (id=? id_let  'let    Σ₀))
   #:when (and kont? let?)
   #:checkpoint (printf "ex-let-rhs\n")
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
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
   (:=<1> kont?  (id=? id_kont  '#%kont Σ))
   (:=<1> kont′? (id=? id_kont′ '#%kont Σ))
   (:=<1> let?   (id=? id_let   'let    Σ))
   #:when (and kont? kont′? let?)
   #:checkpoint (printf "ex-let\n")
   (ζ (Stx (Lst id_let (Stx (zip stl_vars′ stl_rhs′ (empty-ctx)) ctx_binds)
                stx_body′) ctx)
      κ Σ)
   ex-let]

  ;; quote
  [(ζ (Stxξ (and (Stx (Lst (? id? id_quote) _) _ctx) stx) _ξ)
      κ Σ)
   (:=<1> quote? (id=? id_quote 'quote Σ))
   #:when quote?
   #:checkpoint (printf "ex-quote\n")
   (ζ stx
      κ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ (and (Stx (Lst (? id? id_syntax) _) _ctx) stx) _ξ)
      κ Σ)
   (:=<1> syntax? (id=? id_syntax 'syntax Σ))
   #:when syntax?
   #:checkpoint (printf "ex-stx\n")
   (ζ stx
      κ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ (and (Stx (Lst (? id? id_ls)
                           (Stx (Lst (Stx (Lst (? id? id) stx_rhs) ctx_bind))
                                ctx_binds)
                           stx_body) ctx) stx) ξ)
      κ₀ Σ₀)
   (:=<1> let-syntax? (id=? id_ls 'let-syntax Σ₀))
   #:when let-syntax?
   #:checkpoint (printf "ex-ls-rhs\n")
   (:= (values nam Σ₁) (alloc-name   id Σ₀))
   (:= (values scp Σ₂) (alloc-scope 'ls Σ₁))
   (:= id′             (add id scp))
   (:= Σ₃              (bind Σ₂ id′ nam))
   (:= (values 𝓁 Σ₄)   (push-κ Σ₃ stx κ₀))
   (:= stx_body′       (add stx_body scp))
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
   (:=<1> kont?       (id=? id_kont  '#%kont     Σ))
   (:=<1> let-syntax? (id=? id_ls    'let-syntax Σ))
   #:when (and kont? let-syntax?)
   #:checkpoint (printf "ex-ls-eval\n")
   (<- ast (parse stx_rhs′ Σ))
   (InEval (list (AstEnv ast (init-env)) '● (init-store))
           (ζ (Stxξ (Stx (Lst id′ stx_body′) (empty-ctx)) ξ)
              κ Σ))
   ex-ls-eval]

  [(InEval (list (? val? val) '● _sto)
           (ζ (Stxξ (Stx (Lst (? id? id′) stx_body′) _ctx) ξ)
              κ Σ))
   #:checkpoint (printf "ex-ls\n")
   (:=<1> nam (resolve id′ Σ))
   (:=    ξ′  (extend-ξ ξ nam val))
   (ζ (Stxξ stx_body′ ξ′)
      κ Σ)
   ex-ls]

  ;; macro invocation
  [(ζ (Stxξ (and (Stx (Lst (? id? id) _stx ...) ctx) stx) ξ)
      κ Σ₀)
   (:=<1> nam (resolve id Σ₀))
   (:=<1> val (lookup-ξ ξ nam))
   #:when (val? val)
   #:checkpoint (printf "ex-macapp-eval\n")
   (:= (values scpᵤ Σ₁) (alloc-scope 'u Σ₀))
   (:= (values scpᵢ Σ₂) (alloc-scope 'i Σ₁))
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
   #:checkpoint (printf "ex-macapp\n")
   (ζ (Stxξ (flip stx scpᵢ) ξ)
      κ Σ)
   ex-macapp]

  ;; if
  [(ζ (Stxξ (and (Stx (Lst (? id? id_if) . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   (:=<1> if? (id=? id_if 'if  Σ₀))
   #:when if?
   #:checkpoint (printf "ex-if-seq\n")
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (Stx (Lst id-seq stx-nil . stl) ctx) ξ)
      (κ (Stxξ (Stx (Lst id-kont id_if (Hole)) (empty-ctx)) ξ) 𝓁) Σ₁)
   ex-if-seq]

  [(ζ (Stxξ (Stx (Lst (? id? id_kont) (? id? id_if)
                      (Stx (? proper-stl? stl′) ctx)) _ctx) _ξ)
      κ Σ)
   (:=<1> kont? (id=? id_kont '#%kont Σ))
   (:=<1> if?   (id=? id_if   'if     Σ))
   #:when (and kont? if?)
   #:checkpoint (printf "ex-if\n")
   (ζ (Stx (Lst id_if . stl′) ctx)
      κ Σ)
   ex-if]

  ;; application (canonical #%app version)
  [(ζ (Stxξ (and (Stx (Pair (? id? id_app)
                            (Stx (Lst stx_f . stl) ctx_seq)) ctx) stx) ξ)
      κ₀ Σ₀)
   (:=<1> app? (id=? id_app '#%app Σ₀))
   #:when app?
   #:checkpoint (printf "ex-#%app\n")
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx_seq) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ (and (Stx (Lst (? id? id_app) stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   (:=<1> app? (id=? id_app '#%app Σ₀))
   #:when app?
   #:checkpoint (printf "ex-#%app′\n")
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-#%app′]

  ;; application (lambda or primitive)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (not (id? stx_f))
   #:checkpoint (printf "ex-app\n")
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app]

  ;; application (bound var ref)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   (:=<1> nam (resolve stx_f Σ₀))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (TVar? at)
   #:checkpoint (printf "ex-app-bound\n")
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-bound]

  ;; application (free var ref)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   #:when (id? stx_f)
   (:=<1> nam (resolve stx_f Σ₀))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (and (eq? 'not-found at)
               (not (member nam '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%snoc))))
   #:checkpoint (printf "ex-app-free\n")
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ (? id? id) ξ)
      κ Σ)
   (:=<1> nam (resolve id Σ))
   (:=<1> at  (lookup-ξ ξ nam))
   #:when (TVar? at)
   #:checkpoint (printf "ex-var\n")
   (ζ (TVar-id at)
      κ Σ)
   ex-var]
  
  ;; literal
  [(ζ (Stxξ (and (Stx (? Atom? atom) ctx) stx) _ξ)
      κ Σ)
   #:when (not (Sym? atom))
   #:checkpoint (printf "ex-lit\n")
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-lit]

  ;; primitive operator
  [(ζ (Stxξ (and (Stx (? prim?) ctx) stx) _ξ)
      κ Σ)
   #:checkpoint (printf "ex-prim\n")
   (ζ (Stx (Lst (Stx (Sym 'quote) ctx) stx) ctx)
      κ Σ)
   ex-prim]

  ;; pop κ
  [(ζ (? Stx? stx)
      (κ (Stxξ stxₖ ξ) 𝓁) Σ)
   (:=<1> κ₀ (lookup-κ Σ 𝓁))
   (ζ (Stxξ (in-hole stxₖ stx) ξ)
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
  [(ζ (Stxξ (and (Stx (Lst (? id? id_seq)
                           (? Stx? stx′)
                           stx₀ . stl) ctx) stx) ξ)
      κ₀ Σ₀)
   (:=<1> seq? (id=? id_seq '#%seq Σ₀))
   #:when seq?
   #:checkpoint (printf "ex-seq-car\n")
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
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
   (:=<1> kont? (id=? id_kont '#%kont Σ))
   (:=<1> seq?  (id=? id_seq  '#%seq  Σ))
   (:=<1> snoc? (id=? id_snoc '#%snoc Σ))
   #:when (and kont? seq? snoc?)
   #:checkpoint (printf "ex-seq-snoc\n")
   (ζ (Stxξ (Stx (Lst id_seq
                      (Stx (snoc stl′ stx₀′) (empty-ctx))
                      . stl) ctx) ξ)
      κ Σ)
   ex-seq-snoc]
  
  ;; (#%seq (d ...)) ==> (d ...)
  [(ζ (Stxξ (Stx (Lst (? id? id_seq) (Stx stl′ _ctx′)) ctx) _ξ)
      κ Σ)
   (:=<1> seq? (id=? id_seq  '#%seq  Σ))
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

  ;; δ → ζ → (Setof ζ)
  (define (==> δ) (reducer (--> δ) :=)))
