#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match match-λ**)
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"        define-reduction
                                       define-unit-from-reduction
                                       enable-tracing)
 (only-in "../../nondet.rkt"           pure)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../misc.rkt"             update-store* alloc-loc*)
 (only-in "../../set.rkt"              set ∅ set-add for/set)
 (only-in "../../syntax.rkt"           stx→datum snoc zip unzip prune at-phase)
 "../../test/suites.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../mult/full/units.rkt"  [parse@ mult:parse@] parser@)
 (only-in "../../mult/full/eval.rkt"   [--> mult:-->] define-eval-unit)
 (only-in "../../mult/full/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "../full.rkt"                main-minus@)
 (only-in "domain.rkt"                 domain@ val-⊤ atom-⊤ num-⊤ sym-⊤
                                       stx-⊤ list-⊤))
(provide interp)


;;;; Expander

(define-reduction (==> -->) #:super (mult:==> -->)
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only syntax^    empty-ctx add flip in-hole proper-stl?)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:default [(ζ (Stxξ ph stx ξ) κ Σ̂) ;; for debug
             (printf "default: ~a\n" (lst→list/recur (stx→datum stx)))]
  
  [(InEval (list stx '● _sto Σ̂)
           (ζ (Stxξ ph (Stx (Bool #f) _ctxᵢ) ξ)
              κ
              _Σ̂))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   #:checkpoint (printf "ex-macapp-abs\n")
   (ζ (Stxξ ph stx ξ)
      κ
      Σ̂)
   ex-macapp-abs]

  ;; abstract value
  [(ζ (Stxξ ph val ξ)
      κ
      Σ̂)
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤)
              (equal? val num-⊤)
              (equal? val sym-⊤)
              (equal? val stx-⊤)
              (equal? val list-⊤))
   #:checkpoint (printf "ex-abs-⊤\n")
   (ζ val
      κ
      Σ̂)
   ex-abs-⊤])

(define-unit-from-reduction ex:red@ ==>)

(define-expand-unit expand@ ex:red@)


;;;; Parser

(define-mixed-unit parse@
  (import)
  (export  parse^)
  (inherit (mult:parse@ [mult:parse1 parse1] parse*))

  ; parse1 : Ph Stx Σ -> (SetM Ast)
  (define ((parse1 prs1 prs*) ph stx Σ)
    (if (or (equal? stx val-⊤)
            (equal? stx atom-⊤)
            (equal? stx stx-⊤))
      (pure val-⊤)
      ((mult:parse1 prs1 prs*) ph stx Σ)))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define parse (parse1 parse1 parse*)))


;;;; Evaluator

(define-reduction (--> δ ==>) #:super (mult:--> δ ==>)
  #:import [(only common^    push-cont)
            (only   misc^    lookup-cont lookup-val)
            (only syntax^    add flip)
            (only    env^    init-env lookup-env extend-env*)
            (only  store^    lookup-store update-store alloc-loc)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ update-Σ alloc-name alloc-scope alloc-𝓁)
            (only   bind^    bind resolve)
            (only  parse^    parse)]
  ;; (syntax-local-value <abs> _ ...)
  [`(,(Prim 'syntax-local-value _stx)
     ,(KApp′ `(,(? id? id) ,_val ...) `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (equal? id val-⊤) (equal? id atom-⊤)
              (equal? id stx-⊤))
   #:checkpoint (printf "ev-lval-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,val-⊤ ,cnt ,sto ,Σ̂)
   ev-lval-abs]

  ;; (syntax-local-identifier-as-binding <abs>)
  [`(,(Prim 'syntax-local-identifier-as-binding _stx)
     ,(KApp′ `(,(? id? id)) `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (equal? id val-⊤)
              (equal? id atom-⊤)
              (equal? id stx-⊤)
              (and (Stx? id) (equal? (Stx-e id) sym-⊤)))
   #:checkpoint (printf "ev-lbinder-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,stx-⊤ ,cnt ,sto ,Σ̂)
   ev-lbinder-abs]

  ;; (syntax-local-bind-syntaxes <abs> <abs> <abs>)
  [`(,(Prim 'syntax-local-bind-syntaxes _stx)
     ,(KApp′ `(,ids ,rhs ,defs)
             `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (or (equal? ids list-⊤)
                  (and (Pair? ids) (Null? (Pair-d ids))
                       (let ([id (Pair-a ids)])
                         (or (equal? id val-⊤)
                             (equal? id atom-⊤)
                             (equal? id stx-⊤)
                             (and (Stx? id) (equal? (Stx-e id) sym-⊤))))))
              (or (equal? rhs (Bool #f))
                  (equal? rhs val-⊤)
                  (equal? rhs atom-⊤)
                  (equal? rhs stx-⊤))
              (or (equal? defs val-⊤)
                  (equal? defs atom-⊤)))
   #:checkpoint (printf "ev-slbs-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,list-⊤ ,cnt ,sto ,Σ̂)
   ev-slbs-abs]

  ;; (local-expand <abs> contextv idstops defs?) ;; TODO: check other args
  [`(,(Prim 'local-expand stx)
     ,(KApp′ `(,stx_arg ,_val_context ,_ids_stop ,_defs ...)
             `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (equal? stx_arg val-⊤)
              (equal? stx_arg atom-⊤)
              (equal? stx_arg stx-⊤))
   #:checkpoint (printf "ev-lexpand-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,stx-⊤ ,cnt ,sto ,Σ̂)
   ev-lexpand-abs]

  ;; β (<abs> ...)
  [`(,f
     ,(KApp′ _args `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (equal? f val-⊤)
   #:checkpoint (printf "ev-β-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,f ,cnt ,sto ,Σ̂)
   ev-β-abs]

  ;; (if <abs> ...)
  [`(,(? val? val)
     ,(KIf _ast₁ ast₂ (list ph env maybe-scpᵢ ξ) loc)
     ,sto ,Σ̂)   
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤))
   #:checkpoint (printf "ev-if-abs-#f\n")
   cnt <- (lookup-cont sto loc)
   `(,(AstEnv ph ast₂ env maybe-scpᵢ ξ)
     ,cnt
     ,sto ,Σ̂)
   ev-if-abs-#f])

(define-unit-from-reduction ev:red@ -->)

(define-eval-unit eval@ ev:red@)


;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^)
   (link main-minus@ expand@ parse@ parser@ domain@ eval@))
  (import) (export domain^ run^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'full   interp)
  (run-suite 'finite interp))
