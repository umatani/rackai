#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"        define-reduction
                                       define-unit-from-reduction
                                       enable-tracing)
 (only-in "../../nondet.rkt"           do := <- pure lift results)
 (only-in "../../set.rkt"              set ∅? set→list)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../syntax.rkt"           snoc)
 "../../test/suites.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../mult/core/units.rkt"  [parse@ mult:parse@] parser@)
 (only-in "../../mult/core/eval.rkt"   [--> mult:-->] define-eval-unit)
 (only-in "../../mult/core/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "../core.rkt"                main-minus@)
 (only-in "domain.rkt"                 domain@ val-⊤ atom-⊤ num-⊤ sym-⊤ stx-⊤
                                       list-⊤))
(provide eval@ interp)


;;;; Expander

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (mult:==> -->)
  #:import [(only syntax^    empty-ctx zip unzip add flip in-hole)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only     id^    id=?)
            (only  mcont^    push-κ)
            (only  parse^    parse)]

  [(InEval (list stx '● _sto)
           (ζ (Stxξ (Stx (Bool #f) (set _scpᵢ)) ξ)
              κ Σ))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   (ζ (Stxξ stx ξ)
      κ Σ)
   ex-macapp-abs]

  ;; abstract value
  [(ζ (Stxξ val ξ) κ Σ)
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤)
              (equal? val num-⊤)
              (equal? val sym-⊤)
              (equal? val stx-⊤)
              (equal? val list-⊤))
   (ζ val κ Σ)
   ex-abs-⊤])

(define-unit-from-reduction ex:red@ ==>)

(define-expand-unit expand@ ex:red@)


;;;; Parser

(define-mixed-unit parse@
  (import)
  (export  parse^)
  (inherit (mult:parse@ [mult:parse1 parse1] parse*))

  ; parse1 : Stx Σ -> (SetM Ast)
  (define ((parse1 prs1 prs*) stx Σ)
    (if (or (equal? stx val-⊤)
            (equal? stx atom-⊤)
            (equal? stx stx-⊤))
      (pure val-⊤)
      ((mult:parse1 prs1 prs*) stx Σ)))

  ; parse : Stx Σ -> (SetM Ast)
  (define parse (parse1 parse1 parse*)))


;;;; Evaluator

;; Revise --> to interpret abstract values (val-⊤, stx-⊤, etc.)
;; --> : State -> (Setof State)
(define-reduction (--> δ) #:super (mult:--> δ)
  #:import [(only   env^    extend-env* lookup-env)
            (only store^    update-store* lookup-store alloc-loc*)
            (only  cont^    push-cont)]
  ;; β (val-⊤ ...)
  [`(,f ,(KApp′ _args _env loc) ,sto)
   #:when (equal? f val-⊤)
   (<- cnt (lookup-cont sto loc))
   `(,f ,cnt ,sto)
   ev-β-abs]

  [`(,(VFun vars ast env) ,(KApp′ args _env loc) ,sto)
   (:= `(,(Var nams) ...) vars)
   (:= (values locs sto′) (alloc-loc* nams sto))
   (:= env′               (extend-env* env vars locs))
   (:= sto″               (update-store* sto′ locs args))
   (<- cnt                (lookup-cont sto″ loc))
   `(,(AstEnv ast env′) ,cnt ,sto″)
   ev-β]

  ;; (if ⊤ ...)
  [`(,(? val? val) ,(KIf _ast₁ ast₂ env loc) ,sto)
   #:when (or (equal? val val-⊤) (equal? val atom-⊤))
   (<- cnt (lookup-cont sto loc))
   `(,(AstEnv ast₂ env) ,cnt ,sto)
   ev-if-abs-#f])

(define-unit-from-reduction ev:red@ -->)

(define-eval-unit eval@ ev:red@)


;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link  main-minus@
          domain@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'finite interp))
