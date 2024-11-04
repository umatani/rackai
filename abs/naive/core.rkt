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
 (only-in "../../misc.rkt"             update-store* alloc-loc*)
 (only-in "../../syntax.rkt"           stx→datum snoc zip unzip)
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
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only syntax^    empty-ctx add flip in-hole proper-stl?)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:default [(ζ (Stxξ stx ξ) κ Σ) ;; for debug
             (printf "default: ~a\n" (lst→list/recur (stx→datum stx)))]

  [(InEval (list stx '● _sto)
           (ζ (Stxξ (Stx (Bool #f) (set _scpᵢ)) ξ)
              κ Σ))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   #:checkpoint (printf "ex-macapp-abs\n")
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
   #:checkpoint (printf "ex-abs-⊤\n")
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
  #:import [(only common^    push-cont)
            (only   misc^    lookup-cont lookup-val)
            (only    env^    extend-env* lookup-env)
            (only  store^    lookup-store update-store alloc-loc)]
  ;; β (val-⊤ ...)
  [`(,f ,(KApp′ _args _env loc) ,sto)
   #:when (equal? f val-⊤)
   #:checkpoint (printf "ev-β-abs\n")
   (<- cnt (lookup-cont sto loc))
   `(,f ,cnt ,sto)
   ev-β-abs]

  [`(,(VFun vars ast env) ,(KApp′ args _env loc) ,sto)
   #:checkpoint (printf "ev-β\n")
   (:= `(,(Var nams) ...) vars)
   (:= (values locs sto′) (alloc-loc* alloc-loc nams sto))
   (:= env′               (extend-env* env vars locs))
   (:= sto″               (update-store* update-store sto′ locs args))
   (<- cnt                (lookup-cont sto″ loc))
   `(,(AstEnv ast env′) ,cnt ,sto″)
   ev-β]

  ;; (if ⊤ ...)
  [`(,(? val? val) ,(KIf _ast₁ ast₂ env loc) ,sto)
   #:when (or (equal? val val-⊤) (equal? val atom-⊤))
   #:checkpoint (printf "ev-if-abs-#f\n")
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
