#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"        define-reduction
                                       define-unit-from-reduction
                                       enable-tracing)
 (only-in "../../nondet.rkt"           do := <- pure enable-checkpoint)
 (only-in "../../set.rkt"              set ∅? set→list)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../misc.rkt"             update-store* alloc-loc*)
 (only-in "../../syntax.rkt"           stx→datum snoc zip unzip)
 "../../test/suites.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../mult/core/units.rkt"  [syntax@ mult:syntax@]
                                       [parse@ mult:parse@] parser@)
 (only-in "../../mult/core/eval.rkt"   [--> mult:-->] define-eval-unit)
 (only-in "../../mult/core/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "../core.rkt"                main-minus@)
 (only-in "domain.rkt"                 domain@))
(provide eval@ interp)


;;;; Syntax manipulation

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit (mult:syntax@ empty-ctx in-hole add [mult:flip flip] proper-stl?))

  ;; flip : Stx Scp → Stx
  (define (flip stx scp)
    (if (eq? stx 'stx-⊤)
      'stx-⊤
      (mult:flip stx scp))))


;;;; Expander

;; ==> : ζ -> (SetM ζ)
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

  ;; abstract values
  [(ζ (Stxξ 'stx-⊤ ξ) κ Σ)
   #:checkpoint (printf "ex-stx-⊤\n")
   (ζ 'stx-⊤ κ Σ)
   ex-stx-⊤])

(define-unit-from-reduction ex:red@ ==>)

(define-expand-unit expand@ ex:red@)


;;;; Parser

(define-mixed-unit parse@
  (import)
  (export  parse^)
  (inherit (mult:parse@ [mult:parse1 parse1] parse*))

  ; parse1 : Stx Σ -> (SetM Ast)
  (define ((parse1 prs1 prs*) stx Σ)
    (if (eq? stx 'stx-⊤)
      (pure 'val-⊤)
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
  [`(val-⊤ ,(KApp′ _args _env loc) ,sto)
   #:checkpoint (printf "ev-β-abs\n")
   cnt <- (lookup-cont sto loc)
   `(val-⊤ ,cnt ,sto)
   ev-β-abs]


  ;; (if ⊤ ...)
  #;
  [`(,(? val? val) ,(KIf _ast₁ ast₂ env loc) ,sto)
   #:when (or (equal? val val-⊤) (equal? val atom-⊤))
   #:checkpoint (printf "ev-if-abs-#f\n")
   cnt <- (lookup-cont sto loc)
   `(,(AstEnv ast₂ env) ,cnt ,sto)
   ev-if-abs-#f]
  )

(define-unit-from-reduction ev:red@ -->)

(define-eval-unit eval@ ev:red@)


;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^)
   (link  main-minus@
          domain@ syntax@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'finite interp))
