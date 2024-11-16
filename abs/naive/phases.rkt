#lang racket/base
(require
 racket/unit
 (only-in racket/match                   match)
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"          define-reduction
                                         define-unit-from-reduction
                                         enable-tracing)
 (only-in "../../nondet.rkt"             do := <- pure lift enable-checkpoint)
 (only-in "../../mix.rkt"                define-mixed-unit inherit)
 (only-in "../../set.rkt"                set ∅ ∅? set-add set→list)
 (only-in "../../syntax.rkt"             stx→datum snoc zip unzip at-phase)
 "../../test/suites.rkt"
 "../../base/phases/terms.rkt"

 (only-in "../../mult/phases/units.rkt"  [syntax@ mult:syntax@]
                                         [bind@ mult:bind@]
                                         [parse@ mult:parse@] parser@)
 (only-in "../../mult/phases/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "../phases.rkt"                main-minus@)
 (only-in "domain.rkt"                   domain@)
 (only-in "core.rkt"                     eval@))
(provide interp)


;;;; Syntax manipulation

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit (mult:syntax@    empty-ctx in-hole add
                            [mult:flip flip] prune proper-stl?))

  ;; flip : Ph Stx Scp → Stx
  (define (flip ph stx scp)
    (if (eq? stx 'stx-⊤)
      'stx-⊤
      (mult:flip ph stx scp)))
  )


;;;; Name resolution

(define-mixed-unit bind@
  (import
   (only mstore^    lookup-Σ all-nams))
  (export bind^)
  (inherit (mult:bind@    bind [mult:resolve resolve]))

  ;; resolve : Ph Id Σ → (SetM Nam)
  (define (resolve ph id Σ)
    (if (eq? id 'stx-⊤)
      (do nam  <- (lift (all-nams Σ))
          sb   <- (lookup-Σ Σ nam)
          nam′ <- (lift (StoBind-nam sb))
          (pure nam′))
      (mult:resolve ph id Σ)))

  )


;;;; Expander

;; ==> : ζ -> (SetM ζ)
(define-reduction (==> -->) #:super (mult:==> -->)
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only syntax^    empty-ctx add flip prune in-hole proper-stl?)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #;
  [(InEval (list stx '● _sto)
           (ζ (Stxξ ph (Stx (Bool #f) _ctxᵢ) ξ scpsₚ)
              κ Σ))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   #:checkpoint (printf "ex-macapp-abs\n")
   (ζ (Stxξ ph stx ξ scpsₚ)
      κ Σ)
   ex-macapp-abs]

  ;; abstract values
  [(ζ (Stxξ _ph 'stx-⊤ _ξ _scpsₚ) κ Σ)
   #:checkpoint (printf "ex-stx-⊤\n")
   (ζ 'stx-⊤ κ Σ)
   ex-stx-⊤]
  )

(define-unit-from-reduction ex:red@ ==>)

(define-expand-unit expand@ ex:red@)


;;;; Parser

(define-mixed-unit parse@
  (import)
  (export  parse^)
  (inherit (mult:parse@ [mult:parse1 parse1] parse*))

  ; parse1 : Ph Stx Σ -> (SetM Ast)
  (define ((parse1 prs1 prs*) ph stx Σ)
    (if (eq? stx 'stx-⊤)
      (pure 'val-⊤) ;; TODO: ast-⊤?
      ((mult:parse1 prs1 prs*) ph stx Σ)))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define parse (parse1 parse1 parse*)))


;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^)
   (link main-minus@
         domain@ syntax@ bind@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'finite interp))
