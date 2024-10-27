#lang racket/base
(require
 racket/unit
 (only-in racket/match                   match)
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"          define-reduction
                                         define-unit-from-reduction
                                         enable-tracing)
 (only-in "../../nondet.rkt"             := <- pure lift results)
 (only-in "../../mix.rkt"                define-mixed-unit inherit)
 (only-in "../../misc.rkt"               union)
 (only-in "../../set.rkt"                set ∅ ∅? set-add set→list)
 (only-in "../../syntax.rkt"             snoc)
 "../../test/suites.rkt"
 "../../base/phases/terms.rkt"

 (only-in "../../mult/phases/units.rkt"  [parse@ mult:parse@] parser@)
 (only-in "../../mult/phases/expand.rkt" define-expand-unit)
 (only-in "../phases.rkt"                [==> abs:==>] main-minus@)
 (only-in "domain.rkt"                   domain@ val-⊤ atom-⊤ num-⊤ sym-⊤
                                         stx-⊤ list-⊤)
 (only-in "core.rkt"                     eval@))
(provide interp)


;;;; Expander

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (abs:==> -->)
  #:import [(only syntax^    empty-ctx zip unzip add flip in-hole prune at-phase)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only     id^    id=?)
            (only  mcont^    push-κ)
            (only  parse^    parse)]

  [(InEval (list stx '● _sto)
           (ζ (Stxξ ph (Stx (Bool #f) _ctxᵢ) ξ scpsₚ)
              κ Σ))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   (ζ (Stxξ ph stx ξ scpsₚ)
      κ Σ)
   ex-macapp-abs]

  ;; abstract value
  [(ζ (Stxξ ph val _ξ _scpsₚ) κ Σ)
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

  ; parse1 : Ph Stx Σ -> (SetM Ast)
  (define ((parse1 prs1 prs*) ph stx Σ)
    (if (or (equal? stx val-⊤)
            (equal? stx atom-⊤)
            (equal? stx stx-⊤))
      (pure val-⊤)
      ((mult:parse1 prs1 prs*) ph stx Σ)))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define parse (parse1 parse1 parse*)))


;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         eval@ parse@ parser@ expand@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'finite interp))
