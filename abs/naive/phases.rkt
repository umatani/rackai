#lang racket/base
(require
 racket/unit
 (only-in racket/match                   match)
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"          define-reduction
                                         define-unit-from-reduction
                                         enable-tracing)
 (only-in "../../nondet.rkt"             pure)
 (only-in "../../mix.rkt"                define-mixed-unit inherit)
 (only-in "../../set.rkt"                set ∅ ∅? set-add set→list)
 (only-in "../../syntax.rkt"             stx→datum snoc zip unzip prune at-phase)
 "../../test/suites.rkt"
 "../../base/phases/terms.rkt"

 (only-in "../../mult/phases/units.rkt"  [parse@ mult:parse@] parser@)
 (only-in "../../mult/phases/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "../phases.rkt"                main-minus@)
 (only-in "domain.rkt"                   domain@ val-⊤ atom-⊤ num-⊤ sym-⊤
                                         stx-⊤ list-⊤)
 (only-in "core.rkt"                     eval@))
(provide interp)


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

  #:default [(ζ (Stxξ ph stx ξ scpsₚ) κ Σ) ;; for debug
             (printf "default: ~a\n" (lst→list/recur (stx→datum stx)))]

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

  ;; abstract value
  [(ζ (Stxξ ph val _ξ _scpsₚ) κ Σ)
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
   (import) (export domain^ run^)
   (link domain@ main-minus@
         eval@ parse@ parser@ expand@))
  (import) (export domain^ run^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'finite interp))
