#lang racket/base
(require
 racket/unit
 (only-in racket/match                  match)
 "../../interpreter.rkt"
 "../../test/suites.rkt"
 (only-in "../../mix.rkt"               define-mixed-unit inherit)
 (only-in "../../misc.rkt"              union)
 (only-in "../../set.rkt"               set ∅ ∅? set→list)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/phases/terms.rkt"

 (only-in "../../mult/phases/units.rkt" expand/red@ [parse@ mult:parse@] parser@)
 (only-in "../core.rkt"                 eval/red@)
 (only-in "../phases.rkt"               [==> abs:==>] main-minus@)
 (only-in "domain.rkt"                  domain@ val-⊤ atom-⊤ num-⊤ sym-⊤
                                        stx-⊤ list-⊤)
 (only-in "core.rkt"                    ev:red@))
(provide interp)


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


;;;; Expander

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (abs:==> -->)
  #:within-signatures [(only syntax^
                             empty-ctx zip unzip add flip in-hole
                             prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name alloc-scope)
                       (only  bind^    bind resolve)
                       (only    id^    id=?)
                       (only mcont^    push-κ)
                       (only parse^    parse)]

  [(InEval (list stx_exp '● store_0)
           (ζ (Stxξ ph (Stx (Bool #f) ctx_i) ξ scps_p) '◯ κ0 Σ))
   #:when (or (equal? stx_exp val-⊤)
              (equal? stx_exp atom-⊤)
              (equal? stx_exp stx-⊤))
   (ζ (Stxξ ph stx_exp ξ scps_p) '◯ κ0 Σ)
   ex-macapp-flip-abs]

  ;; abstract value
  [(ζ (Stxξ ph val _ _) '◯ κ0 Σ)
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤)
              (equal? val num-⊤)
              (equal? val sym-⊤)
              (equal? val stx-⊤)
              (equal? val list-⊤))
   (ζ val '● κ0 Σ)
   ex-abs-⊤])

(define-unit-from-reduction ex:red@ ==>)

;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         (() eval/red@ ev)   (([ev : red^]) ev:red@)
         parse@ parser@
         (() expand/red@ ex) (([ex : red^]) ex:red@)))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'finite interp))
