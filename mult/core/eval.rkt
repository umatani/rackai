#lang racket/base
(require
 racket/unit
 "../../reduction.rkt"
 (only-in "../../mix.rkt"            define-mixed-unit inherit)
 (only-in "../../misc.rkt"           update-store* alloc-loc*)
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../base/core/eval.rkt" [--> base:-->]))
(provide --> define-eval-unit eval@)

;; Revised reduction rules

;; --> : State → (Setof State)
(define-reduction (--> δ) #:super (base:--> δ <-)
  #:import [(only common^    push-cont)
            (only domain^    val?)
            (only    env^    lookup-env extend-env*)
            (only  store^    lookup-store update-store
                             alloc-loc lookup-cont lookup-val)])

(define-unit-from-reduction red@ -->)

(define-syntax-rule (define-eval-unit eval@ red@)
  (define-mixed-unit eval@
    (import)
    (export  eval^)
    (inherit [red@ reducer])

    ;; --> : δ → State → (Setof State)
    (define (--> δ) (reducer δ))))

(define-eval-unit eval@ red@)
