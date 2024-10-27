#lang racket/base
(require
 racket/unit
 "../../reduction.rkt"
 (only-in "../../mix.rkt"            define-mixed-unit inherit)
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../base/core/eval.rkt" [--> base:-->]))
(provide --> define-eval-unit eval@)

;; Revised reduction rules

;; --> : State → (Setof State)
(define-reduction (--> δ) #:super (base:--> δ <-)
  #:import [(only domain^    val?)
            (only    env^    lookup-env extend-env*)
            (only  store^    lookup-store update-store* alloc-loc*)
            (only   cont^    push-cont)])

(define-unit-from-reduction red@ -->)

(define-syntax-rule (define-eval-unit eval@ red@)
  (define-mixed-unit eval@
    (import)
    (export  eval^)
    (inherit [red@ reducer])

    ;; --> : δ → State → (Setof State)
    (define (--> δ) (reducer δ))))

(define-eval-unit eval@ red@)
