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
            (only   cont^    push-cont)]

  #:do [;; lookup-val : Store Loc → (SetM Val)
        (define (lookup-val sto loc)
          (do val <- (lookup-store sto loc)
              #:when (val? val)
              (pure val)))

        ;; lookup-cont : Store Loc → (SetM Cont)
        (define (lookup-cont sto loc)
          (do cnt <- (lookup-store sto loc)
              #:when (cont? cnt)
              (pure cnt)))])

(define-unit-from-reduction red@ -->)

(define-syntax-rule (define-eval-unit eval@ red@)
  (define-mixed-unit eval@
    (import)
    (export  eval^)
    (inherit [red@ reducer])

    ;; --> : δ → State → (Setof State)
    (define (--> δ) (reducer δ))))

(define-eval-unit eval@ red@)
