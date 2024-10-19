#lang racket/base
(require
 racket/unit
 "../../reduction.rkt"
 (only-in "../../mix.rkt"            define-mixed-unit inherit)
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../base/core/eval.rkt" [--> base:-->]))
(provide --> red@ eval@)

;; Revised reduction rules

;; --> : State → (Setof State)
(define-reduction (--> δ) #:super (base:--> δ <-)
  #:within-signatures [(only domain^
                             val?)
                       (only env^
                             lookup-env extend-env*)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)])

(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import  (only domain^    val?)
           (only    env^    init-env)
           (only  store^    init-store))
  (export  eval^)
  (inherit [red@ reducer])

  ;; δ →   State → (Setof State)
  (define (--> δ) (reducer δ))

  ;; evaluate : Ast → (SetM Val)
  (define (evaluate δ ast)
    (define -->δ (--> δ))
    (do `(,(? val? val) ● ,_store) <- (lift (apply-reduction*
                                             -->δ
                                             `(,(AstEnv ast (init-env))
                                               ● ,(init-store))))
        (pure val))))
