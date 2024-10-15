#lang racket
(require
 (only-in "../../mix.rkt"       define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../phases/units.rkt" [bind@ phases:bind@]))
(provide bind@)

(define-mixed-unit bind@
  (import  (only menv^      init-ξ lookup-ξ))
  (export  bind^)
  (inherit [phases:bind@    bind resolve])

  ; id=? : Ph Id Nam ξ Σ → Boolean
  (define (id=? ph id nam ξ Σ)
    (let ([nam0 (resolve ph id Σ)])
      (and (eq? nam nam0) (not (TStop? (lookup-ξ ξ nam))))))

  ;; core-form? : Ph Nam Σ → Id → Boolean
  (define (core-form? ph nam Σ)
    (λ (id) (id=? ph id nam (init-ξ) Σ)))
  )
