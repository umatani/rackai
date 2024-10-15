#lang racket
(require
 (only-in "../nondet.rkt"     lift)
 (only-in "../mix.rkt"        define-mixed-unit)
 "../signatures.rkt"
 (only-in "../base/units.rkt" [menv@ base:menv@]))
(provide menv@)

(define-mixed-unit menv@
  (import)
  (export  menv^)
  (inherit [base:menv@ init-ξ])

  ;; Set-based ξ

  ; lookup-ξ : ξ Nam -> (SetM AllTransform)
  (define (lookup-ξ ξ nam)
    (lift (hash-ref ξ nam (λ () (set)))))

  ; extend-ξ : ξ Nam AllTransform -> ξ
  (define (extend-ξ ξ nam all-transform)
    (hash-update ξ nam
                 (λ (old) (set-add old all-transform)) (set))))
