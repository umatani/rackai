#lang racket
(require
 (except-in racket set do)
 "../../set.rkt"
 "../../nondet.rkt"
 "../../mix.rkt"
 
 (only-in "../../signatures.rkt" menv^)
 (rename-in "../base/units.rkt" [menv@ base:menv@]))
(provide menv@)

(define-mixed-unit menv@
  (import)
  (export menv^)
  (inherit [base:menv@ init-ξ])

  ;; Set-based ξ

  ; lookup-ξ : ξ Nam -> (SetM AllTransform)
  (define (lookup-ξ ξ nam)
    (lift (hash-ref ξ nam (λ () (set)))))

  ; update-ξ : ξ Nam AllTransform -> ξ
  (define (extend-ξ ξ nam all-transform)
    (hash-update ξ nam
                 (λ (old) (set-add old all-transform)) (set))))
