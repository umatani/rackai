#lang racket/unit
(require
 (only-in "../../../menv-sig.rkt" menv^))

(import)
(export menv^)

;; ----------------------------------------
;; Expand-time environment operations:

; init-ξ : -> ξ
(define (init-ξ) (make-immutable-hash))

; lookup-ξ : ξ Nam -> AllTransform
(define (lookup-ξ ξ nam) (hash-ref ξ nam (λ () 'not-found)))

; extend-ξ : ξ Nam AllTransform -> ξ
(define (extend-ξ ξ nam all-transform) (hash-set ξ nam all-transform))
