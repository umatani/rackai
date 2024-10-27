#lang racket/unit
(require
 "../signatures.rkt")

(import)
(export menv^)

;; ----------------------------------------
;; Expand-time environment operations:

;; init-ξ : → ξ
(define (init-ξ)
  (make-immutable-hash))

;; lookup-ξ : ξ Nam → AllTransform
(define (lookup-ξ ξ nam)
  (hash-ref ξ nam 'not-found))

;; extend-ξ : ξ Nam AllTransform → ξ
(define (extend-ξ ξ nam at)
  (hash-set ξ nam at))
