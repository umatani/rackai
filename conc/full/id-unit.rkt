#lang racket/unit
(require
 "../../signatures.rkt"
 "terms.rkt")

(import (only bind^    resolve)
        (only menv^    init-ξ lookup-ξ))
(export id^)

;; id=? : Ph Id Nam ξ Σ → Boolean
(define (id=? ph id nam ξ Σ)
  (let ([nam0 (resolve ph id Σ)])
    (and (eq? nam nam0) (not (TStop? (lookup-ξ ξ nam))))))

;; core-form? : Ph Nam Σ → Id → Boolean
(define (core-form? ph nam Σ)
  (λ (id) (id=? ph id nam (init-ξ) Σ)))
