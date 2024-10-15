#lang racket/unit
(require
 "../../signatures.rkt")

(import (only bind^    resolve))
(export id^)


;; id=? : Ph Id Nam Σ → Boolean
(define (id=? ph id nam Σ)
  (eq? (resolve ph id Σ) nam))

;; core-form? : Ph Nam Σ → Id → Boolean
(define (core-form? ph nam Σ)
  (λ (id) (id=? ph id nam Σ)))
