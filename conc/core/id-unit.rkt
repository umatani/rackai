#lang racket/unit
(require
 "../../signatures.rkt")

(import (only bind^    resolve))
(export id^)

;; id=? : Id Nam Σ → Boolean
(define (id=? id nam Σ)
  (eq? (resolve id Σ) nam))

;; core-form? : Nam Σ → Id → Boolean
(define (core-form? nam Σ)
  (λ (id) (id=? id nam Σ)))
