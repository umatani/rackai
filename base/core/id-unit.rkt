#lang racket/unit
(require
 "../../signatures.rkt")

(import (only bind^    resolve))
(export id^)

;; id=? : Id Nam Σ → Boolean
(define (id=? id nam Σ)
  (eq? (resolve id Σ) nam))
