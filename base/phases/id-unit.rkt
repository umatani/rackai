#lang racket/unit
(require
 "../../signatures.rkt")

(import (only bind^    resolve))
(export id^)


;; id=? : Ph Id Nam Σ → Boolean
(define (id=? ph id nam Σ)
  (eq? (resolve ph id Σ) nam))
