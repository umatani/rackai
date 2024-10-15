#lang racket/unit
(require
 (only-in "../../set.rkt"    set subset?)
 (only-in "../../nondet.rkt" results)
 "../../signatures.rkt")

(import (only bind^ resolve))
(export id^)


;; id=? : Ph Id Nam Σ -> Boolean
(define (id=? ph id nam Σ)
  (subset? (set nam) (results (resolve ph id Σ))))

;; core-form? : Ph Nam Σ → Id → Boolean
(define (core-form? ph nam Σ) (λ (id) (id=? ph id nam Σ)))
