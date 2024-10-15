#lang racket/unit
(require
 (only-in "../../set.rkt"    set subset?)
 (only-in "../../nondet.rkt" results)
 "../../signatures.rkt")

(import (only bind^    resolve))
(export id^)


;; id=? : Id Nam ξ Σ → Boolean
(define (id=? id nam Σ)
  (subset? (set nam) (results (resolve id Σ))))

;; core-form? : Nam Σ → Id → Boolean
(define (core-form? nam Σ)
  (λ (id) (id=? id nam Σ)))
