#lang racket/unit
(require
 (only-in "../../nondet.rkt" do <- pure)
 "../../signatures.rkt")

(import (only bind^    resolve))
(export id^)


;; id=? : Id Nam Σ → (SetM Boolean)
(define (id=? id nam Σ)
  (do nam′ <- (resolve id Σ)
      (pure (eq? nam nam′))))
