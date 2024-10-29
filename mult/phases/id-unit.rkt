#lang racket/unit
(require
 (only-in "../../nondet.rkt" do <- pure)
 "../../signatures.rkt")

(import (only bind^ resolve))
(export id^)


;; id=? : Ph Id Nam Σ → (SetM Boolean)
(define (id=? ph id nam Σ)
  (do nam′ <- (resolve ph id Σ)
      (pure (eq? nam nam′))))
