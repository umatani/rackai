#lang racket/unit
(require
 (only-in "../../nondet.rkt" do <- pure)
 "../../signatures.rkt"
 "../../base/full/terms.rkt")

(import (only bind^    resolve)
        (only menv^    lookup-ξ))
(export id^)


;; id=? : Ph Id Nam ξ Σ → (SetM Boolean)
(define (id=? ph id nam ξ Σ)
  (do nam′ <- (resolve ph id Σ)
      at   <- (lookup-ξ ξ nam)
      (pure (and (eq? nam nam′) (not (TStop? at))))))
