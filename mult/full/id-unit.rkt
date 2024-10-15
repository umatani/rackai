#lang racket/unit
(require
 (only-in "../../set.rkt"    set subset? set->list)
 (only-in "../../nondet.rkt" results)
 "../../signatures.rkt"
 "../../base/full/terms.rkt")

(import (only bind^    resolve)
        (only menv^    init-ξ lookup-ξ))
(export id^)


;; id=? : Ph Id Nam ξ Σ → Boolean
(define (id=? ph id nam ξ Σ)
  (let ([nam0 (results (resolve ph id Σ))])
    (and (subset? (set nam) nam0)
         (andmap (λ (at) (not (TStop? at)))
                 (set->list (results (lookup-ξ ξ nam)))))))

;; core-form? : Ph Nam Σ → Id → Boolean
(define (core-form? ph nam Σ)
  (λ (id) (id=? ph id nam (init-ξ) Σ)))
