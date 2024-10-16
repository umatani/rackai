#lang racket/unit
(require
 "signatures.rkt")

(import (only mstore^    update-Σ alloc-𝓁))
(export mcont^)


;; ----------------------------------------
;; Expand-time stack operations:

; push-κ : Σ Stx κ → (Values 𝓁 Σ)
;   Stx is being expanded. The entire set of Stx would be finite in abs.
(define (push-κ Σ stx κ)
  (let-values ([(𝓁 Σ′) (alloc-𝓁 stx Σ)])
    (values 𝓁 (update-Σ Σ′ 𝓁 κ))))
