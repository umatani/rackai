#lang racket/unit
(require
 racket/match
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt" mstore^ mcont^)
 (only-in "../../terms.rkt" terms^ #%term-forms))

(import (only terms^  𝓁%)
        (only mstore^ update-Σ alloc-𝓁))
(export mcont^)

(use-terms 𝓁)

;; ----------------------------------------
;; Expand-time stack operations:

; push-κ : Σ κ -> (Values 𝓁 Σ)
(define (push-κ Σ κ)
  (let-values ([(𝓁 Σ_1) (alloc-𝓁 Σ)])
    (values 𝓁 (update-Σ Σ_1 𝓁 κ))))
