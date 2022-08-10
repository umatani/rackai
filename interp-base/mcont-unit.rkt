#lang racket/unit
(require
 racket/match
 (only-in "../term.rkt" use-terms)

 (only-in "../signatures.rkt" mstore^ mcont^)
 (only-in "../terms.rkt"
          𝓁%
          #%term-forms))

(import (only mstore^ update-Σ alloc-𝓁))
(export mcont^)

(use-terms 𝓁)

;; ----------------------------------------
;; Expand-time stack operations:

; push-κ : Σ Stx κ -> (Values 𝓁 Σ)
;   Stx is being expanded. The entire set will be finite in abs.
(define (push-κ Σ stx κ)
  (let-values ([(𝓁 Σ_1) (alloc-𝓁 stx Σ)])
    (values 𝓁 (update-Σ Σ_1 𝓁 κ))))
