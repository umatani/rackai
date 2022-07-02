#lang racket/unit
(require
 racket/match
 (only-in "../../../term.rkt"      use-terms)

 (only-in "terms.rkt"              terms^ #%term-forms)
 (only-in "../../../mcont-sig.rkt" mcont^))

(import (only terms^
              Θ% 𝓁%))
(export mcont^)

(use-terms Θ 𝓁)

;; ----------------------------------------
;; Expand-time stack operations:

; init-Θ : -> Θ
(define (init-Θ) (Θ 0 (make-immutable-hash)))

; alloc-κ : Θ -> (Values 𝓁 Θ)
(define (alloc-κ θ)
  (match-let ([(Θ size tbl) θ])
    (values (𝓁 (string->symbol (format "k~a" size)))
            (Θ (add1 size) tbl))))

; lookup-κ : Θ 𝓁 -> κ
(define (lookup-κ θ 𝓁) (hash-ref (Θ-tbl θ) 𝓁))

; update-κ : Θ 𝓁 κ -> Θ
(define (update-κ θ 𝓁 κ)
  (match-let ([(Θ size tbl) θ])
    (Θ size (hash-set tbl 𝓁 κ))))

; push-κ : Θ κ -> (Values 𝓁 Θ)
(define (push-κ θ κ)
  (let-values ([(𝓁 θ_1) (alloc-κ θ)])
    (values 𝓁 (update-κ θ_1 𝓁 κ))))
