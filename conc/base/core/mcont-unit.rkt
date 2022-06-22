#lang racket/unit
(require
 racket/match
 (only-in "../../../struct-common-sig.rkt" struct-common^)
 (only-in "../../../mcont-sig.rkt"         mcont^))

(import (only struct-common^
              Θ mk-Θ Θ-tbl mk-𝓁))
(export mcont^)

;; ----------------------------------------
;; Expand-time stack operations:

; init-Θ : -> Θ
(define (init-Θ) (mk-Θ 0 (make-immutable-hash)))

; alloc-κ : Θ -> (Values 𝓁 Θ)
(define (alloc-κ θ)
  (match-let ([(Θ size tbl) θ])
    (values (mk-𝓁 (string->symbol (format "k~a" size)))
            (mk-Θ (add1 size) tbl))))

; lookup-κ : Θ 𝓁 -> κ
(define (lookup-κ θ 𝓁) (hash-ref (Θ-tbl θ) 𝓁))

; update-κ : Θ 𝓁 κ -> Θ
(define (update-κ θ 𝓁 κ)
  (match-let ([(Θ size tbl) θ])
    (mk-Θ size (hash-set tbl 𝓁 κ))))

; push-κ : Θ κ -> (Values 𝓁 Θ)
(define (push-κ θ κ)
  (let-values ([(𝓁 θ_1) (alloc-κ θ)])
    (values 𝓁 (update-κ θ_1 𝓁 κ))))
