#lang racket
(require
 (only-in "../../mix.rkt" define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../units.rkt"  parse@))
(provide parser@)

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])

  (define (parser stx Σ*) (parse #:phase 0 stx (Σ*-Σ Σ*))))
