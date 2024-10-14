#lang racket
(require
 (only-in "../../mix.rkt"   define-mixed-unit)
 "../../signatures.rkt"
 (only-in    "../units.rkt" parse@))
(provide parser@)


(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])

  (define parser parse))
