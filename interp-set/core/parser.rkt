#lang racket
(require
 "../../mix.rkt"

 (only-in "../../signatures.rkt"
          syntax^ menv^ bind^ parse^ parser^)

 (only-in "../units.rkt" parse@))
(provide parser@)

;; Non-deterministic parsing

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])

  (define parser parse))
