#lang racket
(require
 (for-syntax racket/unit-exptime
             racket/syntax
             syntax/parse syntax/stx syntax/id-set)
 (only-in "../../../mix.rkt" define-mixed-unit)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)

 (only-in "../units.rkt" parse@ bind@ run@))
(provide parser@)


(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])

  (define parser parse))
