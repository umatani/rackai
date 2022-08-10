#lang racket
(require
 "../../mix.rkt"
 (only-in "../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)

 (only-in "../units.rkt" parse@))
(provide parser@)

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])

  ; parser : Stx Σ -> Ast
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))
