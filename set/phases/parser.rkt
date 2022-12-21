#lang racket
(require
 "../../mix.rkt"
 
 (only-in "../../signatures.rkt"
          domain^ syntax^ menv^ bind^ parse^ parser^)

 (only-in "../units.rkt" parse@))
(provide parser@)

;; Non-deterministic parsing

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ [super:parse parse] parse*])

  (define parse (super:parse super:parse parse*))

  ; parser : Stx Σ -> (SetM Ast)
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))
