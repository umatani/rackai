#lang racket
(require
 (only-in "../../mix.rkt"          define-mixed-unit)
 "../../signatures.rkt"
 "../../conc/full/terms.rkt"
 (only-in    "../phases/units.rkt" parse@))
(provide parse@  parser@)

(define-mixed-unit parser@
  (import)
  (export  parser^)
  (inherit [parse@ [super:parse parse] parse*])

  (define parse (super:parse super:parse parse*))

  ; parser : Stx Σ* → (SetM Ast)
  (define (parser stx Σ*) (parse 0 stx (Σ*-Σ Σ*))))
