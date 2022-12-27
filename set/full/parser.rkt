#lang racket
(require
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          domain^ syntax^ menv^ bind^ parse^ parser^)
 (only-in "../../conc/full/terms.rkt" #%term-forms Σ*%)
 (only-in "../units.rkt" parse@))
(provide parser@)

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ [super:parse parse] parse*])
  (use-terms Σ*)

  (define parse (super:parse super:parse parse*))

  ; parser : Stx Σ* -> (SetM Ast)
  (define (parser stx Σ*) (parse #:phase 0 stx (Σ*-Σ Σ*))))
