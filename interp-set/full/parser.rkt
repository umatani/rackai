#lang racket
(require
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ menv^ bind^ parse^ parser^)
 (only-in "../../interp-base/full/terms.rkt" #%term-forms
          Σ*%)
 (only-in "../units.rkt" parse@))
(provide parser@)

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])
  (use-terms Σ*)

  ; parser : Stx Σ* -> (SetM Ast)
  (define (parser stx Σ*) (parse #:phase 0 stx (Σ*-Σ Σ*))))
