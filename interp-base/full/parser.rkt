#lang racket
(require
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ menv^ bind^ parse^ parser^)
 (only-in "../units.rkt" parse@)
 (only-in "terms.rkt" #%term-forms
          Σ*%))
(provide parser@)

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])
  (use-terms Σ*)

  (define (parser stx Σ*) (parse #:phase 0 stx (Σ*-Σ Σ*))))
