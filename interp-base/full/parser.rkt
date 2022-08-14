#lang racket
(require
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)
 (only-in "config.rkt" config^ #%term-forms)
 (only-in "../units.rkt" parse@))
(provide parser@)

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define-mixed-unit parser@
  (import (only config^
                Σ*%))
  (export parser^)
  (inherit [parse@ parse])

  (use-terms Σ*)

  (define (parser stx Σ*) (parse #:phase 0 stx (Σ*-Σ Σ*))))