#lang racket
(require
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)
 (only-in "../../base/phases/terms.rkt" terms^)

 (only-in "../units.rkt" parse@))
(provide parser@)

;; Non-deterministic parsing

(define-unit parser/parse@
  (import (prefix p: parse^))
  (export parser^)

  (define parse p:parse)

  ; parser : Stx Σ -> (SetM Ast)
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))

(define-compound-unit/infer parser@
  (import terms^ terms-extra^ syntax^ menv^ bind^)
  (export parser^)
  (link   parse@ parser/parse@))
