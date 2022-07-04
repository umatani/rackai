#lang racket
(require
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)
 (only-in "../../base/core/terms.rkt" terms^)

 (only-in "../units.rkt" parse@))
(provide parser@)

;; Non-deterministic parsing

(define-unit parser/parse@
  (import (prefix p: parse^))
  (export parser^)

  (define parse p:parse)

  (define parser parse))

(define-compound-unit/infer parser@
  (import terms^ terms-extra^ syntax^ menv^ bind^)
  (export parser^)
  (link parse@ parser/parse@))
