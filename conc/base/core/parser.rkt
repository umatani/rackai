#lang racket
(require
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)
 (only-in "../../../terms.rkt" terms^ #%term-forms)

 (only-in "../parse-unit.rkt" parse@))
(provide parser@)

(define-unit parser/parse@
  (import (prefix p: parse^))
  (export parser^)

  (define parse p:parse)
  (define parser parse))


(define-compound-unit/infer parser@
  (import terms^ terms-extra^ syntax^ menv^ bind^)
  (export parser^)
  (link   parse@ parser/parse@))
