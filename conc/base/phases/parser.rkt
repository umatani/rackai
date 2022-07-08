#lang racket
(require
 "../../../mix.rkt"
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)
 (only-in "terms.rkt" terms^)

 (only-in "../units.rkt" parse@))
(provide parser@)

#;
(define-unit parser/parse@
  (import (prefix p: parse^))
  (export parser^)

  ; parse : Ph Stx Σ -> Ast
  (define parse p:parse)

  ; parser : Stx Σ -> Ast
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))

#;
(define-compound-unit/infer parser@
  (import terms^ terms-extra^ syntax^ menv^ bind^)
  (export parser^) 
  (link   parse@ parser/parse@))

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ parse])

  ; parser : Stx Σ -> Ast
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))
