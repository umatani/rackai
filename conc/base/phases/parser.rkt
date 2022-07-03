#lang racket
(require
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ mstore^ parse^ parser^)
 (only-in "terms.rkt" terms^)

 (only-in "../parse-unit.rkt" parse@))
(provide parser@)

(define-unit parser/parse@
  (import (prefix p: parse^))
  (export parser^)

  ; parse : Ph Stx Σ -> Ast
  (define parse p:parse)

  ; parser : Stx Σ -> Ast
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))

(define-compound-unit/infer parser@
  (import terms^ terms-extra^ syntax^ menv^ mstore^)
  (export parser^) 
  (link   parse@ parser/parse@))
#;
(define-compound-unit parser@
  (import [t : terms^] [te : terms-extra^] [stx : syntax^]
          [me : menv^] [msto : mstore^])
  (export pr)
  (link (([p  : parse^])  parse@ t te stx me msto)
        (([pr : parser^]) parser/parse@ p)))
