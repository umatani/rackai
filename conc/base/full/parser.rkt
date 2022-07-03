#lang racket
(require
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ mstore^ parse^ parser^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../parse-unit.rkt" parse@))
(provide parser@)

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define-unit parser/parse@
  (import (only terms^
                Σ*%)
          (prefix p: (only parse^
                           parse)))
  (export parser^)

  (use-terms Σ*)

  ; parse : Ph Stx Σ -> Ast
  (define parse p:parse)

  ; parser : Stx Σ* -> Ast
  (define (parser stx Σ*) (parse #:phase 0 stx (Σ*-Σ Σ*))))

(define-compound-unit/infer parser@
  (import terms^ terms-extra^ syntax^ menv^ mstore^)
  (export parser^)
  (link parse@ parser/parse@))
