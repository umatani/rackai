#lang racket
(require
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ bind^ parse^ parser^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 (only-in "../units.rkt" parse@))
(provide parser@)

(define-unit parser/parse@
  (import
   (only terms^
         Σ*%)
   (prefix p: (only parse^
                    parse)))
  (export parser^)

  (use-terms Σ*)

  (define parse p:parse)

  ; parser : Stx Σ* -> (SetM Ast)
  (define (parser stx Σ*) (parse #:phase 0 stx (Σ*-Σ Σ*))))

(define-compound-unit/infer parser@
  (import terms^ terms-extra^ syntax^ menv^ bind^)
  (export parser^)
  (link   parse@ parser/parse@))
