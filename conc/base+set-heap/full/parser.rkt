#lang racket
(require
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ menv^ mstore^ parse^ parser^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 (only-in "../parse-unit.rkt" parse@))
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
  (import terms^ terms-extra^ syntax^ menv^ mstore^)
  (export parser^)
  (link   parse@ parser/parse@))
#;
(define-compound-unit parser@
  (import [t : terms^] [te : terms-extra^] [stx : syntax^]
          [me : menv^] [msto : mstore^])
  (export pr)
  (link (([p  : parse^])  parse@        t te stx me msto)
        (([pr : parser^]) parser/parse@ t p)))
