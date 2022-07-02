#lang racket
(require
 racket/match
 (only-in "../../../term.rkt"        use-terms)

 (only-in "terms.rkt"                terms^ #%term-forms)
 (only-in "../../../terms-extra.rkt" terms-extra^)
 (only-in "../../../syntax-sig.rkt"  syntax^)
 (only-in "../../../menv-sig.rkt"    menv^)
 (only-in "../../../mstore-sig.rkt"  mstore^)
 (only-in "../../../parse-sig.rkt"   parse^)
 (only-in "../../../parser-sig.rkt"  parser^)

 (only-in "../parse-unit.rkt"        parse@))
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

(define-compound-unit parser@
  (import [t : terms^] [te : terms-extra^] [stx : syntax^]
          [me : menv^] [msto : mstore^])
  (export pr)
  (link (([p : parse^]) parse@ t te stx me msto)
        (([pr : parser^]) parser/parse@ t p)))
