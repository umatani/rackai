#lang racket
(require
 (only-in "../../base/phases/terms.rkt" terms^)
 (only-in "../../../terms-extra.rkt"    terms-extra^)
 (only-in "../../../syntax-sig.rkt"     syntax^)
 (only-in "../../../menv-sig.rkt"       menv^)
 (only-in "../../../mstore-sig.rkt"     mstore^)
 (only-in "../../../parse-sig.rkt"      parse^)
 (only-in "../../../parser-sig.rkt"     parser^)

 (only-in "../parse-unit.rkt"           parse@))
(provide parser@)

;; Non-deterministic parsing

(define-unit parser/parse@
  (import (prefix p: parse^))
  (export parser^)

  (define parse p:parse)

  ; parser : Stx Σ -> (SetM Ast)
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))

(define-compound-unit parser@
  (import [t : terms^] [te : terms-extra^] [stx : syntax^]
          [me : menv^] [msto : mstore^])
  (export pr)
  (link (([p  : parse^])  parse@ t te stx me msto)
        (([pr : parser^]) parser/parse@ p)))
