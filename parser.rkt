#lang racket
(require
 "signatures.rkt")
(provide core-parser@ phases-parser@)

;; parser : Stx Σ →       Ast     (base)
;;          Stx Σ → (SetM Ast)    (mult)

(define-unit core-parser@
  (import
   (only   parse^    parse))
  (export parser^)
  (define parser parse))

(define-unit phases-parser@
  (import (only parse^    parse))
  (export  parser^)
  (define (parser stx Σ) (parse 0 stx Σ)))
