#lang racket
(require "../../interp/dprint.rkt"
         (only-in "../../interp/core/misc.rkt" define-runner)
         (only-in "../../interp/core/main.rkt"
                  reader printer expander/expand main/run
                  eval-->/--> eval-->*/--> expand==>/==> expand==>*/==>)

         ;; Abstract version
         (only-in "eval.rkt" -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>c expand)
         
         (for-syntax racket/list))
(provide (all-defined-out))

(define expander (expander/expand expand))

(define-runner run
  reader
  expander
  stripper printer
  eval
  parse)

(define main (main/run run))

;; for debug

(define eval-->    (eval-->/-->    -->c))
(define eval-->*   (eval-->*/-->   -->c))
(define expand==>  (expand==>/==>  ==>c))
(define expand==>* (expand==>*/==> ==>c))
