#lang racket
(require (only-in "../../interp/core/misc.rkt" define-runner)
         (only-in "../../interp/phases/main.rkt"
                  reader printer expander/expand parser/parse main/run
                  eval-->/--> eval-->*/--> expand==>/==> expand==>*/==>)

         ;; Abstract version
         (only-in "../core/eval.rkt" -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>p expand)

         (for-syntax racket/list))
(provide (all-defined-out))

(define expander (expander/expand expand))
(define parser (parser/parse parse))

(define-runner run
  reader
  expander
  stripper printer
  eval
  parser)

(define main (main/run run))

;; for debug

(define eval-->    (eval-->/-->    -->c))
(define eval-->*   (eval-->*/-->   -->c))
(define expand==>  (expand==>/==>  ==>p))
(define expand==>* (expand==>*/==> ==>p))
