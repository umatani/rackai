#lang racket
(require "../../interp/dprint.rkt"
         (only-in "../../interp/example.rkt" core:examples phases:examples)
         (only-in "../../interp/phases/main.rkt"
                  reader printer expander/expand parser/parse
                  eval-->/--> eval-->*/--> expand==>/==> expand==>*/==>)

         ;; Abstract version
         (only-in "../core/misc.rkt" define-runner run-examples)
         (only-in "../core/eval.rkt" -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>p expand)

         (for-syntax racket/list))
(provide (all-defined-out))

(define expander (expander/expand expand))
(define parser (parser/parse parse))

(define-runner run
  reader printer
  expander parser eval)

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))

;; for debug

(define eval-->    (eval-->/-->    -->c))
(define eval-->*   (eval-->*/-->   -->c))
(define expand==>  (expand==>/==>  ==>p))
(define expand==>* (expand==>*/==> ==>p))
