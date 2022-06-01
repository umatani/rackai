#lang racket
(require "../../interp/dprint.rkt"
         "../../interp/nondet.rkt"
         (only-in "../../interp/example.rkt" core:examples)
         (only-in "../../interp/core/main.rkt"
                  reader printer expander/expand
                  eval-->/--> eval-->*/--> expand==>/==> expand==>*/==>)

         ;; Abstract version
         (only-in "misc.rkt" define-runner run-examples)
         (only-in "eval.rkt" -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>c expand)
         
         (for-syntax racket/list))
(provide (all-defined-out))

(define expander (expander/expand expand))

(define-runner run
  reader printer
  expander parse eval)

(define (main [mode 'check])
  (run-examples run core:examples mode))

;; for debug

(define eval-->    (eval-->/-->    -->c))
(define eval-->*   (eval-->*/-->   -->c))
(define expand==>  (expand==>/==>  ==>c))
(define expand==>* (expand==>*/==> ==>c))
