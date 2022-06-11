#lang racket
(require (rename-in (except-in racket do))
         "nondet.rkt"
         "io.rkt"
         "parse-sig.rkt"
         "eval-sig.rkt"
         "expand-sig.rkt")
(provide run^ run@)

;;;; runner

(define-signature run^
  (run))

(define-unit run@
  (import (only io^ reader printer)
          (only expand^ expander)
          (only parse^ parser)
          (only eval^ evaluate))
  (export run^)

  (define (run form mode)
    (cdr (do stx := (reader form)
             #:failif (eq? mode 'read) stx
             (cons stx2 Σ2) := (expander stx)
             #:failif (eq? mode 'expand) stx2
             ast := (parser stx2 Σ2)
             #:failif (eq? mode 'parse) ast
             ast2 := (evaluate ast)
             #:failif (eq? mode 'eval) (printer ast2)
             (error 'run "unknown mode: ~e" mode)))))
