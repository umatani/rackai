#lang racket
(require "../../base/nondet.rkt")
[provide (all-defined-out)]

;;;; runner

(define-syntax-rule (define-runner run reader printer expander parser evaluater)
  (define (run form mode)
    (cdr (do stx := (reader form)
             #:failif (eq? mode 'read) stx
             (cons stx2 Σ2) <- (lift (expander stx))
             #:failif (eq? mode 'expand) stx2
             ast <- (parser stx2 Σ2)
             #:failif (eq? mode 'parse) ast
             ast2 <- (lift (evaluater ast))
             #:failif (eq? mode 'eval) (printer ast2)
             (error 'run "unknown mode: ~e" mode)))))
