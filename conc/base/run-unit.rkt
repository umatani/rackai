#lang racket/unit
(require
 (rename-in (except-in racket do))
 "../../nondet.rkt"

 (only-in "../../eval-sig.rkt"   eval^)
 (only-in "../../parser-sig.rkt" parser^)
 (only-in "../../expand-sig.rkt" expand^)
 (only-in "../../io-sig.rkt"     io^)
 (only-in "../../run-sig.rkt"    run^))

;;;; runner

(import (only eval^ evaluate)
        (only parser^ parser)
        (only expand^ expander)
        (only io^ reader printer))
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
           (error 'run "unknown mode: ~e" mode))))
