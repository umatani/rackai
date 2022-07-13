#lang racket/unit
(require
 (rename-in (except-in racket do))
 "../../nondet.rkt"

 (only-in "../../signatures.rkt"
          eval^ parser^ expander^ io^ run^))

;;;; runner

(import (only eval^      evaluate)
        (only parser^    parser)
        (only expander^  expander)
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
           #:failif (eq? mode 'eval) #;ast2 (printer ast2)
           (error 'run "unknown mode: ~e" mode))))
