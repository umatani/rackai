#lang racket/unit
(require
 "../../nondet.rkt"

 (only-in "../../signatures.rkt"
          eval^ parser^ expander^ io^ run^))

;;;; runner

(import (only io^       reader printer)
        (only expander^ expander)
        (only parser^   parser)
        (only eval^     evaluate))
(export run^)

(define (run form mode)
  (cdr (do stx := (reader form)
           #:failif (eq? mode 'read) stx
           (cons stx2 Σ2) <- (lift (expander stx))
           #:failif (eq? mode 'expand) stx2
           ast <- (parser stx2 Σ2)
           #:failif (eq? mode 'parse) ast
           ast2 <- (lift (evaluate ast))
           #:failif (eq? mode 'eval) (printer ast2)
           (error 'run "unknown mode: ~e" mode))))
