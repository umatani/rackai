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

(define (run delta form mode)
  (aborts (do stx := (reader form)
              #:abort-if (eq? mode 'read) stx
              (cons stx2 Σ2) <- (lift (expander delta stx))
              #:abort-if (eq? mode 'expand) stx2
              ast <- (parser stx2 Σ2)
              #:abort-if (eq? mode 'parse) ast
              ast2 <- (lift (evaluate delta ast))
              #:abort-if (eq? mode 'eval) (printer ast2)
              (error 'run "unknown mode: ~e" mode))))
