#lang racket/unit
(require
 (except-in racket do)
 "../nondet.rkt"

 (only-in "../signatures.rkt"
          eval^ parser^ expander^ io^ run^))

;;;; runner

(import (only eval^      evaluate)
        (only parser^    parser)
        (only expander^  expander)
        (only io^ reader printer))
(export run^)

(define (run delta form mode)
  (aborts (do stx := (reader form)
              #:abort-if (eq? mode 'read) stx
              (cons stx2 Σ2) := (expander delta stx)
              #:abort-if (eq? mode 'expand) stx2
              ast := (parser stx2 Σ2)
              #:abort-if (eq? mode 'parse) ast
              ast2 := (evaluate delta ast)
              #:abort-if (eq? mode 'eval) ast2 #;(printer ast2) 
              (error 'run "unknown mode: ~e" mode))))
