#lang racket/unit
(require
 (except-in racket do)
 "../nondet.rkt"

 (only-in "../signatures.rkt"
          eval^ parser^ expander^ io^ run^)
 (only-in "../terms.rkt" lst->list/recur stx->datum))

;;;; runner

(import (only eval^      evaluate)
        (only parser^    parser)
        (only expander^  expander)
        (only io^ reader printer))
(export run^)

(define (run delta form mode)
  (aborts (do stx := (reader form)
              #:abort-if (eq? mode 'read) stx
              (cons stx* Σ) := (expander delta stx)
              #:abort-if (eq? mode 'expand) (lst->list/recur (stx->datum stx*))
              ast := (parser stx* Σ)
              #:abort-if (eq? mode 'parse) ast
              val := (evaluate delta ast)
              #:abort-if (eq? mode 'eval) (lst->list/recur val)
              (error 'run "unknown mode: ~e" mode))))
