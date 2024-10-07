#lang racket/unit
(require
 "../nondet.rkt"

 (only-in "../signatures.rkt"
           io^ expander^ parser^ eval^ run^)
 (only-in "../terms.rkt" lst->list/recur stx->datum))

;;;; runner

(import (only io^       reader printer)
        (only expander^ expander)
        (only parser^   parser)
        (only eval^     evaluate))
(export run^)

(define (run delta form mode)
  (aborts (do stx := (reader form)
              #:abort-if (eq? mode 'read) (lst->list/recur (stx->datum stx))
              (cons stx* Σ) <- (lift (expander delta stx))
              #:abort-if (eq? mode 'expand) (lst->list/recur (stx->datum stx*))
              ast <- (parser stx* Σ)
              #:abort-if (eq? mode 'parse) ast
              val <- (lift (evaluate delta ast))
              #:abort-if (eq? mode 'eval) val
              (error 'run "unknown mode: ~e" mode))))
