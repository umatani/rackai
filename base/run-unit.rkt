#lang racket/unit
(require
 (only-in racket/match    match-let)
 (only-in "../set.rkt"    set)
 (only-in "../nondet.rkt" aborts do :=)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../syntax.rkt" stx->datum))

;;;; runner

(import (only io^          reader)
        (only expander^    expander)
        (only parser^      parser)
        (only eval^        evaluate))
(export run^)

;; run : δ Sexp Symbol → Val
(define (run δ form mode)
  (match-let
      ([(set v)
        (aborts
         (do stx := (reader form)
             #:abort-if (eq? mode 'read) (lst->list/recur (stx->datum stx))

             (cons stx′ Σ) := (expander δ stx)
             #:abort-if (eq? mode 'expand) (lst->list/recur (stx->datum stx′))

             ast := (parser stx′ Σ)
             #:abort-if (eq? mode 'parse) ast

             val := (evaluate δ ast)
             #:abort-if (eq? mode 'eval) val

             (error 'run "unknown mode: ~e" mode)))])
    v))
