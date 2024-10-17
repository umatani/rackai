#lang racket/unit
(require
 (only-in "../nondet.rkt" aborts do := <- lift)
 "../signatures.rkt"
 "../terms.rkt")

;;;; runner

(import (only       io^    reader)
        (only expander^    expander)
        (only   parser^    parser)
        (only     eval^    evaluate))
(export run^)

;; run : δ Sexp Symbol → (Setof Val)
(define (run delta form mode)
  (aborts
   (do stx := (reader form)
       #:abort-if (eq? mode 'read) (lst->list/recur (stx->datum stx))

       (cons stx′ Σ) <- (expander delta stx)
       #:abort-if (eq? mode 'expand) (lst->list/recur (stx->datum stx′))

       ast <- (parser stx′ Σ)
       #:abort-if (eq? mode 'parse) ast

       val <- (evaluate delta ast)
       #:abort-if (eq? mode 'eval) val

       (error 'run "unknown mode: ~e" mode))))
