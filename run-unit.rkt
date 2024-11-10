#lang racket/unit
(require
 (only-in "set.rkt"    for/set in-set)
 (only-in "nondet.rkt" do := <- pure Right? Left? Right-value Left-msg)
 (only-in "syntax.rkt" stx→datum)
 "signatures.rkt"
 "terms.rkt")

;;;; runner

(import (only        io^    reader)
        (only  expander^    expander)
        (only    parser^    parser)
        (only evaluator^    evaluator))
(export run^)

;; run : δ Sexp Symbol → (Setof Val)
(define (run δ form mode)
  (define m
    (do stx := (reader form)
        (if (eq? mode 'read)
          (pure (lst→list/recur (stx→datum stx)))
          (do (cons stx′ Σ) <- (expander δ stx)
              (if (eq? mode 'expand)
                (pure stx′)
                (do ast <- (parser stx′ Σ)
                    (if (eq? mode 'parse)
                      (pure ast)
                      (do val <- (evaluator δ ast)
                          (if (eq? mode 'eval)
                            (pure val)
                            (error 'run "unknown mode: ~e"
                                   mode))))))))))
  (for ([x (in-set m)]
        #:when (Left? x))
    (printf "[error] ~a\n" (Left-msg x)))
  (for/set ([x (in-set m)]
            #:when (Right? x))
    (Right-value x)))
