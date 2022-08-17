#lang racket
(require
 "../set.rkt"
 (only-in "../term.rkt" use-terms)
 (only-in racket [eval r:eval])
 (prefix-in base: (only-in "../interp-base/full/main.rkt" run delta))
 (only-in "../terms.rkt" #%term-forms
           Bool% Num% Sym% Null% Pair%))
(provide
 fail-count run-examples)

(use-terms Bool Num Sym Null Pair)

;;;; Host evaluater for checking
;; raw-eval: Sexp -> (Setof Val)
(define (raw-eval sexp)
  (define (r->v raw)
    (match raw
      [(? null?)      (Null)]
      [(? boolean? b) (Bool b)]
      [(? real? n)    (Num n)]
      [(? symbol? s)  (Sym s)]
      [(cons a d)     (Pair (r->v a) (r->v d))]))
  (set (r->v (first (call-with-values (λ () (r:eval sexp))
                                      (λ args args))))))

;;;; Base evaluator
;; base-eval: Sexp -> (Setof Val)
(define (base-eval sexp)
  (base:run base:delta sexp 'eval))

;;;; Example runner

(define fail-count (make-parameter -1))

(define (runner run delta example mode α ≤a)
  (pretty-print
   (case mode
     [(raw) (raw-eval example)]
     [(check)
      (fail-count (if (< (fail-count) 0) 0 (fail-count)))
      (let* ([c (base-eval example)]
             [a (run delta example 'eval)]
             [result (≤a (α c) a)])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]
     [(check-with-raw)
      (fail-count (if (< (fail-count) 0) 0 (fail-count)))
      (let* ([c (raw-eval example)]
             [a (run delta example 'eval)]
             [result (≤a (α c) a)])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]

     [else (run delta example mode)])))

(define (run-example run delta examples name
                     [mode 'check] [α identity] [≤a subset?])
  (let ([example (assoc name examples)])
    (when example
      (runner run delta (cadr example) mode α ≤a))))

(define (run-examples run delta examples
                      [mode 'check] [α identity] [≤a subset?])
  (for ([example (in-list examples)])
    (printf "~a: " (car example))
    (runner run delta (cadr example) mode α ≤a)))
