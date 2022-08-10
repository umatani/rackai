#lang racket
(require
 (only-in racket [eval r:eval]))
(provide
 fail-count run-examples)

;;;; Example runner

(define fail-count (make-parameter -1))

(define (runner run delta example mode α ≤a)
  (println
   (case mode
     [(raw) (first (call-with-values (λ () (r:eval example))
                                     (λ args args)))]
     [(check)
      (fail-count (if (< (fail-count) 0) 0 (fail-count)))
      (let* ([c (first (call-with-values
                        (λ () (r:eval example))
                        (λ args args)))]
             [a (run delta example 'eval)]
             [result (≤a (α c) a)])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]
     [else (run delta example mode)])))

(define (run-example run delta examples name [mode 'check] [α set] [≤a subset?])
  (let ([example (assoc name examples)])
    (when example
      (runner run delta (cadr example) mode α ≤a))))

(define (run-examples run delta examples [mode 'check] [α set] [≤a subset?])
  (for ([example (in-list examples)])
    (printf "~a: " (car example))
    (runner run delta (cadr example) mode α ≤a)))
