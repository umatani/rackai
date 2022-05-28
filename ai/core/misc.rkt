#lang racket
(require (only-in racket [eval r:eval])
         "../../interp/set.rkt"
         (only-in "../../interp/nondet.rkt" do pure break)
         (only-in "../../interp/core/misc.rkt"
                  fail-count run-ex/ex-runner
                  run-examples/ex-runner
                  run-all-examples/run-examples))
(provide (all-defined-out))

;;;; runner

(define-syntax-rule (define-runner run reader printer expander parser evaluater)
  (define (run form mode)
    (cdr (do stx := (reader form)
             #:failif (eq? mode 'read) stx
             (cons stx2 Σ2) <- (expander stx)
             #:failif (eq? mode 'expand) stx2
             ast <- (parser stx2 Σ2)
             #:failif (eq? mode 'parse) ast
             ast2 <- (evaluater ast)
             #:failif (eq? mode 'eval) (printer ast2)
             (error 'run "unknown mode: ~e" mode)))))

;;;; Example runner

;; Updated for check
(define (ex-runner run example mode)
  (printf "~a: " (car example))
  (println
   (case mode
     [(raw) (first (call-with-values (λ () (r:eval (cadr example)))
                                     (λ args args)))]
     [(check)
      (fail-count (if (< (fail-count) 0) 0 (fail-count)))
      (let* ([r1 (set-first (run (cadr example) 'eval))]
             [r2 (first (call-with-values
                         (λ () (r:eval (cadr example)))
                         (λ args args)))]
             [result (equal? r1 r2)])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]
     [else (run (cadr example) mode)])))

(define run-ex (run-ex/ex-runner ex-runner))
(define run-examples (run-examples/ex-runner ex-runner))
(define run-all-examples (run-all-examples/run-examples run-examples))
