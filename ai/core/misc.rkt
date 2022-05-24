#lang racket
(require (only-in "../../interp/core/nondet.rkt" do pure break))
(provide (all-defined-out))

;;;; runner

(define-syntax-rule (define-runner run reader printer expander parser evaluater)
  (define (run form mode)
    (set-first
     (cdr (do stx := (reader form)
              #:failif (eq? mode 'read) stx
              (cons stx2 Σ2) := (expander stx)
              #:failif (eq? mode 'expand) stx2
              ast <- (parser stx2 Σ2)
              #:failif (eq? mode 'parse) ast
              ast2 := (evaluater ast)
              #:failif (eq? mode 'eval) (printer ast2)
              (error 'run "unknown mode: ~e" mode))))))


(define-syntax-rule (define-runner3 run reader printer expander parser evaluater)
  (define (run form mode)
    (for/set (#:do [(define result #f)
                    (define stx (reader form))]
              #:do [(when (and (not result)
                               (eq? mode 'read)) (set! result stx))]
              #:do [(define-values (stx2 Σ2)
                      (if result
                          (values #f #f)
                          (expander stx)))]
              #:do [(when (and (not result)
                               (eq? mode 'expand)) (set! result stx2))]
              [ast (in-set (if result
                               (set)
                               (parser stx2 Σ2)))]
              #:do [(when (and (not result)
                               (eq? mode 'parse)) (set! result ast))]
              #:do [(define ast2
                      (if result
                          #f
                          (evaluater ast)))]
              #:do [(when (and (not result)
                               (eq? mode 'eval)) (set! result (printer ast2)))])
      (or result (error 'run "unknown mode: ~e" mode)))))

#;
(define-syntax-rule (define-runner2 run reader printer expander parser evaluater)
  (define (run form mode)
    (if (eq? mode 'none)
        form
        (let ([stx (reader form)])
          (if (eq? mode 'read)
              stx
              (let-values ([(stx2 Σ2) (expander stx)])
                (if (eq? mode 'expand)
                    stx2
                    (let ([ast (parser stx2 Σ2)])
                      (if (eq? mode 'parse)
                          ast
                          (let ([ast2 (evaluater ast)])
                            (if (eq? mode 'eval)
                                (printer ast2)
                                (error 'run "unknown mode: ~e" mode))))))))))))

