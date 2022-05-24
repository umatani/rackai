#lang racket
(provide (all-defined-out))

(define (pure x) (cons (set x) (set)))
(define (break x) (cons (set) (set x)))
(define (bind r k)
  (let ([r2 (set-map (car r) k)])
    (cons (apply set-union (set) (map car r2))
          (apply set-union (cdr r) (map cdr r2)))))

(define-syntax (do stx)
  (syntax-case stx (<- :=)
    [(do s) #'s]
    [(do x <- e s1 s ...)
     #'(bind (cons e (set)) (λ (x) (do s1 s ...)))]
    [(do pat := e s1 s ...)
     #'(match-let ([pat e]) (do s1 s ...))]
    [(do #:failif t e s ...)
     #'(if t
           (break e)
           (do s ...))]
    [(do s1 s ...)
     #'(begin s1 (do s ...))]))
