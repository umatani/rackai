#lang racket
(require "set.rkt"
         (for-syntax racket syntax/parse))
(provide := <-
         pure lift results aborts
         do
         (for-syntax assign elem))

(define := (gensym ':=))
(define <- (gensym '<-))

;; non-deterministic & failure monad
(define (pure  x) (cons (set x) (set)))
(define (lift xs) (cons xs      (set)))
(define (break x) (cons (set)   (set x)))
(define (results m) (car m))
(define (aborts  m) (cdr m))

(define (bind r k)
  (let ([r′ (set-map (car r) k)])
    (cons (apply set-union (set)   (map car r′))
          (apply set-union (cdr r) (map cdr r′)))))

(define (gen-bind kind r k #:multi-values? [is-mv #f])
  (cond
    [(and (eq? kind :=) is-mv) (call-with-values r k)]
    [(eq? kind :=) (k r)]
    [(eq? kind <-) (bind r k)]
    [else (error "no such case")]))

(begin-for-syntax
  (define-syntax-class assign
    #:description "set-monad := operators"
    (pattern s:id #:when (string-prefix? (symbol->string (syntax-e #'s))
                                         ":=")))
  (define-syntax-class elem
    #:description "set-monad <- operators"
    (pattern s:id #:when (string-prefix? (symbol->string (syntax-e #'s))
                                         "<-"))))

(define-syntax (do stx)
  (syntax-parse stx
    #:literals [do values]
    [(do s) #'s]
    [(do (values pat ...) assign-id:assign e s₀ s ...)
     #'(gen-bind assign-id (λ () e)
                 (match-lambda** [(pat ...) (do s₀ s ...)])
                 #:multi-values? #t)]
    [(do pat assign-id:assign e s₀ s ...)
     #'(gen-bind assign-id e
                 (match-lambda [pat (do s₀ s ...)]))]
    [(do pat elem-id:elem e s₀ s ...)
     #'(gen-bind elem-id e
                 (match-lambda [pat (do s₀ s ...)]))]
    [(do #:abort-if t e s ...)
     #'(if t
         (break e)
         (do s ...))]
    [(do s₀ s ...)
     #'(begin s₀ (do s ...))]))
