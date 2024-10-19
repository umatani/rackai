#lang racket/base
(require
 (for-syntax racket/base racket/string syntax/parse)
 (only-in racket/match match-lambda match-lambda**)
 (only-in "set.rkt" set ∅ ∪ set-map))
(provide := <-
         pure lift results aborts
         do
         (for-syntax assign elem))

(define := (gensym ':=))
(define <- (gensym '<-))

;; non-deterministic & failure monad
(define (pure    x) (cons (set x) ∅))
(define (lift   xs) (cons xs      ∅))
(define (break   x) (cons ∅       (set x)))
(define (results m) (car m))
(define (aborts  m) (cdr m))

(define (bind m k)
  (let ([m′ (set-map k (results m))])
    (cons (apply ∪            (map results m′))
          (apply ∪ (aborts m) (map aborts  m′)))))

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
