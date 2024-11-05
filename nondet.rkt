#lang racket/base
(require
 (for-syntax racket/base racket/string syntax/parse)
 (only-in racket/match match-lambda match-lambda**)
 (only-in "set.rkt" set ∅ ∪ set-map))
(provide enable-checkpoint := <-
         mzero mplus pure bind lift results aborts for/m+ do
         (for-syntax assign elem))

(define enable-checkpoint (make-parameter #f))

(define := (gensym ':=))
(define <- (gensym '<-))

;; non-deterministic & failure monad
(define mzero (cons ∅ ∅))

(define (mplus m m′)
  (cons (∪ (car m) (car m′))
        (∪ (cdr m) (cdr m′))))

(define-syntax (for/m+ stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     #'(for/fold ([m mzero])
                 clauses
         (mplus m (let () . defs+exprs)))]))

(define (mconcat . ms) (for/m+ ([m ms]) m))

(define (pure  x) (cons (set x) ∅))
(define (abort x) (cons ∅ (set x)))

(define (bind m k)
  (let ([m′ (set-map k (results m))])
    (cons (apply ∪            (map results m′))
          (apply ∪ (aborts m) (map aborts  m′)))))

(define (lift   xs) (cons xs ∅))
(define (results m) (car m))
(define (aborts  m) (cdr m))

(define (generic-bind kind r k #:multi-values? [is-mv #f])
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
     #'(generic-bind assign-id (λ () e)
                     (match-lambda** [(pat ...) (do s₀ s ...)])
                     #:multi-values? #t)]
    [(do pat assign-id:assign e s₀ s ...)
     #'(generic-bind assign-id e
                     (match-lambda [pat (do s₀ s ...)]))]
    [(do pat elem-id:elem e s₀ s ...)
     #'(generic-bind elem-id e
                     (match-lambda [pat (do s₀ s ...)]))]
    [(do #:when t s ...)
     #'(if t
         (do s ...)
         mzero)]
    [(do #:abort-if t e s ...)
     #'(if t
         (abort e)
         (do s ...))]
    [(do #:checkpoint s₀ s ...)
     #'(begin (when (enable-checkpoint) s₀) (do s ...))]
    [(do s₀ s ...)
     #'(begin s₀ (do s ...))]))
