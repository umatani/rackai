#lang racket/base
(require
 (for-syntax racket/base racket/string syntax/parse)
 (only-in racket/match  match match-lambda match-lambda**)
 (only-in "set.rkt"     set ∅ ∪ for/set in-set))
(provide enable-checkpoint := <-
         mzero mplus pure lift abort bind results aborts for/m+ do
         (struct-out Right) (struct-out Left)
         (for-syntax assign elem))

;; failure + non-deterministic monad

(define enable-checkpoint (make-parameter #f))

(begin-for-syntax
  (define-syntax-class assign
    #:description "set-monad := operators"
    (pattern s:id #:when (string-prefix? (symbol->string (syntax-e #'s))
                                         ":=")))
  (define-syntax-class elem
    #:description "set-monad <- operators"
    (pattern s:id #:when (string-prefix? (symbol->string (syntax-e #'s))
                                         "<-"))))


(define := (gensym ':=))
(define <- (gensym '<-))


;; mzero : (SetM A)
(define mzero ∅)

;; mplus : (SetM A) (SetM A) → (SetM A)
(define (mplus m m′) (∪ m m′))

(define-syntax (for/m+ stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     #'(for/fold ([m mzero])
                 clauses
         (mplus m (let () . defs+exprs)))]))

;; mconcat : (SetM A) ... → (SetM A)
(define (mconcat . ms) (for/m+ ([m ms]) m))

(struct Right (value) #:transparent)
(struct Left  (msg)   #:transparent)

;; pure : A → (SetM A)
(define (pure v) (set (Right v)))

;; lift : (Setof A) → (SetM A)
(define (lift vs) (for/m+ ([v (in-set vs)]) (pure v)))

;; abort : String → (SetM A)
(define (abort msg) (set (Left msg)))

;; bind : (SetM A) (A → (SetM B)) → (SetM B)
(define (bind m k)
  (for/fold ([s ∅])
            ([x (in-set m)])
    (match x
      [(Right  v) (∪ s (k v))]
      [(Left msg) (∪ s (abort msg))])))

;; results : (SetM A) → (Setof A)
(define (results m)
  (for/set ([x (in-set m)]
            #:when (Right? x))
    (Right-value x)))

;; aborts : (SetM A) → (Setof String)
(define (aborts m)
  (for/set ([x (in-set m)]
            #:when (Left? x))
    (Left-msg x)))

(define (generic-bind kind m k #:values? [vs? #f])
  (cond
    [(and (eq? kind :=) vs?) (call-with-values m k)]
    [(eq? kind :=) (k m)]
    [(eq? kind <-) (bind m k)]
    [else (error "no such case")]))

(define-syntax (do stx)
  (syntax-parse stx
    #:literals [do values]
    [(do s) #'s]
    [(do #:when t s ...)
     #'(if t
         (do s ...)
         mzero)]
    [(do #:abort-if t msg s ...)
     #'(if t
         (abort msg)
         (do s ...))]
    [(do #:checkpoint e s ...)
     #'(begin (when (enable-checkpoint) e) (do s ...))]
    [(do (values pat ...) ≐:assign e s₀ s ...)
     #'(generic-bind ≐ (λ () e)
                     (match-lambda** [(pat ...) (do s₀ s ...)])
                     #:values? #t)]
    [(do pat (~or* ≐:assign ≐:elem) e s₀ s ...)
     #'(generic-bind ≐ e (match-lambda [pat (do s₀ s ...)]))]
    [(do s₀ s ...)
     #'(begin s₀ (do s ...))]))
