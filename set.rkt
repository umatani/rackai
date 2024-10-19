#lang racket/base
(require (for-syntax racket/base syntax/parse)
         (only-in racket/match define-match-expander)
         (prefix-in r: racket/set))
(provide set set? ∅ ∅? ∪ set-add set-remove set-subtract ⊆ set-size set=?
         set-map ∈ set→list list→set for/set in-set)

(define (set-print s port mode)
  (when mode (write-string "{" port))
  (let ([es (repl-elems s)]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))])])
    (unless (zero? (r:set-count es))
      (recur (r:set-first es) port)
      (for-each (λ (e)
                  (write-string " " port)
                  (recur e port))
                (r:set->list (r:set-rest es)))))
  (when mode (write-string "}" port)))
 
(struct repl (elems)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc set-print)])

(define-match-expander set
  (syntax-rules (... ...)
    [(set p ... q (... ...))
     (and (app (compose1 r:set->list repl-elems)
               (list-no-order p ... qs (... ...)))
          (app (λ (_) (repl (r:list->set qs))) q))]
    [(set p ...)
     (app (compose1 r:set->list repl-elems) (list-no-order p ...))])
  (syntax-id-rules ()
    [(set p ...) (repl (r:set p ...))]
    [set (λ args (repl (apply r:set args)))]))

(define set? repl?)

(define ∅ (repl (r:set)))
(define (∅? s) (r:set-empty? (repl-elems s)))

(define (∪ . ss)
  (if (null? ss)
    ∅
    (repl (apply r:set-union (map repl-elems ss)))))

(define (set-add s e) (repl (r:set-add (repl-elems s) e)))

(define (set-remove s e) (repl (r:set-remove (repl-elems s) e)))

(define (set-subtract . ss)
  (repl (apply r:set-subtract (map repl-elems ss))))

(define (⊆ s s′) (r:subset? (repl-elems s) (repl-elems s′)))

(define (set-size s) (r:set-count (repl-elems s)))

(define (set=? s s′) (r:set=? (repl-elems s) (repl-elems s′)))

(define (set-map f s) (r:set-map (repl-elems s) f))

(define (∈ e s) (r:set-member? (repl-elems s) e))

(define (set→list s) (r:set->list (repl-elems s)))

(define (list→set l) (repl (r:list->set l)))

(define-syntax (for/set stx)
  (syntax-parse stx
    [(_ (for-clause ...) body ...+)
     #'(repl (r:for/set (for-clause ...) body ...))]))

(define (in-set s) (r:in-set (repl-elems s)))

