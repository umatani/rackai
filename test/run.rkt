#lang racket
(require
 "../set.rkt"
 "../interpreter.rkt"
 (only-in "../term.rkt" use-terms)
 (only-in racket [eval r:eval])
 (prefix-in base: (only-in "../base/full/main.rkt" interp))
 (only-in "../terms.rkt" #%term-forms
          Bool% Num% Sym% Null% Pair%
          lst->list/recur))
(provide run-examples)

(use-terms Bool Num Sym Null Pair)

;;;; Host evaluater for checking
;; raw-eval: Sexp → (Setof Val)
(define (raw-eval form)
  (define (r->v raw)
    (match raw
      [(? null?)      (Null)]
      [(? boolean? b) (Bool b)]
      [(? real? n)    (Num n)]
      [(? symbol? s)  (Sym s)]
      [(cons a d)     (Pair (r->v a) (r->v d))]))
  (set (r->v (first (call-with-values (λ () (r:eval form))
                                      (λ args args))))))

;;;; Base evaluator
;;;;   base-eval: Sexp → (Setof Val)
(define (base-eval form)
  (match-define (interpreter _ run delta _ _ _) base:interp)
  (set (run delta form 'eval)))


;;;; Example runner
(define (runner form interp mode)
  (match-define (interpreter _name run delta α ≤α rslt) interp)
  (case mode
    [(raw) (lst->list/recur (raw-eval form))]
    [(check check-with-raw)
     (define opponent (case mode
                        [(check)          base-eval]
                        [(check-with-raw) raw-eval]))

     (with-handlers ([exn:fail? (λ (_)
                                  (hash-update! rslt 'fail add1)
                                  'fail)])
       (let ([c (with-handlers ([exn:fail?
                                 (λ (e)
                                   (printf "error in opponent: ~a\n" e))])
                  (opponent form))]
             [a (α (run delta form 'eval))])
         (cond
           [(and (≤α c a)
                 (≤α a c)) (hash-update! rslt 'exact   add1) 'exact]
           [(≤α c a)       (hash-update! rslt 'inexact add1) 'inexact]
           [else           (hash-update! rslt 'unsound add1) 'unsound])))]
    [else (lst->list/recur (run delta form mode))]))

(define (run-examples examples interp mode)
  (for ([example (in-list examples)])
    (match-define (list name form) example)
    (printf "  ~a: " name)
    (pretty-display (runner form interp mode))))
