#lang racket
(require
 "../set.rkt"
 "../interpreter.rkt"
 (only-in "../term.rkt" use-terms)
 (only-in racket [eval r:eval])
 (prefix-in base: (only-in "../interp-base/full/main.rkt" interp))
 (only-in "../terms.rkt" #%term-forms
          Bool% Num% Sym% Null% Pair%
          lst->list/recur))
(provide run-examples)

(use-terms Bool Num Sym Null Pair)

;;;; Host evaluater for checking
;; raw-eval: Sexp -> (Setof Val)
(define (raw-eval sexp)
  (define (r->v raw)
    (match raw
      [(? null?)      (Null)]
      [(? boolean? b) (Bool b)]
      [(? real? n)    (Num n)]
      [(? symbol? s)  (Sym s)]
      [(cons a d)     (Pair (r->v a) (r->v d))]))
  (set (r->v (first (call-with-values (λ () (r:eval sexp))
                                      (λ args args))))))

;;;; Base evaluator
;; base-eval: Sexp -> (Setof Val)
(match-define (interpreter _ base:run base:delta _ _ _) base:interp)
(define (base-eval sexp)
  (base:run base:delta sexp 'eval))


;;;; Example runner

(define (runner form interp mode)
  (match-define (interpreter name run delta α ≤a rlt) interp)
  (case mode
    [(raw) (lst->list/recur (raw-eval form))]
    [(check check-with-raw)
     (define opponent (case mode
                        [(check)          base-eval]
                        [(check-with-raw) raw-eval]))

     (with-handlers ([exn:fail? (λ (_)
                                  (hash-update! rlt 'fail add1) 'fail)])
       (let ([c (α (with-handlers ([exn:fail?
                                     (λ (e)
                                       (printf "error in opponent: ~a" e))])
                      (opponent form)))]
             [a (run delta form 'eval)])
         (cond
           [(and (≤a c a)
                 (≤a a c)) (hash-update! rlt 'exact   add1) 'exact]
           [(≤a c a)       (hash-update! rlt 'inexact add1) 'inexact]
           [else           (hash-update! rlt 'unsound add1) 'unsound])))]
    [else (lst->list/recur (run delta form mode))]))

(define (run-examples examples interp mode)
  (for ([example (in-list examples)])
    (match-define (list name form) example)
    (printf "  ~a: " name)
    (pretty-display
     (runner form interp mode))))
