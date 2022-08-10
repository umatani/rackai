#lang racket
(provide core:examples phases:examples full:examples finite:examples)


;; core examples

(define ex-lam
  '[lam
    ((lambda (lambda) lambda) 'foo)])

(define ex-fxx
  '[fxx
    ((lambda (f x) (f x))
     (lambda (x) x)
     100)])

(define ex-call-bound
  '[call-bound
    (let ([f +]) (f 1 2))])

(define core:examples
  (list ex-lam
        ex-fxx       
        ex-call-bound
        ;ex-fact
        ))

;; phases examples

(define phases:examples
  (list))

;; full examples

(define full:examples
  (list))

;; finite exapmles

;; NumとBoolの有限化の確認

(define ex-fact
  '[fact
    (let ([fact (lambda (f n)
                  (if (= n 0)
                      1
                      (* n (f f (- n 1)))))])
      (fact fact 10))])

(define finite:examples
  (list ex-fact))
