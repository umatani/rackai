#lang racket
(require "../term.rkt")

(define-term Fun% (var body))
(define-term CFun% Fun% ([env #f]))

(define-term-interface Fun%
  [constructor (Fun var body)]
  [matcher     (Fun var body)])

(match (Fun 'x 1)
  [(Fun v b) (cons v b)]
  [_ #f])


(define-term-interface Fun% #;CFun%
   [constructor (Fun2 body var)]
   [matcher     (Fun2 var)])

(match (Fun2 'x 1)
  [(Fun2 v) v]
  [_ #f])


(define-term-interface CFun%
  [constructor (CFun env var body)]
  [matcher     (MFun body env var)])

(match (CFun '() 'x 1)
  [(MFun b e v) (list v b e)]
  [_ #f])
