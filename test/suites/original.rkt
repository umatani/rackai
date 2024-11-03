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

(define ex-defs-bind-var
  '[defs-bind-var
    (let-syntax ((q (lambda (stx)
                      (let ((defs (syntax-local-make-definition-context)))
                        (let ((ignored #;1   ;; causes unbound-variable error
                                       (syntax-local-bind-syntaxes
                                        (list (second (syntax-e stx)))
                                        #f
                                        defs)))
                          (let ((new-x (local-expand
                                        (second (syntax-e stx))
                                        'expression
                                        '()
                                        defs)))
                            (datum->syntax #'here
                                           (list #'lambda
                                                 (datum->syntax #'here
                                                                (list new-x))
                                                 new-x))))))))
      ((q x) 100))])


(define full:examples
  (list ex-defs-bind-var))

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
