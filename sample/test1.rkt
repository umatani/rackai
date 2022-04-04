#lang racket/base
(require (for-syntax racket/base))

;;;; from Binding as Sets of Scopes
;;;; https://www.cs.utah.edu/plt/scope-sets/index.html

;; 2.1
(define z 100)
(let ([x 1])
  (lambda (y)
    z))

(let ([x 1])
  (let-syntax ([m (syntax-rules ()
                    [(m) x])])
    (lambda (x)
       (m))))

;; 2.3

(letrec-syntax ([identity (syntax-rules ()
                            [(_ misc-id)
                             (lambda (x)
                               (let ([misc-id 'other])
                                 x))])])
  (identity x))

;; 2.4
(let ()
  (define-syntax-rule (define-identity id)
    (define id (lambda (x) x)))
  
  (define-identity f)
  (f 5))

(let ()
  (define-syntax-rule (define-five misc-id)
    (begin
      (define misc-id 5)
      x))
  
  (define-five x))

#;
(let ()
  (define-syntax-rule (define-other-five misc-id)
    (begin
      (define x 5)
      misc-id))
  
  (define-other-five x))


;; 2.5

#;
(let ()
  (define-syntax-rule (def-m m given-x)
    (begin
      (define x 1)
      (define-syntax-rule (m)
        (begin
          (define given-x 2)
          x))))
  
  (def-m m x)
  (m))


(let ()
  (define-syntax-rule (def-m m orig-x)
    (define-syntax-rule (m)
      (begin
        (define orig-x 2)
        x)))
  (def-m m x)
  (m))

(let ()
  (define-syntax-rule (def-m m)
    (define-syntax-rule (m)
      x))
  (define x 2)
  (def-m m)
  (m))
 
(let ()
  (define-syntax-rule (def-m m orig-x)
    (begin
      (define orig-x 2)
      (define-syntax-rule (m)
        x)))
  (def-m m x)
  (m))
 
(let ()
  (define-syntax-rule (def-m m)
    (define-syntax-rule (m orig-x)
      (begin
        (define orig-x 2)
        x)))
  (def-m m)
  (m x))

;; 3.1

(let ()
  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ a b)
       (begin
         (printf "3.1: ~s\n" (bound-identifier=? #'a #'b))
         #'(begin
             (define a 1)
             b))]))
  (define-syntax n
    (syntax-rules ()
      [(_ id) (m id x)]))
  (n x))

;; 3.2
(printf "[3.2]\n")

;; quote-syntax で prune が必要な例
;; しかし，resolve するのは phase 0 では？
(define-syntax t32-1 (lambda (stx)
                       (let ([id #'x])
                         #`(let ([x 1])
                             #,id))))
(t32-1 'dummy)

(define-syntax t32-2 (lambda (stx)
                       (let ([id (quote-syntax x #:local)])
                         #`(let ([#,(quote-syntax x #:local) 1])
                             #,id))))
;(t32-2 'dummy)


;; 普通に(Schemeや古いRacketで)考えれば同じ(外側の)xを差しているのは
;; 不自然だが，新しいRacketではこうなっている．
(free-identifier=? (let ([x 1]) #'x)
                   #'x)
(bound-identifier=? (let ([x 1]) #'x)
                    #'x)

;; こう書けば古いRacketと同じ
(free-identifier=? (let ([x 1]) (quote-syntax x #:local))
                   (quote-syntax x #:local))
(bound-identifier=? (let ([x 1]) (quote-syntax x #:local))
                    (quote-syntax x #:local))

;; 3.3
(printf "[3.3]\n")

#;
(define-syntax (let-x stx)
  (syntax-case stx ()
    [(_ rhs body)
     #`(let ([#,(syntax-local-introduce #'x) rhs])
         body)]))
#;
(let-x 5
  (let-x 6
    0))

(define-syntax (let-x stx)
  (syntax-case stx ()
    [(_ rhs body)
     #`(let ([#,(syntax-local-introduce #'x) rhs])
         body)]))
(let-x 5
  x)


;;;; from API doc for bound-identifier=?
(define-syntax (check stx)
    (syntax-case stx ()
      [(_ x y)
       #'(let ([y 'no-binds]) (let ([x 'wrong]) y))]))
;(define-syntax-rule (check-a x) (check a x))
(define-syntax-rule (check-a x) (check x a))
(check-a a)
; Answer:
; --> (check{a,i} a{a,u} a{a,i})
; --> (let ([a{a,i} 'no-binds]) (let ([a{a,u} 'wrong]) a{a,i}))
