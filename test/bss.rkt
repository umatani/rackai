#lang racket
;; Samples from `Binding as Sets of Scopes'
;; (https://www-old.cs.utah.edu/plt/scope-sets/)

;; 1 Background: Scope and Macros

(module sec1 racket
  (define-syntax-rule (send obj-expr method-name arg)
    (let ([obj obj-expr])
      ((lookup-method obj 'method-name) obj arg)))

  (define a-point '())
  (define (lookup-method . args) (error "not implemented."))
  (send a-point rotate 90)

  (define f (lambda (obj)
              (send a-point same? obj)))

  (let ([lookup-method #f])
    (send a-point rotate 90))

  (define-syntax-rule (with-method ([local-id (obj-expr method-name)])
                        body)
    (let ([obj obj-expr])
      (let ([method (lookup-method obj 'method-name)])
        (let-syntax ([local-id (syntax-rules ()
                                 [(local-id arg)
                                  (method obj arg)])])
          body))))

  (with-method ([rotate-a-point (a-point rotate)])
    (for ([i 1000000])
      (rotate-a-point 90)))
  )

;; 2 Scope Sets for Pattern-Based Macros

(module sec2 racket
  (let ([x 1])
    (lambda (y)
      'z))

  ((let ([x 1])
     (let-syntax ([m (syntax-rules ()
                       [(m) x])])
       (lambda (x)
         (m)))) 2)

  ((letrec-syntax ([identity (syntax-rules ()
                               [(_ misc-id)
                                (lambda (x)
                                  (let ([misc-id 'other])
                                    x))])])
     (identity x)) 'this)

  (define-syntax-rule (define-identity id)
    (define id (lambda (x) x)))

  (define-identity f)
  (f 5)

  (define-syntax-rule (define-five misc-id)
    (begin
      (define misc-id 5)
      x))
  (define-five x)

  (define-syntax-rule (define-other-five misc-id)
    (begin
      (define x 55)
      misc-id))
  (define-other-five x)

  ;; (define-syntax-rule (def-m m given-x)
  ;;   (begin
  ;;     (define x 1)
  ;;     (define-syntax-rule (m)
  ;;       (begin
  ;;         (define given-x 2)
  ;;         x))))
  ;; (def-m m x)
  ;; (m)

  (define-syntax-rule (def-m m orig-x)
    (define-syntax-rule (m)
      (begin
        (define orig-x 2)
        x)))
  (def-m m x)
  (m)

  (module test2 racket
    (define-syntax-rule (def-m m)
      (define-syntax-rule (m)
        x))
    (define x 2)
    (def-m m)
    (m))

  (module test3 racket
    (define-syntax-rule (def-m m orig-x)
      (begin
        (define orig-x 2)
        (define-syntax-rule (m)
          x)))
    (def-m m x)
    (m))

  (module test4 racket
    (define-syntax-rule (def-m m)
      (define-syntax-rule (m orig-x)
        (begin
          (define orig-x 2)
          x)))
    (def-m m)
    (m x))
  )

;; 3 Scope Sets for Procedural Macros and Modules

(module sec3 racket

  (let ()
    (define-syntax (m stx)
      (syntax-case stx ()
        [(_ a b)
         (begin
           (printf "~s\n" (bound-identifier=? #'a #'b))
           #'(begin
               (define a 1)
               b))]))
    (define-syntax n
      (syntax-rules ()
        [(_ id) (m id x)]))
    (n x))

  ;; from documentation of free-identifier=?
  (define-syntax (check stx)
    (syntax-case stx ()
      [(_ x)
       (if (free-identifier=? #'car #'x)
         #'(list 'same: x)
         #'(list 'different: x))]))
  (check car)
  (check mcar)
  (let ([car list])
    (check car))
  (let ([car car])
    (check car))
  (require (rename-in racket/base [car kar]))
  (check kar)

  ;; from documentation of bound-identifier=?
  (define-syntax (checkb stx)
    (syntax-case stx ()
      [(_ x y)
       (if (bound-identifier=? #'x #'y)
         #'(let ([y 'wrong]) (let ([x 'binds]) y))
         #'(let ([y 'no-binds]) (let ([x 'wrong]) y)))]))
  (checkb a a)
  (checkb a b)
  (checkb b a)
  (define-syntax-rule (check-a  x) (checkb a x))
  (define-syntax-rule (check-a2 x) (checkb x a))
  (check-a  a)
  (check-a2 a)

  (let-syntax ([let-1 (lambda (stx)
                        (let ([id #'x])
                          #`(let ([x 1])
                              #,id)))])
    (let-1))

  #;
  (let-syntax ([let-1 (lambda (stx)
                        (let ([id #'x])
                          #`(let ([x 1])
                              #,(quote-syntax id #:local))))])
    (let-1))

  (define-for-syntax (make-scopeless sym)
    (syntax-local-introduce (datum->syntax #f sym)))
  (define-syntax (let-x stx)
    (syntax-case stx ()
      [(_ rhs body)
       #`(let ([#,(make-scopeless 'x) rhs])
           body)]))
  (let-x 5
         (let-x 6
                0 #;x))

  (define-syntax (def-x stx)
    (syntax-case stx ()
      [(_ rhs)
       #`(define #,(make-scopeless 'x) rhs)]))

  (require racket/block)
  (block
   (define y 1)
   (def-x 5)
   (+ x y))
  (block
   (define y 2)
   (def-x 6)
   (+ x y))

  ;; due to outside-edge scopes
  (block
   (define x 1)
   (def-x 5)
   x)
  (block
   ;; (define x 2)
   (def-x 6)
   x)

  )

(module sec3.6 racket
  

  (module sample racket
    (module defx racket
      (define x -1)
      (provide x))

    (require (for-template 'defx))
    (define x 0)
    (define-for-syntax x 1)

    (define id #'x)
    (define-for-syntax id #'x)

    (provide id (for-syntax id)))

  (require 'sample
           (for-syntax (prefix-in s: 'sample)))
  (printf "phase-0: id = ") (println id)
  (begin-for-syntax
    (printf "phase-1: s:id = ") (println s:id)
    (printf "phase-1:   id = ") (println id)
    (printf "free-id-equal?  ~a\n" (free-identifier=?  s:id id))
    (printf "bound-id-equal? ~a\n" (bound-identifier=? s:id id))
    )
  (define-syntax (get-x stx)
    (printf "in get-x expansion: id = ") (println id)
    id)
  (define-syntax (get-x′ stx)
    (printf "in get-x′ expansion: s:id = ") (println s:id)
    s:id)
  (get-x)
  (get-x′)

  )
