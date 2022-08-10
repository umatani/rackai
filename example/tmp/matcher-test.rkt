#lang racket
(require (for-syntax syntax/parse)
         "../term.rkt"
         "../terms.rkt")

(define-term List ())
(define-term Null List ())
(define-term Pair List (a d))

(define-term-form List% (List))
(define-term-form Null% (Null))
(define-term-form Pair% (Pair a d))

(define (lst->list l)
  (match l
    [(Null) '()]
    [(Pair a d) (cons a (lst->list d))]))

(define (list->lst l)
  (match l
    ['() (Null)]
    [(cons a d) (Pair a (list->lst d))]))

(make-lst-form Lst List? Null Pair lst->list)

#;
(define-match-expander Lst
  (λ (stx)
    (syntax-case stx (... ...)
      [(_ p (... ...)) #'(app lst->list (list p (... ...)))]
      [p (syntax-parse #'p
           #:datum-literals [|.|]
           [(_) #'(Null)]
           [(_ p ps ...) #'(Pair p (Lst ps ...))]

           [(_ . x:id) #'(? List? x)]
           [(_ p ps ... . x:id) #'(Pair p (Lst ps ... . x))]

           [p (syntax-case #'p (... ...)
                [(_ p (... ...)) #'(app lst->list (list p (... ...)))])])]))
  (λ (stx) (syntax-parse stx
              [(_) #'(Null)]
              [(_ x xs ...) #'(Pair x (Lst xs ...))]

              [(_ . xs:id)  #'(and (List? xs) xs)]
              [(_ y ys ... . x:id)  #'(Pair y (Lst ys ... . x))])))

(define l1 (Lst 1 2 3))

(match l1
  [(Lst a b c) a])
(match l1
  [(Lst x y . xs) (list x xs)])
(match l1
  [(Lst x y xs ...) (list x xs)])


(define l2 (Lst 4 5 . l1))
#;l2
