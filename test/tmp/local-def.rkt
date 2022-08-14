#lang racket

#;
(define-syntax (def stx)
  (syntax-case stx ()
    [(_ id exp)
     (identifier? #'id)
     #'(define id exp)]
    [(_ (id arg ...) body ...)
     #'(def id (lambda (arg ...) body ...))]))


#;
(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ (def id expr) ... body)
     #'(letrec ([id expr] ...)
         body)]))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ form ...) #'((lambda () form ...))]))




#;
(letrec-values ([(a) 3]
                [(even?) (lambda (x) (if (= x 0)
                                         #t
                                         (odd? (- x 1))))]
                [(odd?) (lambda (x) (if (= x 0)
                                        #f
                                        (even? (- x 1))))]
                
                [() (begin (println (even? a)) (values))])
  (odd? a))


(my-module-begin
 (def a 3)
 (def even? (lambda (x) (if (= x 0)
                            #t
                            (odd? (- x 1)))))
 (def odd? (lambda (x) (if (= x 0)
                           #f
                           (even? (- x 1)))))
 (define-syntax (def stx)
   (syntax-case stx ()
     [(_ id exp)
      (identifier? #'id)
      #'(define id exp)]
     [(_ (id arg ...) body ...)
      #'(def id (lambda (arg ...) body ...))]))
 (println (even? a))
 (odd? a))
