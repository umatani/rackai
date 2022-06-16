#lang racket
(require (for-syntax racket racket/syntax
                     syntax/parse syntax/stx))

(begin-for-syntax
  (define (find-defined-id e)
    (syntax-parse e
      #:literals (define)
      [(define x:id arg ...)
       #'x])))

;; 教訓： generate-temporaryは引数のsyntax objectのcontextを受け継がない！

(define-syntax (mydef-unit stx)
  (syntax-parse stx
    [(_ red-id:id e:expr)
     #:with unit-id (generate-temporary #'red-id)
     #:with sig-id (format-id #'red-id "~a^" #'red-id) #;(generate-temporary #'red-id)
     #:with defed-id (find-defined-id #'e)
     #`(begin
         (define-signature sig-id (defed-id))
         (define-unit unit-id (import) (export sig-id)
           (define defed-id 100)))]))



(mydef-unit --> (define X 100))

