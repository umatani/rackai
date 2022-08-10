#lang racket
(require (for-syntax racket racket/syntax racket/unit-exptime
                     syntax/parse syntax/stx syntax/id-set
                     syntax/strip-context))

;; マクロが (import (prefix p: parse^)) を生成できるか

(define-signature X^ (X))

(define-unit X@
  (import)
  (export X^)
  (define X 100))

;; OK  (Id X@ だけを syntax-local-introduceもOK)
#;
(define-syntax (gen-unit stx)
  (syntax-parse stx
    [(_)
     (syntax-local-introduce #'(define-unit X@
                                 (import)
                                 (export X^)
                                 (define X 100)))]))

;; これはNG．syntax-local-identifier-as-bindingはこう使うんじゃない．
#;
(define-syntax (gen-unit stx)
  (syntax-parse stx
    [(_)
     #`(define-unit #,(syntax-local-identifier-as-binding #'X@)
         (import)
         (export X^)
         (define X 100))]))

(define-syntax (gen-unit stx)
  (define (signatures-of unit-id)
    (call-with-values
     (λ () (unit-static-signatures unit-id unit-id))
     (λ (is os)
       (unless (= (length os) 1)
         (raise-syntax-error 'define-mixed-unit
                             "multiple exports not supperted"
                             (map (compose1 syntax->datum cdr) os)))
       (list (map cdr is) os))))

  (syntax-parse stx
    [(_ u:id x:id)
     #:with (() ((_ . sig))) (signatures-of #'u)
     #:with X (syntax-local-introduce (format-id #'x "p:~a" #'x))
     #`(define-unit #,(syntax-local-introduce #'Y@)
         (import (prefix p: sig))
         (export)
         (define Y (+ X 1)))]))

(gen-unit X@ X)

Y@
