#lang racket
(require "../reduction.rkt")

;;;;;;;; 1 ;;;;;;;;;;

(define-reduction (-->/+ <+>)
  [(cons a b) (<+> a b) 'add])

(define reducer1 (reducer-of -->/+))
((reducer1 +) (cons 3 4))

;;;;;;;; 2 ;;;;;;;;;;

(define-signature X^ (X))

(define-reduction (--->/+ <+>) #:within-signatures [X^]
  [(cons a b) (<+> a b X) 'add])

(define-unit X@ (import) (export X^)
  (define X 100))

(define reducer2 (reducer-of --->/+ #:within-units [X@]))
((reducer2 +) (cons 3 4))

;;;;;;;; 3 ;;;;;;;;;;

(define-reduction ==> #:super (-->/+ *))

(define reducer3 (reducer-of ==>))
((reducer3) (cons 3 4))

;;;;;;;; 4 ;;;;;;;;;;

(define-reduction ===> #:super (--->/+ *) #:within-signatures [X^])

(define reducer4 (reducer-of ===> #:within-units [X@]))
((reducer4) (cons 3 4))

;;;;;;;; 5 ;;;;;;;;;;
;;;; #:do [...]

(define-reduction (~~>/+ <+>) #:within-signatures [X^]
  #:do [(define Y 300)
        (println 'HOGEEEE-IN-DO)
        ;(define-syntax (m stx) #'(dbgX 'M))
        (define (dbgX msg) (println msg) (+ Y X))]
  [(cons a b)
   ;(+ Y 2)
   (<+> a b #;(m) (dbgX 'HOGEE) Y)
   'add])

(define reducer5 (reducer-of ~~>/+ #:within-units [X@]))
((reducer5 +) (cons 3 4))

;;;;;;;; 6 ;;;;;;;;;;
;;;; #:do [...] を含む reducton を，その定義を含めて継承

(define-reduction ~~~> #:super (~~>/+ *) #:within-signatures [X^])

(define reducer6 (reducer-of ~~~> #:within-units [X@]))
((reducer6) (cons 3 4))

;;;;;;;; 7 ;;;;;;;;;;
;;;; #:do [...] 定義のオーバーライド

(define-reduction ~~~>2 #:super ~~~> #;(~~>/+ *) #:within-signatures [X^]
  #:do [(println 'SUBBBB)
        (define Y (+ Z Z))
        (define Z 999)])

(define reducer7 (reducer-of ~~~>2 #:within-units [X@]))
((reducer7) (cons 3 4))