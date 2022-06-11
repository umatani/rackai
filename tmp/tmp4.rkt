#lang racket
(require "../conc/base/reduction.rkt")

(define-signature X^
  (X))

(define-signature Y^
  (Y Y2))

(define-unit X@ (import) (export X^)
  (define X 200))

(define-unit Y@ (import) (export Y^)
  (define Y -1000)
  (define Y2 20000))

(define-compound-unit XY@ (import) (export x y)
  (link (([x : X^]) X@)
        (([y : Y^]) Y@)))

(define-reduction (--> <+>) #:within-signatures [X^ Y^]
  [(cons a b) (<+> a b X Y Y2) 'add])

(define reducer
  (reducer-of --> #:within-units [X@ Y@])
  ;; the above expands to the following code
  #;
  (invoke-unit
   (compound-unit
    (import) (export)
    (link (([x : X^]) X@)
          (([y : Y^]) Y@)
          (([r : -->]) (unit (import (prefix x: X^)
                                     (prefix y: Y^)) (export -->)
                         (define X  x:X)
                         (define Y  y:Y)
                         (define Y2 y:Y2)) x y)
          (() (unit (import -->) (export)
                -->) r)))))

((reducer *) (cons 3 4))

(define reducer2
  (reducer-of --> #:within-units [XY@])
  ;; the above expands to the following code
  #;
  (invoke-unit
   (compound-unit
    (import) (export)
    (link (([x : X^] [y : Y^]) XY@)
          (([r : -->]) (unit (import (prefix x: X^)
                                     (prefix y: Y^)) (export -->)
                         (define X  x:X)
                         (define Y  y:Y)
                         (define Y2 y:Y2)) x y)
          (() (unit (import -->) (export)
                -->) r)))))

((reducer2 +) (cons 3 4))
