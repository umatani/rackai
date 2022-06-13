#lang racket
(require "../reduction.rkt"
         (only-in "tmp2.rkt" --> -->^))

;; tmp2.rktからは --> ではなく -->^ が必要
(define-reduction ==> #:super (--> /)
  [(? number? x) (* x x) 'mul])

(((reducer-of ==>)) (cons 3 4) #;8)


;; 以下のように，unit内部で define-reductionを使うことはできない．
;; そのために #:within-signatures, #:within-unitsを導入．
;; 詳しくは tmp4.rkt参照
#;
(define-unit test@
  (import)
  (export)
  (define-reduction ==> #:super (--> /)
    [(? number? x) (* x x) 'mul])


  (((reducer-of ==>)) (cons 3 4) #;8))
#;
(invoke-unit test@)
