#lang racket
(require (except-in "../../conc/base/core/struct.rkt"
                    Atom? Val? Tm?))
(provide (all-defined-out))
(provide (all-from-out "../../conc/base/core/struct.rkt"))

(define (bool? x) (or (boolean? x) (eq? x 'bool)))
(define (num? x)  (or (real? x) (eq? x 'num)))

(define (Atom? x)
  (or (null? x)
      (bool? x) ;; changed
      (num? x)    ;; changed
      (Sym? x) (Prim? x) (𝓁? x) (Defs? x)))

;; dependencies
(define (Val? x)
  (define r (or (VFun? x)
                (Atom? x)
                (and (pair? x) (Val? (car x)) (Val? (cdr x)))
                (Stx? x)
                (LBind2? x)))
  (printf "Val?: ~a ~a\n" x r)
  r)

(define (Tm? x) (or (Val? x) (Ser? x)))
