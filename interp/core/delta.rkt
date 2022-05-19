#lang racket
(require "struct.rkt")
(provide δ)

;; ----------------------------------------
;; Implementation of primitives:

(define (plus . ns) (apply + ns))
(define (minus n . ns) (apply - n ns))
(define (times . ns) (apply * ns))
(define (div n . ns) (apply / n ns))
(define (less-than n1 n2 . ns) (apply < n1 n2 ns))
(define (num-eq n1 n2 . ns) (apply = n1 n2 ns))
(define (sym-eq sym1 sym2)
  (match* (sym1 sym2)
    [((Sym nam1) (Sym nam2)) (eq? nam1 nam2)]))

(define (δ p vs)
  (match (cons p vs)
    [`(+ ,(? real? ns) ...)
     (apply plus ns)]
    [`(- ,(? real? n) ,(? real? ns) ...)
     (apply minus n ns)]
    [`(* ,(? real? ns) ...)
     (apply times ns)]
    [`(/ ,(? real? n) ,(? real? ns) ...)
     (apply div n ns)]
    [`(< ,(? real? n1) ,(? real? n2) ,(? real? ns) ...)
     (apply less-than n1 n2 ns)]
    [`(= ,(? real? n1) ,(? real? n2) ,(? real? ns) ...)
     (apply num-eq n1 n2 ns)]

    [`(eq? ,(? Sym? s1) ,(? Sym? s2)) (sym-eq s1 s2)]

    [`(cons ,v1 ,v2) (cons v1 v2)]
    [`(car ,(cons v1 _)) v1]
    [`(cdr ,(cons _ v2)) v2]

    [`(list) '()]
    [`(list ,v1 ,vs ...) (δ 'cons (list v1 (δ 'list vs)))]
    [`(second (,_ ,v2 ,_ ...)) v2]
    [`(third  (,_ ,_ ,v3 ,_ ...)) v3]
    [`(fourth (,_ ,_ ,_ ,v4 ,_ ...)) v4]

    ;; for debug
    [`(printe ,v1 ,v2) (println v1) v2]


    [`(syntax-e ,(GenStx e _)) e]
    [`(datum->syntax ,_ ,(? Stx? stx)) stx]
    [`(datum->syntax ,(and stx0 (GenStx _ ctx_0)) (,v1 ,vs ...))
     (GenStx `(,(δ 'datum->syntax `(,stx0 ,v1))
               ,@(δ 'syntax-e `(,(δ 'datum->syntax `(,stx0 ,vs)))))
             ctx_0)]
    [`(datum->syntax ,(GenStx _ ctx) ,(? Atom? atom))
     (GenStx atom ctx)]))
