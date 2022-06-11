#lang racket
(require "struct-sig.rkt")
(provide delta^ delta@)

(define-signature delta^
  (delta))

(define-unit delta@
  (import (only struct^ Sym Stx stx stx? Sym? atom?))
  (export delta^)

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

  ;(: delta : Prim (Listof Val) -> Val)
  (define (delta p vs)
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
      [`(list ,v1 ,vs ...) (delta 'cons (list v1 (delta 'list vs)))]
      [`(second (,_ ,v2 ,_ ...)) v2]
      [`(third  (,_ ,_ ,v3 ,_ ...)) v3]
      [`(fourth (,_ ,_ ,_ ,v4 ,_ ...)) v4]

      ;; for debug
      [`(printe ,v1 ,v2) (println v1) v2]


      [`(syntax-e ,(Stx e _)) e]
      [`(datum->syntax ,_ ,(? stx? stx)) stx]
      [`(datum->syntax ,(and stx0 (Stx _ ctx_0)) (,v1 ,vs ...))
       (stx `(,(delta 'datum->syntax `(,stx0 ,v1))
              ,@(delta 'syntax-e `(,(delta 'datum->syntax `(,stx0 ,vs)))))
            ctx_0)]
      [`(datum->syntax ,(Stx _ ctx) ,(? atom? atom))
       (stx atom ctx)]))
  )

