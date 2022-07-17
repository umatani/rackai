#lang racket/unit
(require
 racket/match
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt"
          terms-extra^ delta^)
 (only-in "core-terms.rkt" terms^ #%term-forms))

(import (only terms^
              Bool% Num% Sym% Stx%)
        (only terms-extra^
              stx? atom?))
(export delta^)

(use-terms Bool Num Sym Stx)

;; ----------------------------------------
;; Implementation of primitives:

(define (plus . ns) (apply + ns))
(define (minus n . ns) (apply - n ns))
(define (times . ns) (apply * ns))
(define (div n . ns) (apply / n ns))
(define (less-than n1 n2 . ns) (apply < n1 n2 ns))
(define (num-eq n1 n2 . ns) (apply = n1 n2 ns))
(define (sym-eq s1 s2) (eq? s1 s2))

; delta : Prim (Listof Val) -> Val
(define (delta p vs)
  (match (cons p vs)
    [`(+ ,(Num ns) ...)          (Num 'num-⊤)]
    [`(- ,(Num n) ,(Num ns) ...) (Num 'num-⊤)]
    [`(* ,(Num ns) ...)          (Num 'num-⊤)]
    [`(/ ,(Num n) ,(Num ns) ...) (Num 'num-⊤)]
    [`(< ,(Num n1) ,(Num n2) ,(Num ns) ...) (Bool 'bool-⊤)]
    [`(= ,(Num n1) ,(Num n2) ,(Num ns) ...) (Bool 'bool-⊤)]

    [`(eq? ,(Sym s1) ,(Sym s2)) (Bool 'bool-⊤)]

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
    [`(datum->syntax ,(and stx (Stx _ ctx_0)) (,v1 ,vs ...))
     (Stx `(,(delta 'datum->syntax `(,stx ,v1))
            ,@(delta 'syntax-e `(,(delta 'datum->syntax `(,stx ,vs)))))
          ctx_0)]
    [`(datum->syntax ,(Stx _ ctx) ,(? atom? atom))
     (Stx atom ctx)]))
