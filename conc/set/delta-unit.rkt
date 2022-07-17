#lang racket/unit
(require
 racket/match
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" terms-extra^ delta^)
 (only-in "../../terms.rkt"      terms^ #%term-forms))

(import (only terms^
              Atom% Bool% Num% Sym% Stx% Null% Pair% Prim%)
        (only terms-extra^
              stx?))
(export delta^)

;; ----------------------------------------
;; Implementation of primitives:

(use-terms Atom Bool Num Sym Stx Null Pair Prim)

(define (plus . ns) (apply + ns))
(define (minus n . ns) (apply - n ns))
(define (times . ns) (apply * ns))
(define (div n . ns) (apply / n ns))
(define (less-than n1 n2 . ns) (apply < n1 n2 ns))
(define (num-eq n1 n2 . ns) (apply = n1 n2 ns))
(define (sym-eq s1 s2) (eq? s1 s2))

; delta : Prim (Listof Val) -> (SetM Val)
(define (delta p vs)
  (match* (p vs)
    [((Prim '+) (list (Num ns) ...))
     (pure (Num (apply plus ns)))]
    [((Prim '-) (list (Num n) (Num ns) ...))
     (pure (Num (apply minus n ns)))]
    [((Prim '*) (list (Num ns) ...))
     (pure (Num (apply times ns)))]
    [((Prim '/) (list (Num n) (Num ns) ...))
     (pure (Num (apply div n ns)))]
    [((Prim '<) (list (Num n1) (Num n2) (Num ns) ...))
     (pure (Bool (apply less-than n1 n2 ns)))]
    [((Prim '=) (list (Num n1) (Num n2) (Num ns) ...))
     (pure (Bool (apply num-eq n1 n2 ns)))]

    [((Prim 'eq?) (list (Sym s1) (Sym s2)))
     (pure (Bool (sym-eq s1 s2)))]

    [((Prim 'cons) (list v1 v2))
     (pure (Pair v1 v2))]
    [((Prim 'car) (list (Pair v1 _)))
     (pure v1)]
    [((Prim 'cdr) (list (Pair _ v2)))
     (pure v2)]

    [((Prim 'list) (list))
     (pure (Null))]

    [((Prim 'list) (list v1 vs ...))
     (do l <- (delta (Prim 'list) vs)
         (delta (Prim 'cons) (list v1 l)))]

    [((Prim 'second) (list (Pair _ (Pair v2 _))))
     (pure v2)]
    [((Prim 'third)  (list (Pair _ (Pair _ (Pair v3 _)))))
     (pure v3)]
    [((Prim 'fourth) (list (Pair _ (Pair _ (Pair _ (Pair v4 _))))))
     (pure v4)]

    ;; for debug
    [((Prim 'printe) (list v1 v2))
     (println v1)
     (pure v2)]

    [((Prim 'syntax-e) (list (Stx e _)))
     (pure e)]
    [((Prim 'datum->syntax) (list _ (? stx? stx)))
     (pure stx)]
    [((Prim 'datum->syntax) (list (Stx _ ctx) (Null)))
     (pure (Stx (Null) ctx))]



    [((Prim 'datum->syntax) (list (and stx (Stx _ ctx_0))
                                  (Pair v1 vs)))
     (do s1 <- (delta (Prim 'datum->syntax) (list stx v1))
         ss <- (delta (Prim 'datum->syntax) (list stx vs))
         d  <- (delta (Prim 'syntax-e) (list ss))
         (pure (Stx (Pair s1 d) ctx_0)))]

    [((Prim 'datum->syntax) (list (Stx _ ctx) (? Atom? atom)))
     (pure (Stx atom ctx))]))
