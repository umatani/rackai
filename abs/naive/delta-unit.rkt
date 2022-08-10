#lang racket/unit
(require
 racket/match
 "../../set.rkt"
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

; delta : Prim (Listof Val) -> (SetM Val)
(define (delta p vs)
  (match* (p vs)
    [((Prim '+) (list (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '-) (list (Num n) (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '*) (list (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '/) (list (Num n) (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '<) (list (Num n1) (Num n2) (Num ns) ...))
     (lift (set (Bool #t) (Bool #f)))]
    [((Prim '=) (list (Num n1) (Num n2) (Num ns) ...))
     (lift (set (Bool #t) (Bool #f)))]

    [((Prim 'eq?) (list (Sym s1) (Sym s2)))
     (lift (set (Bool #t) (Bool #f)))]

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