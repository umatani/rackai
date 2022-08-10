#lang racket/unit
(require
 racket/match
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" terms-extra^ domain^)

 (only-in "../../terms.rkt" #%term-forms
          Atom% Bool% Num% Sym% Stx% Null% Pair% Prim%)
 )

(import (only terms-extra^
              stx?))
(export domain^)

(define α 1)
(define ≤a 1)

;; ----------------------------------------
;; Implementation of primitives:

(use-terms Atom Bool Num Sym Stx Null Pair Prim)

; delta : Prim (Listof Val) -> (SetM Val)
(define (delta p vs)
  (match* (p vs)
    [((Prim '+ _) (list (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '- _) (list (Num n) (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '* _) (list (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '/ _) (list (Num n) (Num ns) ...))
     (pure (Num 'num-⊤))]
    [((Prim '< _) (list (Num n1) (Num n2) (Num ns) ...))
     (lift (set (Bool #t) (Bool #f)))]
    [((Prim '= _) (list (Num n1) (Num n2) (Num ns) ...))
     (lift (set (Bool #t) (Bool #f)))]

    [((Prim 'eq? _) (list (Sym s1) (Sym s2)))
     (lift (set (Bool #t) (Bool #f)))]

    [((Prim 'cons _) (list v1 v2))
     (pure (Pair v1 v2))]
    [((Prim 'car _) (list (Pair v1 _)))
     (pure v1)]
    [((Prim 'cdr _) (list (Pair _ v2)))
     (pure v2)]

    [((Prim 'list _) (list))
     (pure (Null))]

    [((Prim 'list _) (list v1 vs ...))
     (do l <- (delta (Prim 'list #f) vs)
         (delta (Prim 'cons #f) (list v1 l)))]

    [((Prim 'second _) (list (Pair _ (Pair v2 _))))
     (pure v2)]
    [((Prim 'third _)  (list (Pair _ (Pair _ (Pair v3 _)))))
     (pure v3)]
    [((Prim 'fourth _) (list (Pair _ (Pair _ (Pair _ (Pair v4 _))))))
     (pure v4)]

    ;; for debug
    [((Prim 'printe _) (list v1 v2))
     (println v1)
     (pure v2)]

    [((Prim 'syntax-e _) (list (Stx e _)))
     (pure e)]
    [((Prim 'datum->syntax _) (list _ (? stx? stx)))
     (pure stx)]
    [((Prim 'datum->syntax _) (list (Stx _ ctx) (Null)))
     (pure (Stx (Null) ctx))]
    [((Prim 'datum->syntax _) (list (and stx (Stx _ ctx_0))
                                  (Pair v1 vs)))
     (do s1 <- (delta (Prim 'datum->syntax #f) (list stx v1))
         ss <- (delta (Prim 'datum->syntax #f) (list stx vs))
         d  <- (delta (Prim 'syntax-e #f) (list ss))
         (pure (Stx (Pair s1 d) ctx_0)))]
    [((Prim 'datum->syntax _) (list (Stx _ ctx) (? Atom? atom)))
     (pure (Stx atom ctx))]))
