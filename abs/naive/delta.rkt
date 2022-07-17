#lang racket
(require
 "../../mix.rkt"
 (only-in "../../term.rkt"  use-terms)
 (only-in "../../signatures.rkt"
          terms-extra^ delta^)
 ;(only-in "../../terms.rkt" use-lst-form)
 (only-in "core-terms.rkt" terms^ #%term-forms)

 (only-in "../../conc/delta-unit.rkt" [delta@ conc:delta@]))
(provide delta@)

;; ----------------------------------------
;; Implementation of primitives:

(define-mixed-unit delta@
  (import (only terms^
                Atom% Bool% Num% Sym% Stx% Null% Pair% Prim%)
          (only terms-extra^
                stx?))
  (export delta^)
  (inherit [conc:delta@ prim? stx-prim?])

  (use-terms Atom Bool Num Sym Stx Null Pair Prim)

  ; delta : Prim (Listof Val) -> Val
  (define (delta p vs)
    (match* (p vs)
      [((Prim '+) (list (Num ns) ...))
       (Num 'n-⊤)]
      [((Prim '-) (list (Num n) (Num ns) ...))
       (Num 'n-⊤)]
      [((Prim '*) (list (Num ns) ...))
       (Num 'n-⊤)]
      [((Prim '/) (list (Num n) (Num ns) ...))
       (Num 'n-⊤)]
      [((Prim '<) (list (Num n1) (Num n2) (Num ns) ...))
       (Bool 'b-⊤)]
      [((Prim '=) (list (Num n1) (Num n2) (Num ns) ...))
       (Bool 'b-⊤)]

      [((Prim 'eq?) (list (Sym s1) (Sym s2)))
       (Bool 'b-⊤)]

      [((Prim 'cons) (list v1 v2))
       (Pair v1 v2)]
      [((Prim 'car) (list (Pair v1 _)))
       v1]
      [((Prim 'cdr) (list (Pair _ v2)))
       v2]

      [((Prim 'list) (list))
       (Null)]
      [((Prim 'list) (list v1 vs ...))
       (delta (Prim 'cons) (list v1 (delta (Prim 'list) vs)))]
      [((Prim 'second) (list (Pair _ (Pair v2 _))))
       v2]
      [((Prim 'third)  (list (Pair _ (Pair _ (Pair v3 _)))))
       v3]
      [((Prim 'fourth) (list (Pair _ (Pair _ (Pair _ (Pair v4 _))))))
       v4]

      ;; for debug
      [((Prim 'printe) (list v1 v2))
       (println v1)
       v2]

      [((Prim 'syntax-e) (list (Stx e _)))
       e]
      [((Prim 'datum->syntax) (list _ (? stx? stx)))
       stx]
      [((Prim 'datum->syntax) (list (Stx _ ctx) (Null)))
       (Stx (Null) ctx)]
      [((Prim 'datum->syntax) (list (and stx (Stx _ ctx_0))
                                    (Pair v1 vs)))
       (Stx (Pair (delta (Prim 'datum->syntax) (list stx v1))
                  (delta (Prim 'syntax-e)
                         (list (delta (Prim 'datum->syntax) (list stx vs)))))
            ctx_0)]
      [((Prim 'datum->syntax) (list (Stx _ ctx) (? Atom? atom)))
       (Stx atom ctx)])))
