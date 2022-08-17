#lang racket/unit
(require
 racket/match (only-in racket/function identity) racket/pretty
 "../set.rkt"
 (only-in "../term.rkt" use-terms)
 
 (only-in "../signatures.rkt" domain^)
 (only-in "../terms.rkt" #%term-forms
          Atom% Bool% Num% Sym% Stx% Null% Pair% Prim%
          lst->list/recur stx->datum))

(import)
(export domain^)

;; ----------------------------------------
;; Implementation of Domains:

(define α  identity)
(define ≤a subset?)

(use-terms Atom Bool Num Sym Stx Null Pair Prim)

(define (plus . ns) (apply + ns))
(define (minus n . ns) (apply - n ns))
(define (times . ns) (apply * ns))
(define (div n . ns) (apply / n ns))
(define (less-than n1 n2 . ns) (apply < n1 n2 ns))
(define (num-eq n1 n2 . ns) (apply = n1 n2 ns))
(define (sym-eq s1 s2) (eq? s1 s2))

; delta : Prim (Listof Val) -> Val
(define (delta p vs)
  (match* (p vs)
    [((Prim '+ _) (list (Num ns) ...))
     (Num (apply plus ns))]
    [((Prim '- _) (list (Num n) (Num ns) ...))
     (Num (apply minus n ns))]
    [((Prim '* _) (list (Num ns) ...))
     (Num (apply times ns))]
    [((Prim '/ _) (list (Num n) (Num ns) ...))
     (Num (apply div n ns))]
    [((Prim '< _) (list (Num n1) (Num n2) (Num ns) ...))
     (Bool (apply less-than n1 n2 ns))]
    [((Prim '= _) (list (Num n1) (Num n2) (Num ns) ...))
     (Bool (apply num-eq n1 n2 ns))]

    [((Prim 'eq? _) (list (Sym s1) (Sym s2)))
     (Bool (sym-eq s1 s2))]

    [((Prim 'cons _) (list v1 v2))
     (Pair v1 v2)]
    [((Prim 'car _) (list (Pair v1 _)))
     v1]
    [((Prim 'cdr _) (list (Pair _ v2)))
     v2]

    [((Prim 'list _) (list))
     (Null)]
    [((Prim 'list _) (list v1 vs ...))
     (delta (Prim 'cons #f) (list v1 (delta (Prim 'list #f) vs)))]
    [((Prim 'second _) (list (Pair _ (Pair v2 _))))
     v2]
    [((Prim 'third _)  (list (Pair _ (Pair _ (Pair v3 _)))))
     v3]
    [((Prim 'fourth _) (list (Pair _ (Pair _ (Pair _ (Pair v4 _))))))
     v4]

    [((Prim 'syntax-e _) (list (Stx e _)))
     e]
    [((Prim 'syntax->datum _) (list v))
     (stx->datum v)]

    [((Prim 'datum->syntax _) (list _ (? Stx? stx)))
     stx]
    [((Prim 'datum->syntax _) (list (Stx _ ctx) (Null)))
     (Stx (Null) ctx)]
    [((Prim 'datum->syntax _) (list (and stx (Stx _ ctx_0))
                                    (Pair v1 vs)))
     (Stx (Pair (delta (Prim 'datum->syntax #f) (list stx v1))
                (delta (Prim 'syntax-e #f)
                       (list (delta (Prim 'datum->syntax #f) (list stx vs)))))
          ctx_0)]
    [((Prim 'datum->syntax _) (list (Stx _ ctx) (? Atom? atom)))
     (Stx atom ctx)]

    ;; for debug
    [((Prim 'printe _) (list v1 v2))
     (pretty-print (lst->list/recur v1))
     v2]))
