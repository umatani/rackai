#lang racket
(require
 "../set.rkt"
 "../nondet.rkt"
 "../mix.rkt"
 (only-in "../term.rkt" use-terms)
 
 (only-in "../signatures.rkt" domain^)
 (only-in "../terms.rkt" #%term-forms
          Atom% Bool% Num% Sym% Stx% Null% Pair% Prim%
          lst->list/recur stx->datum)
 (only-in "../conc/domain-unit.rkt" [domain@ base:domain@]))
(provide domain@)

;; ----------------------------------------
;; Implementation of Domains:

(define-mixed-unit domain@
  (import)
  (export domain^)
  (inherit [base:domain@ val? stx? stl? proper-stl?])

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

  ; delta : Prim (Listof Val) -> (SetM Val)
  (define (delta p vs)
    (match* (p vs)
      [((Prim '+ _) (list (Num ns) ...))
       (pure (Num (apply plus ns)))]
      [((Prim '- _) (list (Num n) (Num ns) ...))
       (pure (Num (apply minus n ns)))]
      [((Prim '* _) (list (Num ns) ...))
       (pure (Num (apply times ns)))]
      [((Prim '/ _) (list (Num n) (Num ns) ...))
       (pure (Num (apply div n ns)))]
      [((Prim '< _) (list (Num n1) (Num n2) (Num ns) ...))
       (pure (Bool (apply less-than n1 n2 ns)))]
      [((Prim '= _) (list (Num n1) (Num n2) (Num ns) ...))
       (pure (Bool (apply num-eq n1 n2 ns)))]

      [((Prim 'eq? _) (list (Sym s1) (Sym s2)))
       (pure (Bool (sym-eq s1 s2)))]

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

      [((Prim 'syntax-e _) (list (Stx e _)))
       (pure e)]
      [((Prim 'syntax->datum _) (list v))
       (pure (stx->datum v))]
      
      [((Prim 'datum->syntax _) (list _ (? Stx? stx)))
       (pure stx)]
      [((Prim 'datum->syntax _) (list (Stx _ ctx) (Null)))
       (pure (Stx (Null) ctx))]

      [((Prim 'datum->syntax _) (list (and stx (Stx _ ctx_0))
                                      (Pair v1 vs)))
       (do s1 <- (delta (Prim 'datum->syntax #f) (list stx v1))
           ss <- (delta (Prim 'datum->syntax #f) (list stx vs))
           d  <- (delta (Prim 'syntax-e      #f) (list ss))
           (pure (Stx (Pair s1 d) ctx_0)))]

      [((Prim 'datum->syntax _) (list (Stx _ ctx) (? Atom? atom)))
       (pure (Stx atom ctx))]

      ;; for debug
      [((Prim 'printe _) (list v1 v2))
       (pretty-print (lst->list/recur v1))
       (pure v2)]))
  )
