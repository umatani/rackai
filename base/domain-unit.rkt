#lang racket/unit
(require
 (only-in racket/function identity)
 (only-in racket/match    match*)
 (only-in racket/pretty   pretty-print)
 (only-in "../set.rkt"    set ⊆)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../syntax.rkt" stx->datum))

(import)
(export domain^)

;; ----------------------------------------
;; Implementation of Domains:

;; α : Val → (Setof Val)
(define α  set)

;; ≤ₐ : (Setof Val) (Setof Val) → Boolean
(define ≤ₐ ⊆)

(define (plus          . ns) (apply +     ns))
(define (minus       n . ns) (apply -   n ns))
(define (times         . ns) (apply *     ns))
(define (div         n . ns) (apply /   n ns))
(define (less-than m n . ns) (apply < m n ns))
(define (num-eq    m n . ns) (apply = m n ns))
(define (sym-eq         s t) (eq?        s t))

; δ : Prim (Listof Val) → Val
(define (δ p vs)
  (match* (p vs)
    [((Prim '+ _) `(         ,(Num ns) ...)) (Num (apply plus ns))]
    [((Prim '- _) `(,(Num n) ,(Num ns) ...)) (Num (apply minus n ns))]
    [((Prim '* _) `(         ,(Num ns) ...)) (Num (apply times ns))]
    [((Prim '/ _) `(,(Num n) ,(Num ns) ...)) (Num (apply div n ns))]
    [((Prim '< _) `(,(Num m) ,(Num n) ,(Num ns) ...))
     (Bool (apply less-than m n ns))]
    [((Prim '= _) `(,(Num m) ,(Num n) ,(Num ns) ...))
     (Bool (apply num-eq m n ns))]

    [((Prim 'eq?  _) `(,(Sym s) ,(Sym t))) (Bool (sym-eq s t))]
    [((Prim 'cons _) `(,v1 ,v2))           (Pair v1 v2)]
    [((Prim 'car  _) `(,(Pair v _)))       v]
    [((Prim 'cdr  _) `(,(Pair _ v)))       v]

    [((Prim 'list _) `())     (Null)]
    [((Prim 'list _) `(,v ,vs ...))
     (δ (Prim 'cons #f) `(,v ,(δ (Prim 'list #f) vs)))]

    [((Prim 'second _) `(,(Pair _ (Pair v _                  )))) v]
    [((Prim 'third  _) `(,(Pair _ (Pair _ (Pair v _         ))))) v]
    [((Prim 'fourth _) `(,(Pair _ (Pair _ (Pair _ (Pair v _)))))) v]

    [((Prim 'syntax-e      _) `(,(Stx e _))) e]
    [((Prim 'syntax->datum _) `(,v))         (stx->datum v)]

    [((Prim 'datum->syntax _) `(,_           ,(? Stx? stx)))   stx]
    [((Prim 'datum->syntax _) `(,(Stx _ ctx) ,(Null)))         (Stx (Null) ctx)]
    [((Prim 'datum->syntax _) `(,(Stx _ ctx) ,(? Atom? atom))) (Stx atom ctx)]
    [((Prim 'datum->syntax _) `(,(and stx (Stx _ ctx)) ,(Pair v vs)))
     (Stx (Pair (δ (Prim 'datum->syntax #f) `(,stx ,v))
                (δ (Prim 'syntax-e #f)
                       `(,(δ (Prim 'datum->syntax #f) `(,stx ,vs)))))
          ctx)]

    ;; for debug
    [((Prim 'printe _) `(,u ,v))
     (pretty-print (lst->list/recur u))
     v]))

;; val? : Ast → Boolean
(define val? Val?)

;; stx? : Val → Boolean
(define (stx? x)
  (or (and (Stx? x) (Atom? (Stx-e x)))
      (and (Stx? x) (prim? (Stx-e x)))
      (and (Stx? x) (Pair? (Stx-e x))
           (stx? (Pair-a (Stx-e x)))
           (stl? (Pair-d (Stx-e x))))
      (and (Stx? x) (proper-stl? (Stx-e x)))
      (Stxξ? x)
      (Hole? x)
      (and (Stx? x) (Hole? (Stx-e x)))))

;; stl? : Val → Boolean
(define (stl? x)
  (or (Null? x)
      (stx? x)
      (and (Pair? x) (stx? (Pair-a x)) (stl? (Pair-d x)))
      (Hole? x)))

;; proper-stl? : Val → Boolean
(define (proper-stl? x)
  (or (Null? x)
      (and (Pair? x) (stx? (Pair-a x)) (proper-stl? (Pair-d x)))))
