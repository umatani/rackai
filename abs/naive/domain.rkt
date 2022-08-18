#lang racket
(require
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" domain^)

 (only-in "../../terms.rkt" #%term-forms
          Val% Atom% List% Bool% Num% Sym% Stx% Stxξ% Null% Pair% Prim% Hole%
          lst->list/recur prim?))
(provide domain@ val-⊤ atom-⊤ num-⊤ sym-⊤ stx-⊤ list-⊤ ≤e)

;; ----------------------------------------
;; Implementation of Domains:

(use-terms Val Atom List Bool Num Sym Stx Stxξ Null Pair Prim Hole)

(define val-⊤  (Val))
(define atom-⊤ (Atom))
(define num-⊤  (Num 'num-⊤))
(define sym-⊤  (Sym 'sym-⊤))
(define stx-⊤  (Stx 'stx-⊤ (set)))
(define list-⊤ (List))

(define (≤e v1 v2)
  (or (equal? v1 v2)
      (match* (v1 v2)
        [(_         val-⊤)  #t]
        [((? Atom?) atom-⊤) #t]
        [(val-⊤     atom-⊤) #f]
        [((? Num?)  num-⊤)  #t]
        [(_         num-⊤)  #f]
        [((? Sym?)  sym-⊤)  #t]
        [(_         sym-⊤)  #f]
        [((? Stx?)  stx-⊤)  #t]
        [(_         stx-⊤)  #f]
        [((? List?) list-⊤) #t]
        [(_         list-⊤) #f]
        [(_ _) #f])))

(define-unit domain@
  (import)
  (export domain^)
  
  (define (α vs) vs)
  (define (≤a vs1 vs2)
    (define ((∈a vs) v1) (ormap (λ (v2) (≤e v1 v2)) vs))
    (andmap (∈a (set->list vs2)) (set->list vs1)))

  ; delta : Prim (Listof Val) -> (SetM Val)
  (define (delta p vs)
    (match* (p vs)
      [((Prim (? (λ (op) (or (eq? op '+) (eq? op '*)))) _)
        (list (Num ns) ...))
       (pure num-⊤)]
      [((Prim (? (λ (op) (or (eq? op '+) (eq? op '*)))) _)
        (list (? (λ (x) (or (Num? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤)))) ...))
       (pure num-⊤)]
      [((Prim (? (λ (op) (or (eq? op '+) (eq? op '*)))) _)
        (list _ ...))
       (lift (set))]

      [((Prim (? (λ (op) (or (eq? op '-) (eq? op '/)))) _)
        (list (Num n) (Num ns) ...))
       (pure num-⊤)]
      [((Prim (? (λ (op) (or (eq? op '-) (eq? op '/)))) _)
        (list (? (λ (x) (or (Num? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤))))
              (? (λ (x) (or (Num? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤)))) ...))
       (pure num-⊤)]
      [((Prim (? (λ (op) (or (eq? op '-) (eq? op '/)))) _)
        (list _ ...))
       (lift (set))]

      [((Prim (? (λ (op) (or (eq? op '<) (eq? op '=)))) _)
        (list (Num n) (Num ns) ...))
       (lift (set (Bool #t) (Bool #f)))]
      [((Prim (? (λ (op) (or (eq? op '<) (eq? op '=)))) _)
        (list (? (λ (x) (or (Num? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤))))
              (? (λ (x) (or (Num? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤)))) ...))
       (lift (set (Bool #t) (Bool #f)))]
      [((Prim (? (λ (op) (or (eq? op '<) (eq? op '=)))) _)
        (list _ ...))
       (lift (set))]

      [((Prim 'eq? _) (list v1 v2))
       (lift (set (Bool #t) (Bool #f)))]
      [((Prim 'eq? _) (list _ ...))
       (lift (set))]

      [((Prim 'cons _) (list v1 v2))
       (pure list-⊤)]
      [((Prim 'cons _) (list _ ...))
       (lift (set))]

      [((Prim 'list _) (list vs ...))
       (pure list-⊤)]

      [((Prim (? (λ (op) (or (eq? op 'car) (eq? op 'cdr)
                              (eq? op 'second) (eq? op 'third)
                              (eq? op 'fourth)))) _)
        (list (? (λ (x) (or (Pair? x)
                             (equal? x list-⊤)
                             (equal? x val-⊤))))))
       (pure val-⊤)]
      [((Prim (? (λ (op) (or (eq? op 'car) (eq? op 'cdr)))) _)
        (list _ ...))
       (list (set))]

      [((Prim 'syntax-e _)
        (list (? (λ (x) (or (Stx? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤))))))
       (pure val-⊤)]
      [((Prim 'syntax-e _) (list _ ...))
       (lift (set))]

      [((Prim 'syntax->datum _)
        (list (? (λ (x) (or (Stx? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤))))))
       (pure val-⊤)]
      [((Prim 'syntax->datum _) (list _ ...))
       (lift (set))]

      [((Prim 'datum->syntax _)
        (list (? (λ (x) (or (Stx? x)
                             (equal? x atom-⊤)
                             (equal? x val-⊤))))
              v))
       (pure stx-⊤)]
      [((Prim 'datum->syntax _) (list _ ...))
       (lift (set))]

      ;; for debug
      [((Prim 'printe _) (list v1 v2))
       (pretty-print (lst->list/recur v1))
       (pure v2)]))

  ;; adapt to abstract value

  (define (val? x)
    (or (Val? x)
        (and (Pair? x) (val? (Pair-a x)) (val? (Pair-d x)))
        (stx? x)
        (equal? x stx-⊤)))

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

  (define (stl? x)
    (or (Null? x) (stx? x)
        (and (Pair? x) (stx? (Pair-a x)) (stl? (Pair-d x)))
        (Hole? x)))

  (define (proper-stl? x)
    (or (Null? x)
        (and (Pair? x) (stx? (Pair-a x)) (proper-stl? (Pair-d x))))))
