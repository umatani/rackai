#lang racket
(require
 (only-in "../../set.rkt"    set)
 (only-in "../../nondet.rkt" pure lift)
 "../../signatures.rkt"
 "../../terms.rkt")
(provide domain@ val-⊤ atom-⊤ num-⊤ sym-⊤ stx-⊤ list-⊤ ≤e)

;; ----------------------------------------
;; Implementation of Domains:

(define val-⊤  (Val))
(define atom-⊤ (Atom))
(define num-⊤  (Num 'num-⊤))
(define sym-⊤  (Sym 'sym-⊤))
(define stx-⊤  (Stx 'stx-⊤ (set)))
(define list-⊤ (List))

(define (≤e v1 v2)
  (or (equal? v1 v2)
      (match* (v1 v2)
        [(_         (? (λ (x) (equal? x val-⊤))))  #t]
        [((? Atom?) (? (λ (x) (equal? x atom-⊤)))) #t]
        [((Val)     (? (λ (x) (equal? x atom-⊤)))) #f]
        [((? Num?)  (Num 'num-⊤))                  #t]
        [(_         (Num 'num-⊤))                  #f]
        [((? Sym?)  (Sym 'sym-⊤))                  #t]
        [(_         (Sym 'sym-⊤))                  #f]
        [((? Stx?)  (Stx 'stx-⊤ (set)))            #t]
        [(_         (Stx 'stx-⊤ (set)))            #f]
        [((? List?) (? (λ (x) (equal? x list-⊤)))) #t]
        [(_         (? (λ (x) (equal? x list-⊤)))) #f]
        [(_                        _)              #f])))

(define-unit domain@
  (import)
  (export domain^)
  
  (define (α vs) vs)
  (define (≤a vs1 vs2)
    (define vs1* (set->list vs1))
    (define vs2* (set->list vs2))
    (define (∈a v1) (ormap (λ (v2) (≤e v1 v2)) vs2*))
    (andmap ∈a vs1*))

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
        ;(and (Pair? x) (val? (Pair-a x)) (val? (Pair-d x)))
        ;(stx? x)
        (equal? x stx-⊤) ;; added
        ))

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
