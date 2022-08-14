#lang racket
(require
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" terms-extra^ domain^)

 (only-in "../../terms.rkt" #%term-forms
          Val% Atom% List% Bool% Num% Sym% Stx% Null% Pair% Prim%))
(provide val-⊤ atom-⊤ num-⊤ stx-⊤ list-⊤
         ≤e domain@)

;; ----------------------------------------
;; Implementation of Domains:

(use-terms Val Atom List Bool Num Sym Stx Null Pair Prim)

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
       (println v1)
       (pure v2)])))
