#lang racket/base
(require
 racket/unit
 (only-in racket/match           match*)
 (only-in racket/pretty          pretty-print)
 (only-in "../../set.rkt"        set ∅ set→list)
 (only-in "../../nondet.rkt"     mzero mplus pure)
 (only-in "../../mix.rkt"        define-mixed-unit inherit)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../mult/units.rkt" [domain@ mult:domain@]))
(provide domain@ val-⊤ pair-⊤ atom-⊤ num-⊤ stx-⊤ ≤e)

;; ----------------------------------------
;; Implementation of Domains:

;; Abstract values
;;   Null, Bool, Sym, VFun, Prim は具象値が有限のため，それがそのまま抽象値
;;   Pair, Num, Stx は具象値が無限のため，それぞれに ⊤ を用意

(define (≤v v₁ v₂)
  (or (equal? v₁ v₂)
      (match* (v₁ v₂)
        [(             _         'val-⊤) #t]
        [(     (? Pair?)        'pair-⊤) #t]
        [(     (?  Num?)         'num-⊤) #t]
        [(     (?  Stx?)         'stx-⊤) #t]

        ;; recursive check for Pair and Stx
        [(  (Pair a₁ d₁)   (Pair a₂ d₂)) (and (≤v a₁ a₂) (≤v d₁ d₂))]
        [((Stx e₁ _ctx₁) (Stx e₂ _ctx₂)) (≤v e₁ e₂)]

        [(             _              _) #f])))

(define-mixed-unit domain@
  (import)
  (export domain^)
  (inherit [mult:domain@    α val?])

  ;; stx? : Val → Boolean
  (define (stx? x)
    (or (Stx? x) (eq? x 'val-⊤)))

  ;; num? : Val → Boolean
  (define (num? x)
    (or (Num? x)
        (eq? x 'num-⊤)
        (eq? x 'val-⊤)))

  ;; maybe-zero? : Val → Boolean
  (define (maybe-zero? x)
    (or (and (Num? x) (zero? (Num-n x)))
        (eq? x 'num-⊤)
        (eq? x 'val-⊤)))

  
  (define (≤ₐ vs₁ vs₂)
    (define vs₁* (set→list vs₁))
    (define vs₂* (set→list vs₂))
    (define (∈ₐ v₁) (ormap (λ (v₂) (≤v v₁ v₂)) vs₂*))
    (andmap ∈ₐ vs₁*))

  ; δ : Prim (Listof Val) → (SetM Val)
  (define (δ op vs)
    (match* (op vs)
      ;; +
      [((Prim '+ _) (list (? num? _) ...))
       (pure 'num-⊤)]
      [((Prim '+ _) (list _          ...))
       mzero]

      ;; *
      [((Prim '* _) (list (? num? _) ...))
       (pure 'num-⊤)]
      [((Prim '* _) (list _          ...))
       mzero]

      ;; -
      [((Prim '- _) (list (? num? _) (? num? _) ...))
       (pure num-⊤)]
      [((Prim '- _) (list _          _          ...))
       mzero]

      ;; /
      [((Prim '/ _) (list (? num? _) (? num? n) ...))
       (cond [(ormap (λ (n)
                       (and (Num? n)
                            (zero? (Num-n n)))) n) mzero]
             [(ormap maybe-zero? n)                (mplus mzero (pure num-⊤))]
             [else                                 (pure num-⊤)])]
      [((Prim '/ _) (list _          _          ...))
       mzero]

      ;; <
      [((Prim '< _) (list (? num? _) (? num? _) ...))
       (mplus (Bool #t) (Bool #f))]
      [((Prim '< _) (list _          _          ...))
       mzero]

      ;; =
      [((Prim '= _) (list (? num? _) (? num? _) ...))
       (mplus (Bool #t) (Bool #f))]
      [((Prim '= _) (list _          _          ...))
       mzero]

      ;; eq?
      [((Prim 'eq? _) (list (Sym s) (Sym t)))
       (pure (Bool (eq? s t)))]
      [((Prim 'eq? _) (list _       _))
       mzero]

      ;; cons
      [((Prim 'cons _) (list _ _))
       (pure pair-⊤)]

      ;; list
      [((Prim 'list _) (list))
       (pure (Null))]
      [((Prim 'list _) (list _ _ ...))
       (pure pair-⊤)]

      ; -------------------------------------------------------------------

      ;; car
      [((Prim 'car _) (list (? (λ (x) (or (Pair? x)
                                          (equal? x val-⊤))))))
       (pure val-⊤)]
      [((Prim (? (λ (op) (or (eq? op 'car) (eq? op 'cdr)))) _) (list _ ...))
       mzero]


      ;; cdr, second, third, fourth
      [((Prim (? (λ (op) (or (eq? op 'cdr)
                             (eq? op 'second) (eq? op 'third)
                             (eq? op 'fourth)))) _)
        (list (? (λ (x) (or (Pair? x)
                            (equal? x val-⊤))))))
       (pure val-⊤)]
      [((Prim (? (λ (op) (or (eq? op 'cdr)))) _) (list _ ...))
       mzero]



      ;; syntax-e
      [((Prim 'syntax-e _) (list (Stx e _)))
       (pure e)]
      [((Prim 'syntax-e _) (list (? stx? _)))
       (pure val-⊤)]
      [((Prim 'syntax-e _) (list _))
       mzero]


      ;; syntax->datum
      [((Prim 'syntax->datum _) (list (? (λ (x) (or (Stx? x)
                                                    (equal? x atom-⊤)
                                                    (equal? x val-⊤))))))
       (pure val-⊤)]
      [((Prim 'syntax->datum _) (list _ ...))
       mzero]

      ;; datum->syntax
      [((Prim 'datum->syntax _) (list (? (λ (x) (or (Stx? x)
                                                    (equal? x atom-⊤)
                                                    (equal? x val-⊤))))
                                      v))
       (pure stx-⊤)]
      [((Prim 'datum->syntax _) (list _ ...))
       mzero]

      ; -------------------------------------------------------------------


      ;; for debug
      [((Prim 'printe _) (list v₀ v))
       (pretty-print (lst→list/recur v₀))
       (pure v)])))
