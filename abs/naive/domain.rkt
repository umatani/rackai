#lang racket/base
(require
 racket/unit
 (only-in racket/match           match*)
 (only-in racket/pretty          pretty-print)
 (only-in "../../set.rkt"        set ∅ set→list)
 (only-in "../../nondet.rkt"     mzero mplus pure abort)
 (only-in "../../mix.rkt"        define-mixed-unit inherit)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../mult/units.rkt" [domain@ mult:domain@]))
(provide domain@ ≤ᵥ)

;; ----------------------------------------
;; Implementation of Domains:

;; Abstract values
;;   Null, Bool, Sym, VFun, Prim は具象値が有限のため，それがそのまま抽象値
;;   Pair, Num, Stx は具象値が無限のため，それぞれに ⊤ を用意

(define (≤ᵥ v₁ v₂)
  (or (equal? v₁ v₂)
      (match* (v₁ v₂)
        [(             _         'val-⊤) #t]
        [(     (? Pair?)        'pair-⊤) #t]
        [(     (?  Num?)         'num-⊤) #t]
        [(     (?  Stx?)         'stx-⊤) #t]

        ;; recursive check for Pair and Stx
        [(  (Pair a₁ d₁)   (Pair a₂ d₂)) (and (≤ᵥ a₁ a₂) (≤ᵥ d₁ d₂))]
        [((Stx e₁ _ctx₁) (Stx e₂ _ctx₂)) (≤ᵥ e₁ e₂)]

        [(             _              _) #f])))

(define-mixed-unit domain@
  (import)
  (export domain^)
  (inherit [mult:domain@    α])

  ;; val? : Ast → Boolean
  (define (val? x)
    (or (Val? x)
        (eq? x 'stx-⊤)
        (eq? x 'num-⊤)
        (eq? x 'pair-⊤)
        (eq? x 'val-⊤)))

  ;; pair? : Ast → Boolean
  (define (pair? x)
    (or (Pair? x)
        (eq? x 'pair-⊤)
        (eq? x 'val-⊤)))

  ;; stx? : Ast → Boolean
  (define (stx? x)
    (or (Stx? x)
        (eq? x 'stx-⊤)
        (eq? x 'val-⊤)))

  ;; num? : Ast → Boolean
  (define (num? x)
    (or (Num? x)
        (eq? x 'num-⊤)
        (eq? x 'val-⊤)))

  ;; maybe-zero? : Ast → Boolean
  (define (maybe-zero? x)
    (or (and (Num? x) (zero? (Num-n x)))
        (eq? x 'num-⊤)
        (eq? x 'val-⊤)))

  (define (≤ₐ vs₁ vs₂)
    (define vs₁* (set→list vs₁))
    (define vs₂* (set→list vs₂))
    (define (∈ₐ v₁) (ormap (λ (v₂) (≤ᵥ v₁ v₂)) vs₂*))
    (andmap ∈ₐ vs₁*))

  ; δ : Prim (Listof Val) → (SetM Val)
  (define (δ op vs)
    (match* (op vs)
      ;; +
      [((Prim '+ _) (list (? num?) ...))
       (pure 'num-⊤)]

      ;; *
      [((Prim '* _) (list (? num?) ...))
       (pure 'num-⊤)]

      ;; -
      [((Prim '- _) (list (? num?) (? num?) ...))
       (pure 'num-⊤)]

      ;; /
      [((Prim '/ _) (list (? num?) (? num? ns) ...))
       (cond [(ormap (λ (n) (and (Num? n) (zero? (Num-n n)))) ns)
              (abort "division by zero")]
             [(ormap maybe-zero? ns)
              (mplus (abort "division by zero") (pure 'num-⊤))]
             [else
              (pure 'num-⊤)])]

      ;; <
      [((Prim '< _) (list (? num?) (? num?) ...))
       (mplus (pure (Bool #t)) (pure (Bool #f)))]

      ;; =
      [((Prim '= _) (list (? num?) (? num?) ...))
       (mplus (pure (Bool #t)) (pure (Bool #f)))]

      ;; eq?
      [((Prim 'eq? _) (list (Sym s) (Sym t)))
       (pure (Bool (eq? s t)))]

      ;; cons
      [((Prim 'cons _) (list _ _))
       (pure 'pair-⊤)]

      ;; list
      [((Prim 'list _) (list))
       (pure (Null))]
      [((Prim 'list _) (list _ _ ...))
       (pure 'pair-⊤)]

      ; -------------------------------------------------------------------

      ;; car
      [((Prim 'car _) (list (Pair a _d)))
       (pure a)]
      [((Prim 'car _) (list (? pair?)))
       (pure 'val-⊤)]

      ;; cdr
      [((Prim 'car _) (list (Pair _a d)))
       (pure d)]
      [((Prim 'cdr _) (list (? pair?)))
       (pure 'val-⊤)]

      ;; second
      [((Prim 'second _) (list (Pair _ (Pair a _))))
       (pure a)]
      [((Prim 'second _) (list (Pair _ (? pair?))))
       (pure 'val-⊤)]
      [((Prim 'second _) (list (? pair?)))
       (pure 'val-⊤)]

      ;; third
      [((Prim 'third _) (list (Pair _ (Pair _ (Pair a _)))))
       (pure a)]
      [((Prim 'third _) (list (Pair _ (Pair _ (? pair?)))))
       (pure 'val-⊤)]
      [((Prim 'third _) (list (Pair _ (? pair?))))
       (pure 'val-⊤)]
      [((Prim 'third _) (list (? pair?)))
       (pure 'val-⊤)]

      ;; fourth
      [((Prim 'fourth _) (list (Pair _ (Pair _ (Pair _ (Pair a _))))))
       (pure a)]
      [((Prim 'fourth _) (list (Pair _ (Pair _ (Pair _ (? pair?))))))
       (pure 'val-⊤)]
      [((Prim 'fourth _) (list (Pair _ (Pair _ (? pair?)))))
       (pure 'val-⊤)]
      [((Prim 'fourth _) (list (Pair _ (? pair?))))
       (pure 'val-⊤)]
      [((Prim 'fourth _) (list (? pair?)))
       (pure 'val-⊤)]

      ;; syntax-e
      [((Prim 'syntax-e _) (list (Stx e _ctx)))
       (pure e)]
      [((Prim 'syntax-e _) (list (? stx?)))
       (pure 'val-⊤)]

      ;; syntax->datum
      [((Prim 'syntax->datum _) (list (? stx?)))
       (pure 'val-⊤)]

      ;; datum->syntax
      [((Prim 'datum->syntax _) (list (? stx?) _v))
       (pure 'stx-⊤)]

      ;; for debug
      [((Prim 'printe _) (list v₀ v))
       (pretty-print (lst→list/recur v₀))
       (pure v)]

      [((Prim op _) _) (abort (format "illegal use of ~s\n" op))])))
