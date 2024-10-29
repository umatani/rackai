#lang racket/base
(require
 racket/unit
 (only-in racket/match match)
 (only-in "mix.rkt"    define-mixed-unit inherit)
 "signatures.rkt"
 "terms.rkt")
(provide core-common@ phases-common@ full-common@)

(define-unit core-common@
  (import
   (only    bind^    bind)
   (only    menv^    extend-ξ)
   (only  mstore^    update-Σ alloc-name alloc-𝓁)
   (only   store^    update-store alloc-loc)
   (only  syntax^    add))
  (export common^)

  ;; push-cont : Store Label Cont → (Values Loc Store)
  ;;   - lbl is generated for each AST (currently for App and If) during parse
  (define (push-cont sto lbl cnt)
    (let-values ([(loc sto′) (alloc-loc lbl sto)])
      (values loc (update-store sto′ loc cnt))))

  ;; push-κ : Σ Stx κ → (Values 𝓁 Σ)
  ;;   Stx is being expanded. The entire set of Stx would be finite in abs.
  (define (push-κ Σ stx κ)
    (let-values ([(𝓁 Σ′) (alloc-𝓁 stx Σ)])
      (values 𝓁 (update-Σ Σ′ 𝓁 κ))))


  ;; regist-vars : Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)
  (define (regist-vars scp ids₀ ξ₀ Σ₀)
    (match ids₀
      [(Null)
       (values (Null) ξ₀ Σ₀)]
      [(Pair id ids)
       (let*-values ([(ids′ ξ₁ Σ₁) (regist-vars scp ids ξ₀ Σ₀)]
                     [(nam Σ₂)     (alloc-name id Σ₁)]
                     [(id′)        (add id scp)]
                     [(Σ₃)         (bind Σ₂ id′ nam)]
                     [(ξ₂)         (extend-ξ ξ₁ nam (TVar id′))])
         (values (Pair id′ ids′) ξ₂ Σ₃))])))

(define-mixed-unit phases-common@
  (import
   (only    bind^    bind)
   (only    menv^    extend-ξ)
   (only  mstore^    alloc-name)
   (only  syntax^    add))
  (export common^)
  (inherit [core-common@    push-cont push-κ])

  ;; regist-vars : Ph Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)
  ;;   This is the same as the single-phase one, but with `ph`
  ;;   threaded through to `add` & `bind`
  (define (regist-vars ph scp ids₀ ξ₀ Σ₀)
    (match ids₀
      [(Null)
       (values (Null) ξ₀ Σ₀)]
      [(Pair id ids)
       (let*-values ([(ids′ ξ₁ Σ₁) (regist-vars ph scp ids ξ₀ Σ₀)]
                     [(nam Σ₂)     (alloc-name id Σ₁)]
                     [(id′)        (add ph id scp)]
                     [(Σ₃)         (bind ph Σ₂ id′ nam)]
                     [(ξ₂)         (extend-ξ ξ₁ nam (TVar id′))])
         (values (Pair id′ ids′) ξ₂ Σ₃))])))

(define-mixed-unit full-common@
  (import)
  (export common^)
  (inherit [phases-common@    push-cont push-κ regist-vars]))
