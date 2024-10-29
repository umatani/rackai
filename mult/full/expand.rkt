#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 (only-in "../../misc.rkt"             union)
 (only-in "../../set.rkt"              set ∅ ∅? set-add set→list)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../syntax.rkt"           snoc stx→datum)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../base/full/expand.rkt" [==> base:==>]))
(provide ==> define-expand-unit expand@)

(define-reduction (==> -->) #:super (base:==> --> <-)
  #:import [(only common^    push-κ regist-vars)
            (only domain^    val? stx? proper-stl?)
            (only syntax^    empty-ctx zip unzip add flip in-hole prune at-phase)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only     id^    id=?)
            (only  parse^    parse)]

  #:do [;; lookup-κ : Σ 𝓁 → (SetM κ)
        (define (lookup-κ Σ 𝓁)
          (do κ <- (lookup-Σ Σ 𝓁)
              #:when (or (κ? κ) (eq? κ '●))
              (pure κ)))]

  #:default [(ζ (Stxξ ph stx ξ) κ Σ̂) ;; for debug
             (printf "default: ~a\n" (lst→list/recur (stx→datum stx)))]

  ;; application (free var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ (Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:when (id? stx_f)
   (<- nam (resolve ph stx_f Σ₀))
   (<- at  (lookup-ξ ξ nam))
   #:when (and (eq? at 'not-found)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (? id? id) ξ)
      κ₀ (Σ̂ Σ₀ scpsₚ scpsᵤ))
   (<- nam (resolve ph id Σ₀))
   (<- at  (lookup-ξ ξ nam))
   #:when (TVar? at)
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ id κ₀))
   (ζ (TVar-id at)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^ bind^ id^ parse^)
    (export  expand^)
    (inherit [red@    reducer])

    (define (==> δ) (λ () (reducer (--> δ))))))

(define-expand-unit expand@ red@)
