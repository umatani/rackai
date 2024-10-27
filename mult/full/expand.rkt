#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 (only-in "../../misc.rkt"             union)
 (only-in "../../set.rkt"              set ∅ ∅? set-add set→list)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../syntax.rkt"           snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../base/full/expand.rkt" [==> base:==>]))
(provide ==> define-expand-unit expand@)

(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only domain^    val? stx? proper-stl?)
                       (only syntax^    empty-ctx zip unzip add flip in-hole
                                        prune at-phase)
                       (only    env^    init-env)
                       (only  store^    init-store)
                       (only   menv^    init-ξ lookup-ξ extend-ξ)
                       (only mstore^    lookup-Σ alloc-name alloc-scope)
                       (only   bind^    bind resolve)
                       (only     id^    id=?)
                       (only  mcont^    push-κ)
                       (only  parse^    parse)]
  ;; application (free var-ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ)
      κ₀ (Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:when (id? stx_f)
   #:with nam <- (resolve ph stx_f Σ₀)
   #:with  at := (results (lookup-ξ ξ nam))
   #:when (and (∅? at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:with id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ scpsₚ ∅))
   ex-app-free]

  ;; reference
  ;; set-basedにすることにより，bind-syntaxesがbinding storeに多重化をもたらし，
  ;; 名前の解決が不正確になる．atが empty なら unbound error で停止するのではなく，
  ;; 探索候補から除去する．
  [(ζ (Stxξ ph (? id? id) ξ)
      κ₀ (Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:with nam <- (resolve ph id Σ₀)
   #:with  at <- (lookup-ξ ξ nam)
   #:when (TVar? at)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ id κ₀)
   (ζ (TVar-id at)
      (κ (Hole) scpsₚ scpsᵤ 𝓁)
      (Σ̂ Σ₁ ∅ ∅))
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^ mcont^
             bind^ id^ parse^)
    (export  expand^)
    (inherit [red@    reducer])

    (define (==> δ) (λ () (reducer (--> δ))))))

(define-expand-unit expand@ red@)
