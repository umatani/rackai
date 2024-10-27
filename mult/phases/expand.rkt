#lang racket/base
(require
 racket/unit
 (only-in racket/match                   match)
 (only-in "../../set.rkt"                set ∅ ∅? set-add set→list)
 (only-in "../../mix.rkt"                define-mixed-unit inherit)
 (only-in "../../syntax.rkt"             snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/phases/terms.rkt"
 (only-in "../../base/phases/expand.rkt" [==> base:==>]))
(provide ==> define-expand-unit expand@)

;; ==> : ζ → (Setof ζ)
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
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ) κ₀ Σ₀)
   #:when (id? stx_f)
   #:with nam <- (resolve ph stx_f Σ₀)
   #:with  at := (results (lookup-ξ ξ nam))
   #:when (and (∅? at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:with        id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (? id? id) ξ _scpsₚ) κ Σ)
   #:with nam <- (resolve ph id Σ)
   #:with  at := (results (lookup-ξ ξ nam))
   #:with id′ <- (if (∅? at)
                   (error '==>p "unbound identifier: ~a" nam)
                   (do v <- (lift at)
                       (match v
                         [(TVar id′) (pure id′)]
                         [_ (error '==>p "unbound identifier: ~a" nam)])))
   (ζ id′ κ Σ)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^
             mcont^ bind^ id^ parse^)
    (export  expand^)
    (inherit [red@    reducer])
    
    ;; ==> : δ → ζ → (Setof ζ)
    (define (==> δ) (reducer (--> δ)))))

(define-expand-unit expand@ red@)
