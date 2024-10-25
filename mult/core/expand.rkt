#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 (only-in "../../set.rkt"              set ∅? set→list)
 (only-in "../../syntax.rkt"           snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../base/core/expand.rkt" [==> base:==>]))
(provide ==> red@ expand/red@ expand@)

;; Revised reduction rules

;; ==> : ζ → (Setof ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only domain^    val? stx? proper-stl?)
                       (only syntax^    empty-ctx zip unzip add flip in-hole)
                       (only    env^    init-env)
                       (only  store^    init-store)
                       (only   menv^    init-ξ lookup-ξ extend-ξ)
                       (only mstore^    lookup-Σ alloc-name alloc-scope)
                       (only   bind^    bind resolve)
                       (only     id^    id=?)
                       (only  mcont^    push-κ)
                       (only  parse^    parse)]

  ;; application (free var-ref)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ) κ₀ Σ₀)
   #:when (id? stx_f)
   #:with nam <- (resolve stx_f Σ₀)
   #:with  at := (results (lookup-ξ ξ nam))
   #:when (and (∅? at)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:with        id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁 Σ₁) := (push-κ Σ₀ stx κ₀)
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁)
      Σ₁)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ (? id? id) ξ) κ Σ)
   #:with nam <- (resolve id Σ)
   #:with at  := (results (lookup-ξ ξ nam))
   #:with id′ <- (if (∅? at)
                        (error '==> "unbound identifier: ~a" nam)
                        (do v <- (lift at)
                            (match v
                              [(TVar id′) (pure id′)]
                              [_ (error '==> "unbound identifier: ~a" nam)])))
   (ζ id′ κ Σ)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-unit expand/red@
  (import (only eval^    -->)
          (only  red^    reducer))
  (export expand^)

  ;; δ → ζ → (Setof ζ)
  (define (==> δ) (reducer (--> δ)))

  ;; expand : δ Stx ξ Σ → (SetM (Cons Stx Σ))
  (define (expand δ stx ξ Σ)
    (define ==>δ (==> δ))
    (define ζᵢ   (ζ (Stxξ stx ξ) '● Σ))

    (do (ζ stx′ '● Σ′) <- (lift (apply-reduction* ==>δ ζᵢ))
        (pure (cons stx′ Σ′)))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^ menv^ mstore^
          mcont^ bind^ id^ parse^)
  (export expand^)
  (link expand/red@ red@))
