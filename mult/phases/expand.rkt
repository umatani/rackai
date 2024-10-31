#lang racket/base
(require
 racket/unit
 (only-in racket/match                   match)
 (only-in "../../set.rkt"                set ∅ ∅? set-add set→list)
 (only-in "../../mix.rkt"                define-mixed-unit inherit)
 (only-in "../../syntax.rkt"             snoc stx→datum)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/phases/terms.rkt"
 (only-in "../../base/phases/expand.rkt" [==> base:==>]))
(provide ==> define-expand-unit expand@)

;; ==> : ζ → (Setof ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:import [(only common^    push-κ regist-vars)
            (only domain^    val? stx? proper-stl?)
            (only syntax^    empty-ctx zip unzip add flip in-hole prune at-phase)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; lookup-κ : Σ 𝓁 → (SetM κ)
        (define (lookup-κ Σ 𝓁)
          (do κ <- (lookup-Σ Σ 𝓁)
              #:when (or (κ? κ) (eq? κ '●))
              (pure κ)))

        ;; id=? : Ph Id Nam Σ → (SetM Boolean)
        (define (id=? ph id nam Σ)
          (do nam′ <- (resolve ph id Σ)
              (pure (eq? nam nam′))))]

  #:default [(ζ (Stxξ ph stx ξ scpsₚ) κ Σ) ;; for debug
             (printf "default: ~a\n" (lst→list/recur (stx→datum stx)))]

  ;; application (free var ref)
  [(ζ (Stxξ ph (and (Stx (Lst stx_f . stl) ctx) stx) ξ scpsₚ) κ₀ Σ₀)
   #:when (id? stx_f)
   (<- nam (resolve ph stx_f Σ₀))
   (<- at  (lookup-ξ ξ nam))
   #:when (and (eq? at 'not-found)
               (not (member nam
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ scpsₚ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁) Σ₁)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (? id? id) ξ _scpsₚ)
      κ Σ)
   (<- nam (resolve ph id Σ))
   (<- at  (lookup-ξ ξ nam))
   #:when (TVar? at)
   (ζ (TVar-id at)
      κ Σ)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^ bind^ parse^)
    (export  expand^)
    (inherit [red@    reducer])
    
    ;; ==> : δ → ζ → (Setof ζ)
    (define (==> δ) (reducer (--> δ)))))

(define-expand-unit expand@ red@)
