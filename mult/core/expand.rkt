#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 (only-in "../../set.rkt"              set ∅? set→list)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../syntax.rkt"           snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../base/core/expand.rkt" [==> base:==>]))
(provide ==> define-expand-unit expand@)

;; Revised reduction rules

;; ==> : ζ → (Setof ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:import [(only domain^    val? stx? proper-stl?)
            (only syntax^    empty-ctx zip unzip add flip in-hole)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only     id^    id=?)
            (only  mcont^    push-κ)
            (only  parse^    parse)]

  #:do [;; lookup-κ : Σ 𝓁 → (SetM κ)
        (define (lookup-κ Σ 𝓁)
          (do κ <- (lookup-Σ Σ 𝓁)
              #:when (or (κ? κ) (eq? κ '●))
              (pure κ)))]

  ;; application (free var ref)
  [(ζ (Stxξ (and (Stx (Lst stx_f . stl) ctx) stx) ξ) κ₀ Σ₀)
   #:when (id? stx_f)
   (<- nam (resolve stx_f Σ₀))
   (<- at  (lookup-ξ ξ nam))
   #:when (and (eq? at 'not-found)
               (not (member nam '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%snoc))))
   (:= id_app        (Stx (Sym '#%app) ctx))
   (:= (values 𝓁 Σ₁) (push-κ Σ₀ stx κ₀))
   (ζ (Stxξ (Stx (Lst id-seq stx-nil stx_f . stl) ctx) ξ)
      (κ (Stx (Pair id_app (Hole)) ctx) 𝓁)
      Σ₁)
   ex-app-free]

  ;; reference
  ;; set-basedにすることにより，得にfullではbind-syntaxesがbinding storeに多重化を
  ;; もたらし，名前の解決が不正確になる．
  ;; at が not-found なら unbound error で停止するのではなく，探索候補から除去する．
  [(ζ (Stxξ (? id? id) ξ)
      κ Σ)
   (<- nam (resolve id Σ))
   (<- at  (lookup-ξ ξ nam))
   #:when (TVar? at)
   (ζ (TVar-id at)
      κ Σ)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^
             mcont^ bind^ id^ parse^)
    (export  expand^)
    (inherit [red@    reducer])

    ;; δ → ζ → (Setof ζ)
    (define (==> δ) (reducer (--> δ)))))

(define-expand-unit expand@ red@)
