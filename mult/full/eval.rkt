#lang racket/base
(require
 racket/unit
 (only-in racket/match               match match-λ**)
 (only-in "../../set.rkt"            set ∅ ∅? set-add for/set)
 (only-in "../../mix.rkt"            define-mixed-unit inherit)
 (only-in "../../misc.rkt"           update-store* alloc-loc*)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../base/full/eval.rkt" [--> base:-->]))
(provide --> define-eval-unit eval@)

;; --> : State -> (Setof State)
(define-reduction (--> δ ==>) #:super (base:--> δ ==> <-)
  #:import [(only common^    push-cont)
            (only   misc^    lookup-cont lookup-val)
            (only domain^    val? stx?)
            (only syntax^    add flip prune)
            (only    env^    init-env lookup-env extend-env*)
            (only  store^    lookup-store update-store alloc-loc)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    alloc-name alloc-scope alloc-𝓁 lookup-Σ update-Σ)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; resolve* : Ph (Listof Id) Σ → (SetM (Listof Nam))
        (define (resolve* ph ids Σ)
          (match ids
            ['() (pure '())]
            [(cons id ids)
             (do nam  <- (resolve  ph id  Σ)
                 nams <- (resolve* ph ids Σ)
                 (pure (cons nam nams)))]))

        ;; lookup-ξ* : ξ (Listof Nam) → (SetM (Listof AllTransform))
        (define (lookup-ξ* ξ ns)
          (match ns
            ['() (pure '())]
            [(cons n ns)
             (do  a  <- (lookup-ξ  ξ n)
                  as <- (lookup-ξ* ξ ns)
                  (pure (cons a as)))]))

        ;; unstop-ξ : ξ → ξ
        (define (unstop-ξ ξ)
          (make-immutable-hash
           (hash-map ξ (λ (nam ats)
                         (cons nam (for/set ([at ats]) (unstop at)))))))

        ;; lookup-def-ξ : Σ 𝓁 → (SetM ξ)
        (define (lookup-def-ξ Σ 𝓁)
          (do ξ <- (lookup-Σ Σ 𝓁)
              #:when (hash? ξ)
              (pure ξ)))

        ;; extend-def-ξ : Σ 𝓁 nam at → Σ
        (define (extend-def-ξ Σ₀ 𝓁 nam at)
          (Σ (Σ-size Σ₀) (hash-update (Σ-tbl Σ₀) 𝓁
                                      (λ (vs)
                                        (for/set ([ξ vs])
                                          (if (hash? ξ)
                                            (extend-ξ ξ nam at)
                                            ξ))))))

        ;; lookup-box : Σ 𝓁 → (SetM Val)
        (define (lookup-box Σ 𝓁)
          (do val <- (lookup-Σ Σ 𝓁)
              #:when (val? val)
              (pure val)))])

(define-unit-from-reduction red@ -->)

(define-syntax-rule (define-eval-unit eval@ red@)
  (define-mixed-unit eval@
    (import domain^ syntax^ env^ store^ menv^ mstore^ bind^ expand^ parse^)
    (export eval^)
    (inherit [red@    reducer])

    ;; --> : δ → → State → (Setof State)
    (define (--> δ) (λ () (reducer δ (==> δ))))))

(define-eval-unit eval@ red@)
