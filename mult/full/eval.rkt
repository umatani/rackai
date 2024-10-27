#lang racket/base
(require
 racket/unit
 (only-in racket/match               match match-λ**)
 (only-in "../../set.rkt"            set ∅ ∅? set-add)
 (only-in "../../mix.rkt"            define-mixed-unit inherit)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../base/full/eval.rkt" [--> base:-->]))
(provide --> define-eval-unit eval@)

;; --> : State -> (Setof State)
(define-reduction (--> δ ==>) #:super (base:--> δ ==> <-)
  #:import [(only domain^    val? stx?)
            (only syntax^    add flip prune)
            (only    env^    init-env lookup-env extend-env*)
            (only  store^    lookup-store update-store* alloc-loc*)
            (only   cont^    push-cont)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    alloc-name alloc-scope alloc-𝓁 lookup-Σ update-Σ)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; lookup-val : Store Loc → (SetM Val)
        (define (lookup-val sto loc)
          (do val <- (lookup-store sto loc)
              #:when (val? val)
              (pure val)))
        
        ;; lookup-cont : Store Loc → (SetM Cont)
        (define (lookup-cont sto loc)
          (do cnt <- (lookup-store sto loc)
              #:when (cont? cnt)
              (pure cnt)))

        ;; lookup-def-ξ : Σ 𝓁 → (SetM ξ)
        (define (lookup-def-ξ Σ 𝓁)
          (do ξ <- (lookup-Σ Σ 𝓁)
              #:when (hash? ξ)
              (pure ξ)))

        ;; lookup-box : Σ 𝓁 → (SetM Val)
        (define (lookup-box Σ 𝓁)
          (do val <- (lookup-Σ Σ 𝓁)
              #:when (val? val)
              (pure val)))

        ;; resolve* : Ph (Listof Id) Σ → (SetM (Listof Nam))
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
                  (pure (cons a as)))]))])

(define-unit-from-reduction red@ -->)

(define-syntax-rule (define-eval-unit eval@ red@)
  (define-mixed-unit eval@
    (import domain^ syntax^ env^ store^ cont^ menv^ mstore^ bind^ expand^ parse^)
    (export eval^)
    (inherit [red@    reducer])

    ;; --> : δ → → State → (Setof State)
    (define (--> δ) (λ () (reducer δ (==> δ))))))

(define-eval-unit eval@ red@)
