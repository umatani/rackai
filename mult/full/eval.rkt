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
  #:do [; resolve* : Ph (Listof Id) Σ → (SetM (Listof Nam))
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
             (do  a <- (let ([as (lookup-ξ ξ n)])
                         (if (∅? (results as))
                           (pure 'not-found)
                           as))
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
