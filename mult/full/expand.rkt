#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
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
            (only mstore^    lookup-Σ lookup-κ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; id=? : Ph Id Nam ξ Σ → (SetM Boolean)
        (define (id=? ph id nam ξ Σ)
          (do nam′ <- (resolve ph id Σ)
              at   <- (lookup-ξ ξ nam)
              (pure (and (eq? nam nam′) (not (TStop? at))))))])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^ bind^ parse^)
    (export  expand^)
    (inherit [red@    reducer])

    (define (==> δ) (λ () (reducer (--> δ))))))

(define-expand-unit expand@ red@)
