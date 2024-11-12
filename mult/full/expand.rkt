#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 (only-in "../../set.rkt"              set ∅ ∅? set-add set→list)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../syntax.rkt"           stx→datum snoc zip unzip prune at-phase)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../base/full/expand.rkt" [==> base:==>]))
(provide ==> define-expand-unit expand@)

(define-reduction (==> -->) #:super (base:==> --> <-)
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only domain^    val? stx? lst→list/recur)
            (only syntax^    empty-ctx add flip in-hole proper-stl?)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
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
