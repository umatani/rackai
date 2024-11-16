#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 (only-in "../../set.rkt"              set)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../syntax.rkt"           stx→datum snoc zip unzip)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../base/core/expand.rkt" [==> base:==>]))
(provide ==> define-expand-unit expand@)

;; Revised reduction rules

;; ==> : ζ → (SetM ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only domain^    val? stx? id? lst→list/recur)
            (only syntax^    empty-ctx add flip in-hole proper-stl?)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; id=? : Id Nam Σ → (SetM Boolean)
        (define (id=? id nam Σ)
          (do nam′ <- (resolve id Σ)
              (pure (eq? nam nam′))))])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^ bind^ parse^)
    (export  expand^)
    (inherit [red@    reducer])

    ;; δ → ζ → (SetM ζ)
    (define (==> δ) (reducer (--> δ)))))

(define-expand-unit expand@ red@)
