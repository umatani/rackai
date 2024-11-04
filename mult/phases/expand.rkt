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
            (only   misc^    lookup-κ)
            (only domain^    val? stx? proper-stl?)
            (only syntax^    empty-ctx zip unzip add flip in-hole prune at-phase)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; id=? : Ph Id Nam Σ → (SetM Boolean)
        (define (id=? ph id nam Σ)
          (do nam′ <- (resolve ph id Σ)
              (pure (eq? nam nam′))))])

(define-unit-from-reduction red@ ==>)

(define-syntax-rule (define-expand-unit expand@ red@)
  (define-mixed-unit expand@
    (import  domain^ syntax^ env^ store^ eval^ menv^ mstore^ bind^ parse^)
    (export  expand^)
    (inherit [red@    reducer])
    
    ;; ==> : δ → ζ → (Setof ζ)
    (define (==> δ) (reducer (--> δ)))))

(define-expand-unit expand@ red@)
