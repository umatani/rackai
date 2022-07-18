#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt"  use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^ expander^)
 (only-in "../../../terms.rkt"
          App% Atom% Sym% Stx% List% Null% Pair% Hole%
          lst->list snoc id? prim?
          use-lst-form)
 (only-in "../../base/core/config.rkt" config^ #%term-forms)
 (only-in "../../base/core/expander.rkt" [==> base:==>] expander/expand@))
(provide ==> expander@)

;; Revised reduction rules

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only config^
                             AstEnv% TVar% ζ% Stxξ% κ% InEval%)
                       (only terms-extra^
                             val? stx? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip in-hole)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name alloc-scope)
                       (only bind^
                             bind resolve id=?)
                       (only mcont^
                             push-κ)
                       (only parser^
                             parse)])

(define-unit-from-reduction red@ ==>)

(define-mixed-unit expander@
  (import (only config^
                Stxξ% ζ%)
          (only eval^
                -->))
  (export expand^ expander^)
  (inherit [red@ reducer]
           [expander/expand@ expander])

  (use-terms Stxξ ζ)

  (define (==> delta) (reducer (--> delta)))

  ; expand : Stx ξ Σ -> (Setof (Cons Stx Σ))
  (define (expand delta stx0 ξ Σ)
    (define ==>d (==> delta))
    (let ([init-ζ (ζ (Stxξ stx0 ξ) '∘ '• Σ)])
      (match-let ([(set (ζ stx_new '• '• Σ_new) ...)
                   (apply-reduction-relation* ==>d init-ζ)])
        (list->set (map cons stx_new Σ_new)))))  )
