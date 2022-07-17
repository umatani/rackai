#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt"  use-terms)
 (only-in "../../../terms.rkt" use-lst-form)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ delta^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^ expander^)
 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)

 (only-in "../../base/core/expander.rkt" [==> base:==>] expander/expand@))
(provide ==> expander@)

;; Revised reduction rules

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only terms^
                             App% Atom% Sym% Stx% List% Null% Pair% AstEnv%
                             TVar% κ% Stxξ% InEval% Hole% ζ%)
                       (only terms-extra^
                             lst->list snoc stx? id? val? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip in-hole)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only delta^
                             prim?)
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
  (import (only terms^
                Stxξ% ζ%)
          (only eval^
                -->))
  (export expand^ expander^)
  (inherit [red@ reducer]
           [expander/expand@ expander])

  (use-terms Stxξ ζ)

  (define ==> (reducer -->))

  ; expand : Stx ξ Σ -> (Setof (Cons Stx Σ))
  (define (expand stx0 ξ Σ)
    (let ([init-ζ (ζ (Stxξ stx0 ξ) '∘ '• Σ)])
      (match-let ([(set (ζ stx_new '• '• Σ_new) ...)
                   (apply-reduction-relation* ==> init-ζ)])
        (list->set (map cons stx_new Σ_new)))))  )
