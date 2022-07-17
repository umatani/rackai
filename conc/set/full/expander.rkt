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
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 (only-in "../../base/full/expander.rkt" [==> base:==>] expander/expand@))
(provide ==> expander@)

(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only terms^
                             App% Atom% Sym% Stx% List% Null% Pair% AstEnv%
                             Stxξ% κ% Σ*% TVar% TStop% ζ% InEval% Hole%)
                       (only terms-extra^
                             lst->list snoc id? val? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip union in-hole
                             prune at-phase)
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
                Stxξ% Σ*% ζ%)
          (only eval^
                -->))
  (export expand^ expander^)
  (inherit [red@ reducer]
           [expander/expand@ expander])

  (use-terms Stxξ Σ* ζ)

  (define ==> (λ () (reducer -->)))
  
  ; expand : Ph Stx ξ Σ* -> (Setof (Cons Stx Σ*))
  (define (expand ph stx ξ Σ*)
    (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• Σ*)])
      (match-let ([(set (ζ stx_new '• '• Σ*_new) ...)
                   (apply-reduction-relation* (==>) init-ζ)])
        (list->set (map cons stx_new Σ*_new))))))
