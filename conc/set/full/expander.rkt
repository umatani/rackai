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
 (only-in "../../base/full/config.rkt" config^ #%term-forms)
 (only-in "../../base/full/expander.rkt" [==> base:==>] expander/expand@))
(provide ==> expander@)

(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only config^
                             AstEnv% TVar% TStop% ζ% Stxξ% κ% Σ*% InEval%)
                       (only terms-extra^
                             val? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip union in-hole
                             prune at-phase)
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
                Stxξ% Σ*% ζ%)
          (only eval^
                -->))
  (export expand^ expander^)
  (inherit [red@ reducer]
           [expander/expand@ expander])
  (use-terms Stxξ Σ* ζ)

  (define (==> delta) (λ () (reducer (--> delta))))
  
  ; expand : Ph Stx ξ Σ* -> (Setof (Cons Stx Σ*))
  (define (expand delta ph stx ξ Σ*)
    (define ==>d (==> delta))
    (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• Σ*)])
      (match-let ([(set (ζ stx_new '• '• Σ*_new) ...)
                   (apply-reduction-relation* (==>d) init-ζ)])
        (list->set (map cons stx_new Σ*_new))))))
