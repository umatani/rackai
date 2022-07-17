#lang racket
(require
 (except-in racket set do)
 "../../../set.rkt"
 "../../../reduction.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt"  use-terms)
 (only-in "../../../terms.rkt" use-lst-form)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ delta^ eval^ 
          menv^ mstore^ bind^ mcont^ parser^ expand^ expander^)
 (only-in "../../base/phases/terms.rkt"  terms^ #%term-forms)

 (only-in "../../base/phases/expander.rkt" [==> base:==>] expander/expand@))
(provide ==> expander@)

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> <- -->)
  #:within-signatures [(only terms^
                             App% Atom% Sym% Stx% List% Null% Pair% TVar%
                             AstEnv% Stxξ% κ% ζ% InEval% Hole%)
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
                Stxξ% ζ%)
          (only eval^
                -->))
  (export expand^ expander^)
  (inherit [red@ reducer]
           [expander/expand@ expander])

  (use-terms Stxξ ζ)
  
  (define ==> (reducer -->))

  ; expand : Ph Stx ξ Scps Σ -> (Setof (Cons Stx Σ))
  (define (expand ph stx ξ scps_p Σ)
    (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• Σ)])
      (match-let ([(set (ζ stx_new '• '• Σ_new) ...)
                   (apply-reduction-relation* ==> init-ζ)])
        (list->set (map cons stx_new Σ_new))))))
