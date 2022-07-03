#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ parser^ expand^)
 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)

 (only-in "../../base/core/expand.rkt" [==> base:==>]))
(provide ==> expand@)

;; Revised reduction rules

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only terms^
                             App% Sym% Stx% AstEnv% TVar% κ% Stxξ%
                             InEval% Hole% ζ%)
                       (only terms-extra^
                             stx? id? atom? val? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip snoc add flip in-hole)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-scope bind resolve id=?)
                       (only mcont^
                             lookup-κ push-κ)
                       (only parser^
                             parse)])

(define-unit-from-reduction red@ ==>)

(define-unit expand/red@
  (import (only terms^
                Stxξ% ζ%)
          (only eval^
                -->)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only mcont^
                init-Θ)
          (only red^
                reducer))
  (export expand^)

  (use-terms Stxξ ζ)

  (define ==> (reducer -->))

  ; expand : Stx ξ Σ -> (Setof (Cons Stx Σ))
  (define (expand stx0 ξ Σ)
    (let ([init-ζ (ζ (Stxξ stx0 ξ) '∘ '• (init-Θ) Σ)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                   (apply-reduction-relation* ==> init-ζ)])
        (list->set (map cons stx_new Σ_new)))))

  ; expander : Stx -> (Cons Stx Σ)
  (define (expander stx)
    (expand stx (init-ξ) (init-Σ))))

(define-compound-unit/infer expand@
  (import terms^ terms-extra^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ parser^)
  (export expand^)
  (link   red@ expand/red@))
