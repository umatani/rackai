#lang racket
(require
 (except-in racket set do)
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ eval^ 
          menv^ mstore^ mcont^ parser^ expand^)
 (only-in "../../base/phases/terms.rkt"  terms^ #%term-forms)

 (only-in "../../base/phases/expand.rkt" [==> base:==>]))
(provide ==> expand@)

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> <- -->)
  #:within-signatures [(only terms^
                             App% Sym% Stx% TVar% AstEnv% Stxξ% κ% ζ%
                             InEval% Hole%)
                       (only terms-extra^
                             id? val? atom? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip snoc add flip union in-hole
                             prune at-phase)
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

  ; expand : Ph Stx ξ Scps Σ -> (Setof (Cons Stx Σ))
  (define (expand ph stx ξ scps_p Σ)
    (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                   (apply-reduction-relation* ==> init-ζ)])
        (list->set (map cons stx_new Σ_new)))))

  ; expander : Stx -> (Values Stx Σ)
  (define (expander stx)
    (expand 0 stx (init-ξ) (set) (init-Σ))))

(define-compound-unit/infer expand@
  (import terms^ terms-extra^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ parser^)
  (export expand^)
  (link   red@ expand/red@))
