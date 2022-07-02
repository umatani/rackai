#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt"        use-terms)

 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)
 (only-in "../../../terms-extra.rkt"  terms-extra^)
 (only-in "../../../syntax-sig.rkt"   syntax^)
 (only-in "../../../env-sig.rkt"      env^)
 (only-in "../../../store-sig.rkt"    store^)
 (only-in "../../../menv-sig.rkt"     menv^)
 (only-in "../../../mstore-sig.rkt"   mstore^)
 (only-in "../../../mcont-sig.rkt"    mcont^)
 (only-in "../../../eval-sig.rkt"     eval^)
 (only-in "../../../parser-sig.rkt"   parser^)
 (only-in "../../../expand-sig.rkt"   expand^)

 (only-in "../../base/core/expand.rkt" [==> base:==>]))
(provide expand-red@ expand@)


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

(define expand-red@ (reduction->unit ==>))

(define-unit expand@
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
