#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt"          use-terms)

 (only-in "../../base/full/terms.rkt"  terms^ #%term-forms)
 (only-in "../../../terms-extra.rkt"   terms-extra^)
 (only-in "../../../syntax-sig.rkt"    syntax^)
 (only-in "../../../env-sig.rkt"       env^)
 (only-in "../../../eval-sig.rkt"      eval^)
 (only-in "../../../store-sig.rkt"     store^)
 (only-in "../../../menv-sig.rkt"      menv^)
 (only-in "../../../mstore-sig.rkt"    mstore^)
 (only-in "../../../mcont-sig.rkt"     mcont^)
 (only-in "../../../parser-sig.rkt"    parser^)
 (only-in "../../../expand-sig.rkt"    expand^)
 (only-in "../../../phase-sig.rkt"     phase^)

 (only-in "../../base/full/expand.rkt" [==> base:==>]))
(provide (all-defined-out))


(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only terms^
                             App% Sym% Stx% AstEnv% Stxξ% κ% Σ*%
                             TVar% TStop% ζ% InEval% Hole%)
                       (only terms-extra^
                             id? val? atom? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip snoc add flip union in-hole)
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
                             parse)
                       (only phase^
                             prune at-phase)])

(define expand-red@ (reduction->unit ==>))

(define-unit expand@
  (import (only terms^
                Stxξ% Σ*% ζ%)
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

  (use-terms Stxξ Σ* ζ)

  (define ==> (λ () (reducer -->)))
  
  ; expand : Ph Stx ξ Σ* -> (Setof (Cons Stx Σ*))
  (define (expand ph stx ξ Σ*)
    (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• (init-Θ) Σ*)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ*_new) ...)
                   (apply-reduction-relation* (==>) init-ζ)])
        (list->set (map cons stx_new Σ*_new)))))

  ; expander : Stx -> (Setof (Cons Stx Σ*))
  (define (expander stx)
    (expand 0 stx (init-ξ) (Σ* (init-Σ) (set) (set)))))
