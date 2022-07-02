#lang racket
(require
 (except-in racket set do)
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt"            use-terms)

 (only-in "../../base/phases/terms.rkt"  terms^ #%term-forms)
 (only-in "../../../terms-extra.rkt"     terms-extra^)
 (only-in "../../../syntax-sig.rkt"      syntax^)
 (only-in "../../../env-sig.rkt"         env^)
 (only-in "../../../store-sig.rkt"       store^)
 (only-in "../../../eval-sig.rkt"        eval^)
 (only-in "../../../phase-sig.rkt"       phase^)
 (only-in "../../../menv-sig.rkt"        menv^)
 (only-in "../../../mstore-sig.rkt"      mstore^)
 (only-in "../../../mcont-sig.rkt"       mcont^)
 (only-in "../../../parser-sig.rkt"      parser^)
 (only-in "../../../expand-sig.rkt"      expand^)

 (only-in "../../base/phases/expand.rkt" [==> base:==>]))
(provide (all-defined-out))

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> <- -->)
  #:within-signatures [(only terms^
                             App% Sym% Stx% TVar% AstEnv% Stxξ% κ% ζ%
                             InEval% Hole%)
                       (only terms-extra^
                             id? val? atom? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip snoc add flip union in-hole)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only phase^
                             prune at-phase)
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

  ; expand : Ph Stx ξ Scps Σ -> (Setof (Cons Stx Σ))
  (define (expand ph stx ξ scps_p Σ)
    (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                   (apply-reduction-relation* ==> init-ζ)])
        (list->set (map cons stx_new Σ_new)))))

  ; expander : Stx -> (Values Stx Σ)
  (define (expander stx)
    (expand 0 stx (init-ξ) (set) (init-Σ))))
