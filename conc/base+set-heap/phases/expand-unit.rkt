#lang racket
(require (except-in racket set do)
         "../../../set.rkt"
         "../../../reduction.rkt"

         (only-in "../../../struct-common-sig.rkt" struct-common^)
         (only-in "../../base/phases/struct-stxe-sig.rkt" struct-stxe^)
         (only-in "../../../syntax-sig.rkt"   syntax^)
         (only-in "../../../env-sig.rkt"      env^)
         (only-in "../../../store-sig.rkt"    store^)
         (only-in "../../../eval-sig.rkt"     eval^)
         (only-in "../../../phase-sig.rkt"    phase^)
         (only-in "../../../menv-sig.rkt"     menv^)
         (only-in "../../../mstore-sig.rkt"   mstore^)
         (only-in "../../../mcont-sig.rkt"    mcont^)
         (only-in "../../../parse-sig.rkt"    parse^)
         (only-in "../../../expand-sig.rkt"   expand^)

         (only-in "../../base/phases/expand-unit.rkt" [==> base:==>])

         ;; (only-in "../../base/core/syntax.rkt" zip unzip snoc union)
         ;; (only-in "../../base/phases/syntax.rkt"
         ;;          empty-ctx in-hole add flip prune at-phase)
         ;; (only-in "../../base/core/eval.rkt" init-env init-store)
         ;; (only-in "../../base/core/expand.rkt"
         ;;          init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ)
         ;; (only-in "../../base/phases/expand.rkt"
         ;;          regist-vars/bind/alloc-name
         ;;          id-seq id-kont id-snoc stx-nil
         ;;          [==>p/Σ base:==>p/Σ]))

         ;; Set-based version
         ;; (only-in "../core/eval.rkt" -->c)
         ;; (only-in "../core/expand.rkt" alloc-name alloc-scope)
         ;; (only-in "syntax.rkt" bind resolve id=?)
         ;; (only-in "parse.rkt" parse)
         )


;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> <- -->)
  #:within-signatures [struct-common^ struct-stxe^ syntax^ env^ store^ phase^
                       menv^ mstore^ mcont^ parse^])

(define expand-red@ (reduction->unit ==>))


(define-unit expand@
  (import (only struct-common^ ζ mk-ζ)
          (only struct-stxe^ stx&ξ)
          (only menv^ init-ξ)
          (only mstore^ init-Σ)
          (only mcont^ init-Θ)
          (only eval^ -->)
          (only red^ reducer))
  (export expand^)
  
  (define ==> (reducer -->))

  ; expand : Ph Stx ξ Scps Σ -> (Setof (Cons Stx Σ))
  (define (expand ph stx ξ scps_p Σ)
    (let ([init-ζ (mk-ζ (stx&ξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                   (apply-reduction-relation* ==> init-ζ)])
        (list->set (map cons stx_new Σ_new)))))

  ; expander : Stx -> (Values Stx Σ)
  (define (expander stx)
    (expand 0 stx (init-ξ) (set) (init-Σ))))
