#lang racket
(require "../../../set.rkt"
         "../../../reduction.rkt"

         "../../../struct-sig.rkt"
         "../../../mstore-sig.rkt"
         "../../../mcont-sig.rkt"

         (only-in "../../base/core/expand-unit.rkt" [==> base:==>]))


;; Revised reduction rules

;; (: ==> : ζ -> (Setof ζ))
;; TODO: inherit #:do definitions by importing necessary units
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [struct^ mstore^ mcont^])

#;(define ==>c ((reducer-of ==>c/Σ) -->c))

;(: expand : Stx ξ Σ -> (Setof (Cons Stx Σ))
#;
(define ((expand/==> ==>) stx ξ Σ)
  (let ([init-ζ (ζ (Stxξ stx ξ) '∘ '• (init-Θ) Σ)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                 (apply-reduction-relation* ==> init-ζ)])
      (list->set (map cons stx_new Σ_new)))))

#;(define expand (expand/==> ==>c))

