#lang racket
(require "../../../set.rkt"
         "../../../reduction.rkt"

         "../../../struct-common-sig.rkt"
         "../../base/core/struct-stxe-sig.rkt"
         "../../../syntax-sig.rkt"
         "../../../env-sig.rkt"
         "../../../store-sig.rkt"
         "../../../menv-sig.rkt"
         "../../../mstore-sig.rkt"
         "../../../mcont-sig.rkt"
         "../../../eval-sig.rkt"
         "../../../parse-sig.rkt"
         "../../../expand-sig.rkt"

         (only-in "../../base/core/expand-unit.rkt" [==> base:==>]))
(provide expand-red@ expand@)


;; Revised reduction rules

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [struct-common^ struct-stxe^ syntax^ env^ store^
                       menv^ mstore^ mcont^ parse^])

(define expand-red@ (reduction->unit ==>))

(define-unit expand@
  (import (only struct-common^ ζ mk-ζ)
          (only struct-stxe^ stx&ξ)
          (only mcont^ init-Θ)
          (only menv^ init-ξ)
          (only mstore^ init-Σ)
          (only eval^ -->)
          (only red^ reducer))
  (export expand^)

  (define ==> (reducer -->))

  ; expand : Stx ξ Σ -> (Setof (Cons Stx Σ))
  (define (expand stx0 ξ Σ)
    (let ([init-ζ (mk-ζ (stx&ξ stx0 ξ) '∘ '• (init-Θ) Σ)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new) ...)
                   (apply-reduction-relation* ==> init-ζ)])
        (list->set (map cons stx_new Σ_new)))))

  ; expander : Stx -> (Cons Stx Σ)
  (define (expander stx)
    (expand stx (init-ξ) (init-Σ))))
