#lang racket
(require "../../../set.rkt"
         "../../../reduction.rkt"

         "../../../struct-sig.rkt"
         "../../../env-sig.rkt"
         "../../../store-sig.rkt"
         "../../../cont-sig.rkt"
         "../../../delta-sig.rkt"
         "../../../eval-sig.rkt"

         (only-in "../../base/core/eval-unit.rkt" [--> base:-->]))
(provide eval@ eval-red@)

;; Revised reduction rules

;; --> : State -> (Setof State)
(define-reduction (--> delta) #:super (base:--> delta <-)
  #:within-signatures [struct^ env^ store^ cont^])

(define eval-red@ (reduction->unit -->))

(define-unit eval@
  (import (only struct^ ast&env val?)
          (only env^ init-env)
          (only store^ init-store)
          (only delta^ delta)
          (only red^ reducer))
  (export eval^)

  (define --> (reducer delta))

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate ast)
    (match-let ([(set `(,(? val? val) • ,_store) ...)
                 (apply-reduction-relation*
                  --> `(,(ast&env ast (init-env)) • ,(init-store)))])
      (list->set val))))

