#lang racket
(require "../../../set.rkt"
         "../../../reduction.rkt"

         "../../../struct-sig.rkt"
         "../../../delta-sig.rkt"
         "../../../eval-sig.rkt"

         (reduction-in "../../base/core/eval-unit.rkt" -->))


;; Revised reduction rules

;; --> : State -> (Setof State)
(define-reduction (---> delta) #:super (--> delta <-))


(define-unit eval@
  (import struct^
          (prefix base: eval^))
  (export #;eval^
          ))


;; (define --> ((reducer-of -->/store) delta))

;; ; eval : Ast -> (Setof Val)
;; (define (eval ast)
;;   (match-let ([(set `(,(? Val? val) • ,_store) ...)
;;                (apply-reduction-relation*
;;                 --> `(,(AstEnv ast (init-env)) • ,(init-store)))])
;;     (list->set val)))

;; (define evaluate eval)
