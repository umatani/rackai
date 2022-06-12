#lang racket/unit
(require "../../base/set.rkt"
         "../../base/reduction.rkt"

         "../../base/struct-sig.rkt"
         ;(only-in "../../base/delta.rkt" delta^)
         "../../base/eval-sig.rkt"

         #;
         (only-in "../../base/core/eval-unit.rkt"
                  [-->/store base:-->/store]
                  ;init-env lookup-env update-env
                  ;init-store push-cont/alloc-loc/update-store
                  ))

(import struct^
        (prefix base: eval^))
(export eval^)

(define init-env   base:init-env)
(define lookup-env base:lookup-env)
(define update-env base:update-env)


;; Revised reduction rules

;; (: --> : State -> (Setof State))
(define-reduction (-->/store delta) (base:-->/store delta <-))

(define --> ((reducer-of -->/store) delta))

; (: eval : Ast -> (Setof Val))
(define ((eval/--> -->) ast)
  (match-let ([(set `(,(? Val? val) • ,_store) ...)
               (apply-reduction-relation*
                --> `(,(AstEnv ast (init-env)) • ,(init-store)))])
    (list->set val)))

(define eval (eval/--> -->))
(define evaluate eval)
