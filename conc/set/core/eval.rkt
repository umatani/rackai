#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ env^ store^ cont^ eval^)
 (only-in "../../../terms.rkt"
          Var% Fun% App% If% Bool% VFun% Prim%)
 (only-in "../../base/core/config.rkt" config^ #%term-forms)
 (only-in "../../base/core/eval.rkt" [--> base:-->]))
(provide --> eval@ red@)

;; Revised reduction rules

;; --> : State -> (Setof State)
(define-reduction (--> delta) #:super (base:--> delta <-)
  #:within-signatures [(only config^
                             AstEnv% KApp% KIf% SApp% SIf%)
                       (only terms-extra^
                             val?)
                       (only env^
                             lookup-env extend-env)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)])

(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import (only config^
                AstEnv%)
          (only terms-extra^
                val?)
          (only env^
                init-env)
          (only store^
                init-store))
  (export eval^)
  (inherit [red@ reducer])
  
  (use-terms AstEnv)

  (define (--> delta) (reducer delta))

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate delta ast)
    (define -->d (--> delta))
    (match-let ([(set `(,(? val? val) • ,_store) ...)
                 (apply-reduction-relation*
                  -->d `(,(AstEnv ast (init-env)) • ,(init-store)))])
      (list->set val))))
