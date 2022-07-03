#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ env^ store^ cont^ delta^ eval^)
 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)

 (only-in "../../base/core/eval.rkt" [--> base:-->]))
(provide eval@ eval-red@)

;; Revised reduction rules

;; --> : State -> (Setof State)
(define-reduction (--> delta) #:super (base:--> delta <-)
  #:within-signatures [(only terms^
                             Var% Fun% App% If% VFun% KApp% KIf% SApp% SIf%
                             AstEnv%)
                       (only terms-extra^
                             val? prim?)
                       (only env^
                             lookup-env update-env)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)])

(define-unit-from-reduction eval-red@ -->)

(define-unit eval@
  (import (only terms^
                AstEnv%)
          (only terms-extra^
                val?)
          (only env^
                init-env)
          (only store^
                init-store)
          (only delta^
                delta)
          (only red^
                reducer))
  (export eval^)

  (use-terms AstEnv)

  (define --> (reducer delta))

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate ast)
    (match-let ([(set `(,(? val? val) • ,_store) ...)
                 (apply-reduction-relation*
                  --> `(,(AstEnv ast (init-env)) • ,(init-store)))])
      (list->set val))))

