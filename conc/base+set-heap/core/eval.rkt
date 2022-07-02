#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt"        use-terms)

 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)
 (only-in "../../../terms-extra.rkt"  terms-extra^)
 (only-in "../../../env-sig.rkt"      env^)
 (only-in "../../../store-sig.rkt"    store^)
 (only-in "../../../cont-sig.rkt"     cont^)
 (only-in "../../../delta-sig.rkt"    delta^)
 (only-in "../../../eval-sig.rkt"     eval^)

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

(define eval-red@ (reduction->unit -->))

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

