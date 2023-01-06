#lang racket
(require
 "../../set.rkt"
 "../../reduction.rkt"
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          domain^ env^ store^ cont^ eval^)
 (only-in "../../conc/core/terms.rkt" #%term-forms
          Var% Fun% App% If% Bool% VFun% Prim%
          AstEnv% KApp% KIf% SApp% SIf%)
 (only-in "../../conc/core/eval.rkt" [--> base:-->]))
(provide --> red@ eval@)

;; Revised reduction rules

;; --> : State -> (Setof State)
(define-reduction (--> delta) #:super (base:--> delta <-)
  #:within-signatures [(only domain^
                             val?)
                       (only env^
                             lookup-env extend-env)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)])

(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import (only domain^
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