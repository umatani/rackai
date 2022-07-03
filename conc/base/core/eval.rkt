#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ env^ store^ cont^ delta^ eval^)
 (only-in "terms.rkt" terms^ #%term-forms))
(provide eval-red@ eval@ -->)

;; ----------------------------------------
;; Evaluating AST:

;; --> : State -> (Setof State)
(define-reduction (--> delta :=<1>)
  #:within-signatures [(only terms^
                             Var% Fun% App% If% VFun% AstEnv%
                             KIf% KApp% SIf% SApp%)
                       (only terms-extra^
                             val? prim?)
                       (only env^
                             lookup-env update-env)
                       (only store^
                             lookup-store alloc-loc* update-store*)
                       (only cont^
                             push-cont)]
  #:do [(use-terms Var Fun App If VFun AstEnv KApp KIf SApp SIf)]

  ;; propagate env into subterms
  [`(,(AstEnv (If ast_test ast_then ast_else) env) ,cont ,store)
   `(,(SIf (AstEnv ast_test env)
           (AstEnv ast_then env)
           (AstEnv ast_else env)) ,cont ,store)
   ev-env-if]

  [`(,(AstEnv (App ast_fun ast_args) env) ,cont ,store)
   `(,(SApp '()
            (cons (AstEnv ast_fun env)
                  (map (λ (arg) (AstEnv arg env)) ast_args)))
     ,cont ,store)
   ev-env-app]

  ;; value
  [`(,(AstEnv (? val? val) _) ,cont ,store)
   `(,val ,cont ,store)
   ev-val]

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cont ,store)
   #:with val :=<1> (lookup-store store (lookup-env env var))
   `(,(AstEnv val env) ,cont ,store)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun vars ast) env) ,cont ,store)
   `(,(AstEnv (VFun vars ast env) env) ,cont ,store)
   ev-lam]

  ;; application
  [`(,(SApp `(,vals ...) `(,tm ,tms ...)) ,cont ,store)
   #:with (values loc_new store_1) := (push-cont store cont)
   `(,tm ,(KApp vals  tms loc_new) ,store_1)
   ev-push-app]

  [`(,(? val? val) ,(KApp vals tms loc_cont) ,store)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SApp (append vals (list val)) tms) ,cont ,store)
   ev-pop-app]

  ;; β
  [`(,(SApp vals '()) ,cont ,store)
   #:when (and (pair? vals) (VFun? (car vals)))
   #:with (values vars ast env vals) := (let ([f(car vals)])
                                          (values (VFun-vars f)
                                                  (VFun-ast f)
                                                  (VFun-env f)
                                                  (cdr vals)))
   #:with                       nams := (map Var-nam vars)
   #:with      (values locs store_1) := (alloc-loc* nams store)
   #:with                    env_new := (update-env env vars locs)
   #:with                    store_2 := (update-store* store_1 locs vals)
   `(,(AstEnv ast env_new) ,cont ,store_2)
   ev-β]

  ;; primitive application
  [`(,(SApp vals '()) ,cont ,store)
   #:when (and (pair? vals) (prim? (car vals)))
   `(,(delta (car vals) (cdr vals)) ,cont ,store)
   ev-delta]

  ;; if
  [`(,(SIf (? (λ (x) (not (val? x))) ser_test) tm_then tm_else) ,cont ,store)
   #:with (values loc_new store_1) := (push-cont store cont)
   `(,ser_test ,(KIf tm_then tm_else loc_new) ,store_1)
   ev-push-if]

  [`(,(? val? val) ,(KIf tm_then tm_else loc_cont) ,store)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SIf val tm_then tm_else) ,cont ,store)
   ev-pop-if]

  [`(,(SIf #f _ tm_else) ,cont ,store)
   `(,tm_else ,cont ,store)
   ev-if-#f]

  [`(,(SIf (? val? val) tm_then _) ,cont ,store)
   #:when (not (equal? val #f))
   `(,tm_then ,cont ,store)
   ev-if-#t])

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

  (define --> (reducer delta :=))

  ; evaluate : Ast -> Val
  (define (evaluate ast)
    (match-let ([(set `(,(? val? val) • ,_store))
                 (apply-reduction-relation*
                  --> `(,(AstEnv ast (init-env)) • ,(init-store)))])
      val)))
