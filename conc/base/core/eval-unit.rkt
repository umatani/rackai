#lang racket
(require "../set.rkt" "../dprint.rkt" "../reduction.rkt"

         "../struct-sig.rkt"
         "../env-sig.rkt"
         "../store-sig.rkt"
         "../cont-sig.rkt"
         "../delta-sig.rkt"
         "../eval-sig.rkt")
(provide eval-red@ eval@)

;; ----------------------------------------
;; Evaluating AST:


;; (: --> : State -> (Setof State))
(define-reduction (--> delta :=<1>)
  #:within-signatures [struct^ env^ store^ cont^]

  ;; propagate env into subterms
  [`(,(AstEnv (If ast_test ast_then ast_else) env) ,cont ,store)
   `(,(sif (ast&env ast_test env)
           (ast&env ast_then env)
           (ast&env ast_else env)) ,cont ,store)
   ev-env-if]

  [`(,(AstEnv (App ast_fun ast_args) env) ,cont ,store)
   `(,(sapp '()
            (cons (ast&env ast_fun env)
                  (map (λ (arg) (ast&env arg env)) ast_args)))
     ,cont ,store)
   ev-env-app]

  ;; value
  [`(,(AstEnv (? val? val) _) ,cont ,store)
   `(,val ,cont ,store)
   ev-val]

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cont ,store)
   #:with val :=<1> (lookup-store store (lookup-env env var))
   `(,(ast&env val env) ,cont ,store)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun vars ast) env) ,cont ,store)
   `(,(ast&env (vfun vars ast env) env) ,cont ,store)
   ev-lam]

  ;; application
  [`(,(SApp `(,vals ...) `(,tm ,tms ...)) ,cont ,store)
   #:with (values loc_new store_1) := (push-cont store cont)
   `(,tm ,(kapp vals  tms loc_new) ,store_1)
   ev-push-app]

  [`(,(? val? val) ,(KApp vals tms loc_cont) ,store)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(sapp (append vals (list val)) tms) ,cont ,store)
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
   `(,(ast&env ast env_new) ,cont ,store_2)
   ev-β]

  ;; primitive application
  [`(,(SApp vals '()) ,cont ,store)
   #:when (and (pair? vals) (prim? (car vals)))
   `(,(delta (car vals) (cdr vals)) ,cont ,store)
   ev-delta]

  ;; if
  [`(,(SIf (? (λ (x) (not (val? x))) ser_test) tm_then tm_else) ,cont ,store)
   #:with (values loc_new store_1) := (push-cont store cont)
   `(,ser_test ,(kif tm_then tm_else loc_new) ,store_1)
   ev-push-if]

  [`(,(? val? val) ,(KIf tm_then tm_else loc_cont) ,store)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(sif val tm_then tm_else) ,cont ,store)
   ev-pop-if]

  [`(,(SIf #f _ tm_else) ,cont ,store)
   `(,tm_else ,cont ,store)
   ev-if-#f]

  [`(,(SIf (? val? val) tm_then _) ,cont ,store)
   #:when (not (equal? val #f))
   `(,tm_then ,cont ,store)
   ev-if-#t])

(define eval-red@ (reduction->unit -->))

(define-unit eval@
  (import (only struct^ ast&env val?)
          (only env^ init-env)
          (only store^ init-store)
          (only delta^ delta)
          (only red^ reducer))
  (export eval^)

  (define --> (reducer delta :=))

  ; (: evaluate : Ast -> Val)
  (define (evaluate ast)
    (match-let ([(set `(,(? val? val) • ,_store))
                 (apply-reduction-relation*
                  --> `(,(ast&env ast (init-env)) • ,(init-store)))])
      val)))
