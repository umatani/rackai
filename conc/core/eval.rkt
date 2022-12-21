#lang racket
(require
 "../../set.rkt"
 "../../reduction.rkt"
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt" domain^ env^ store^ cont^ eval^)
 (only-in "terms.rkt" #%term-forms
          Var% Fun% App% If% Bool% VFun% Prim%
          AstEnv% KApp% KIf% SApp% SIf%))
(provide --> eval@)

;; ----------------------------------------
;; Evaluating AST:

;; --> : State -> (Setof State)
(define-reduction (--> delta :=<1>)
  #:within-signatures [(only domain^
                             val?)
                       (only env^
                             lookup-env extend-env)
                       (only store^
                             lookup-store alloc-loc* update-store*)
                       (only cont^
                             push-cont)]
  #:do [(use-terms Var Fun App If Bool VFun Prim AstEnv KApp KIf SApp SIf)]

  ;; propagate env into subterms
  [`(,(AstEnv (If lbl ast_test ast_then ast_else) env) ,cont ,store)
   `(,(SIf lbl
           (AstEnv ast_test env)
           (AstEnv ast_then env)
           (AstEnv ast_else env)) ,cont ,store)
   ev-env-if]

  [`(,(AstEnv (App lbl ast_fun ast_args) env) ,cont ,store)
   `(,(SApp lbl
            '()
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
   #:with loc :=<1> (lookup-env env var)
   #:with val :=<1> (lookup-store store loc)
   `(,(AstEnv val env) ,cont ,store)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun vars ast) env) ,cont ,store)
   `(,(AstEnv (VFun vars ast env) env) ,cont ,store)
   ev-lam]

  ;; application
  [`(,(SApp lbl `(,vals ...) `(,tm ,tms ...)) ,cont ,store)
   #:with (values loc_new store_1) := (push-cont store lbl cont)
   `(,tm ,(KApp lbl vals  tms loc_new) ,store_1)
   ev-push-app]

  [`(,(? val? val) ,(KApp lbl vals tms loc_cont) ,store)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SApp lbl (append vals (list val)) tms) ,cont ,store)
   ev-pop-app]

  ;; β
  [`(,(SApp _lbl vals '()) ,cont ,store)
   #:when (and (pair? vals) (VFun? (car vals)))
   #:with (values vars ast env vals) := (let ([f (car vals)])
                                          (values (VFun-vars f)
                                                  (VFun-ast f)
                                                  (VFun-env f)
                                                  (cdr vals)))
   #:with                       nams := (map Var-nam vars)
   #:with      (values locs store_1) := (alloc-loc* nams store)
   #:with                    env_new := (extend-env env vars locs)
   #:with                    store_2 := (update-store* store_1 locs vals)
   `(,(AstEnv ast env_new) ,cont ,store_2)
   ev-β]

  ;; primitive application
  [`(,(SApp _lbl vals '()) ,cont ,store)
   #:when (and (pair? vals) (Prim? (car vals)))
   #:with val :=<1> (delta (car vals) (cdr vals))
   `(,val ,cont ,store)
   ev-delta]

  ;; if
  [`(,(SIf lbl (? (λ (x) (not (val? x))) ser_test)
           tm_then tm_else) ,cont ,store)
   #:with (values loc_new store_1) := (push-cont store lbl cont)
   `(,ser_test ,(KIf lbl tm_then tm_else loc_new) ,store_1)
   ev-push-if]

  [`(,(? val? val) ,(KIf lbl tm_then tm_else loc_cont) ,store)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SIf lbl val tm_then tm_else) ,cont ,store)
   ev-pop-if]

  [`(,(SIf _lbl (Bool #f) _ tm_else) ,cont ,store)
   `(,tm_else ,cont ,store)
   ev-if-#f]

  [`(,(SIf _lbl (? val? val) tm_then _) ,cont ,store)
   #:when (not (equal? val (Bool #f)))
   `(,tm_then ,cont ,store)
   ev-if-#t])

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

  (define (--> delta) (reducer delta :=))

  ; evaluate : Ast -> Val
  (define (evaluate delta ast)
    (define -->d (--> delta))
    (match-let ([(set `(,(? val? val) • ,_store))
                 (apply-reduction-relation*
                  -->d `(,(AstEnv ast (init-env)) • ,(init-store)))])
      val)))
