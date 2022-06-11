#lang racket/unit
(require (except-in racket set do)
         "../set.rkt" "../dprint.rkt" "../reduction.rkt"

         "../struct-sig.rkt"
         (only-in "../delta.rkt" delta^)
         "../eval-sig.rkt")

(import (only struct^
              store Store-tbl Store-size Var? Var-nam
              Fun VFun? VFun-vars VFun-ast VFun-env vfun
              App SApp sapp KApp kapp 
              If SIf sif KIf kif val? prim? AstEnv ast&env)
        (only delta^
              delta))
(export eval^)


;; ----------------------------------------
;; Evaluating AST:

; (: init-env : -> Env)
(define (init-env) (make-immutable-hash))

;(: lookup-env : Env Var -> Loc)
(define (lookup-env env var) (hash-ref env var))

;(: update-env : Env (Listof Var) (Listof Loc) -> Env)
(define (update-env env vars locs)
  (foldl (λ (v l e) (hash-set e v l))
         env vars locs))

; (: init-store : -> Store)
(define (init-store) (store 0 (make-immutable-hash)))

;(: lookup-store : Store Loc -> (U Val Cont))
(define (lookup-store st loc)
  (dprint 'core 'lookup-store "")
  (hash-ref (Store-tbl st) loc))

;(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store st loc u)
  (dprint 'core 'update-store "")
  (store (Store-size st)
         (hash-set (Store-tbl st) loc u)))

;(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* st locs us)
  (dprint 'core 'update-store* "")
  (store (Store-size st)
         (foldl (λ (l u t) (hash-set t l u))
                (Store-tbl st) locs us)))

;(: alloc-loc : Store -> (Values Loc Store))
(define (alloc-loc st)
  (dprint 'core 'alloc-loc "")
  (let ([size (Store-size st)])
    (values (string->symbol (format "l~a" size))
            (store (add1 size) (Store-tbl st)))))

;; for eval-time value binding
;(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
(define (alloc-loc* nams st)
  (dprint 'core 'alloc-loc* "")
  (match nams
    ['() (values '() st)]
    [(list nam1 nams ...)
     (let* ([size (Store-size st)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (store (add1 size) (Store-tbl st)))])
         (values (cons loc_0 locs_new) store_new)))]))

;(: push-cont : Store Cont -> (Values Loc Store))
(define (push-cont st cont)
  (let-values ([(loc store_1) (alloc-loc st)])
    (let ([store_2 (update-store store_1 loc cont)])
      (values loc store_2))))


;; (: --> : State -> (Setof State))
(define-parameterized-reduction-relation (-->/store delta :=<1>)

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

(define --> ((reducer-of -->/store) delta :=))

; (: eval : Ast -> Val)
(define ((eval/--> -->) ast)
  (match-let ([(set `(,(? val? val) • ,_store))
               (apply-reduction-relation*
                --> `(,(ast&env ast (init-env)) • ,(init-store)))])
    val))

(define eval (eval/--> -->))
(define evaluate eval)
