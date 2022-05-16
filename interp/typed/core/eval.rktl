;; ----------------------------------------
;; Evaluating AST:


;; (: -->c : State -> (Setof State))
(define-reduction-relation -->c State ζ
  ;; propagate env into subterms
  [`(,(AstEnv (If ast_test ast_then ast_else) env) ,cont ,store)
   `(,(SIf (AstEnv ast_test env)
           (AstEnv ast_then env)
           (AstEnv ast_else env)) ,cont ,store)
   ev-env-if]

  [`(,(AstEnv (App ast_fun ast_args) env) ,cont ,store)
   `(,(SApp '()
            (cons (AstEnv ast_fun env)
                  (map (λ ([arg : Ast]) (AstEnv arg env)) ast_args)))
     ,cont ,store)
   ev-env-app]

  ;; value
  [`(,(AstEnv (? Val? val) _) ,cont ,store)
   `(,val ,cont ,store)
   ev-val]

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cont ,store)
   `(,(AstEnv (cast (lookup-store store (lookup-env env var)) Val) env)
     ,cont ,store)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun vars ast) env) ,cont ,store)
   `(,(AstEnv (VFun vars ast env) env) ,cont ,store)
   ev-lam]

  ;; application
  [`(,(SApp `(,vals ...) `(,tm ,tms ...)) ,cont ,store)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,tm ,(KApp vals  tms loc_new) ,store_1))
   ev-push-app]

  [`(,(? Val? val) ,(KApp vals tms loc_cont) ,store)
   `(,(SApp (append vals (list val)) tms)
     ,(cast (lookup-store store loc_cont) Cont) ,store)
   ev-pop-app]

  ;; β
  [`(,(SApp vals '()) ,cont ,store)
   #:when (and (pair? vals)
               (VFun? (car vals)))
   (let*-values ([(vars ast env vals) (let ([f(car vals)])
                                        (values (VFun-vars f)
                                                (VFun-ast f)
                                                (VFun-env f)
                                                (cdr vals)))]
                 [(nams) (map Var-nam vars)]
                 [(locs store_1) (alloc-loc* nams store)]
                 [(env_new) (update-env env vars locs)]
                 [(store_2) (update-store* store_1 locs vals)])
     `(,(AstEnv ast env_new) ,cont ,store_2))
   ev-β]

  ;; primitive application
  [`(,(SApp vals '()) ,cont ,store)
   #:when (and (pair? vals)
               (Prim? (car vals)))
   `(,(δ (car vals) (cdr vals)) ,cont ,store)
   ev-δ]

  ;; if
  [`(,(SIf (? (λ (x) (not (Val? x))) ser_test) tm_then tm_else) ,cont ,store)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,ser_test ,(KIf tm_then tm_else loc_new) ,store_1))
   ev-push-if]

  [`(,(? Val? val) ,(KIf tm_then tm_else loc_cont) ,store)
   `(,(SIf val tm_then tm_else)
     ,(cast (lookup-store store loc_cont) Cont) ,store)
   ev-pop-if]

  [`(,(SIf #f _ tm_else) ,cont ,store)
   `(,tm_else ,cont ,store)
   ev-if-#f]

  [`(,(SIf (? Val? val) tm_then _) ,cont ,store)
   #:when (not (equal? val #f))
   `(,tm_then ,cont ,store)
   ev-if-#t])

(: eval : Ast -> Val)
(define (eval ast)
  (match-let ([`((,(? Val? val) • ,_store))
               (apply-reduction-relation*
                (reducer-of -->c)
                `(,(AstEnv ast (init-env)) • ,(init-store)))])
    val))

;; for debug

(: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  ((reducer-of -->c)
   `(,(AstEnv (cast (run form 'parse) Ast) (init-env)) • ,(init-store))))

(: eval-->* : Sexp -> (Listof State))
(define (eval-->* form)
  (apply-reduction-relation*
   (reducer-of -->c)
   `(,(AstEnv (cast (run form 'parse) Ast) (init-env)) • ,(init-store))))
