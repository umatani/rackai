;; ----------------------------------------
;; Evaluating AST:

(: lookup-env : Env Var -> Loc)
(define (lookup-env env var) (hash-ref env var))

(: update-env : Env (Listof Var) (Listof Loc) -> Env)
(define (update-env env vars locs)
  (foldl (λ ([v : Var] [l : Loc] [e : Env]) (hash-set e v l))
         env vars locs))

(: lookup-store : Store Loc -> (U Val Cont))
(define (lookup-store store loc)
  (hash-ref (Store-tbl store) loc))

(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store store loc u)
  (Store (Store-size store)
         (hash-set (Store-tbl store) loc u)))

(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* store locs us)
  (Store (Store-size store)
         (foldl (λ ([l : Loc]
                     [u : (U Val Cont)]
                     [t : (HashTable Loc (U Val Cont))])
                  (hash-set t l u))
                (Store-tbl store) locs us)))

(: alloc-loc : Store -> (Values Loc Store))
(define (alloc-loc store)
  (let ([size (Store-size store)])
    (values (string->symbol (format "l~a" size))
            (Store (add1 size) (Store-tbl store)))))

;; for eval-time value binding
(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
(define (alloc-loc* nams store)
  (match nams
    ['() (values '() store)]
    [(list nam1 nams ...)
     (let* ([size (Store-size store)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (Store (add1 size) (Store-tbl store)))])
         (values (cons loc_0 locs_new) store_new)))]))

(: push-cont : Store Cont -> (Values Loc Store))
(define (push-cont store cont)
  (let-values ([(loc store_1) (alloc-loc store)])
    (let ([store_2 (update-store store_1 loc cont)])
      (values loc store_2))))

;(: -->c : (-> State (Setof State)))
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
  [`(,(SApp `(,vals ...) `(,clo ,clos ...)) ,cont ,store)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,clo ,(KApp vals  clos loc_new) ,store_1))
   ev-push-app]

  [`(,(? Val? val) ,(KApp vals clos loc_cont) ,store)
   `(,(SApp (append vals (list val)) clos)
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
  [`(,(SIf (? (λ (x) (not (Val? x))) ser_test) clo_then clo_else) ,cont ,store)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,ser_test ,(KIf clo_then clo_else loc_new) ,store_1))
   ev-push-if]

  [`(,(? Val? val) ,(KIf clo_then clo_else loc_cont) ,store)
   `(,(SIf val clo_then clo_else)
     ,(cast (lookup-store store loc_cont) Cont) ,store)
   ev-pop-if]

  [`(,(SIf #f _ clo_else) ,cont ,store)
   `(,clo_else ,cont ,store)
   ev-if-#f]

  [`(,(SIf (? Val? val) clo_then _) ,cont ,store)
   #:when (not (equal? val #f))
   `(,clo_then ,cont ,store)
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
