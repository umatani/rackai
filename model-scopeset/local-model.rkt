#lang racket
(require redex/reduction-semantics
         "common.rkt"
         slideshow/pict
         (rename-in (except-in "phases-model.rkt"
                               eval
                               expand
                               expand*)
                    [run phases:run])
         (for-syntax racket/list))

(provide Lloc
         eval eval* extend-env* unstop
         expand expand*
         run local:examples
         (all-from-out "phases-model.rkt"))

(define-extended-language Lloc Lph
  [maybe-scp scp no-scope]
  [Σ* (Tup Σ scps scps)])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction Lloc
  eval : ph ast maybe-scp env Σ* -> (values val Σ*)
  
  ;; local value
  [(eval ph (App syntax-local-value ast_id) scp_i env Σ*)
   (values val Σ*_2)
   (side-condition (printf "local-value start: ~a\n" (term ph)))
   (where (values id_result Σ*_2) (eval ph ast_id scp_i env Σ*))
   (where (Tup Σ_2 _ _) Σ*_2)
   (where nam (resolve ph id_result Σ_2))
   (side-condition (printf "local-value id_result: ~a\n" (term id_result)))
   (side-condition (printf "local-value nam: ~a\n" (term nam)))
   (side-condition (printf "local-value Σ_2: ~a\n" (term Σ_2)))
   (side-condition (printf "local-value nam: ~a\n" (term nam)))
   (where val (lookup-env env nam))]

  ;; local expand
  [(eval ph (App local-expand ast any_contextv ast_stops) scp_i env Σ*)
   (values (flip ph stx_exp scp_i) Σ*_4)
   (where (values stx Σ*_2) (eval ph ast scp_i env Σ*))
   (where (values val_idstops Σ*_3) (eval ph ast_stops scp_i env Σ*_2))
   (where env_unstops
          ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p))))) (term env)))
   (where (Tup Σ_3 _ _) Σ*_3)
   (where (nam_stop ...) (resolve* ph val_idstops Σ_3))
   (where env_stops
          (extend-env* env_unstops ((nam_stop (TStop (lookup-env env_unstops nam_stop))) ...)))
   (where (values stx_exp Σ*_4) (expand ph (flip ph stx scp_i) env_stops Σ*_3))]
  
  ;; local binder
  [(eval ph (App syntax-local-identifier-as-binding ast_id) scp_i env Σ*)
   (values (prune ph id_result scps_u2) Σ*_2)
   (where (values id_result Σ*_2) (eval ph ast_id scp_i env Σ*))
   (where (Tup _ _ scps_u2) Σ*_2)]

  ;; core cases
  [(eval ph (App ast_fun ast_arg ...) maybe-scp env Σ*)
   (eval ph (substs ast_body (var ...) (val_arg ...)) maybe-scp env Σ*_3)
   (where (values (Fun (var ...) ast_body) Σ*_2) (eval ph ast_fun maybe-scp env Σ*))
   (where (values (val_arg ...) Σ*_3) (eval* ph () (ast_arg ...) maybe-scp env Σ*_2))]
  [(eval ph (App prim ast_arg ...) maybe-scp env Σ*)
   (values (δ prim (val_arg ...)) Σ*_2)
   (where (values (val_arg ...) Σ*_2) (eval* ph () (ast_arg ...) maybe-scp env Σ*))]
  [(eval ph (If ast_test ast_then ast_else) maybe-scp env Σ*)
   (eval ph ast_else maybe-scp env Σ*_1)
   (where (values #f Σ*_1) (eval ph ast_test maybe-scp env Σ*))]
  [(eval ph (If ast_test ast_then ast_else) maybe-scp env Σ*)
   (eval ph ast_then maybe-scp env Σ*_1)
   (where (values _ Σ*_1) (eval ph ast_test maybe-scp env Σ*))]
  [(eval ph val maybe-scp env Σ*)
   (values val Σ*)])

(define-metafunction Lloc
  eval* : ph (val ...) (ast ...) maybe-scp env Σ* -> (values (val ...) Σ*)
  [(eval* ph (val ...) () maybe-scp env Σ*)
   (values (val ...) Σ*)]
  [(eval* ph (val ...) (ast_0 ast_1 ...) maybe-scp env Σ*)
   (eval* ph (val ... val_0) (ast_1 ...) maybe-scp env Σ*_2)
   (where (values val_0 Σ*_2) (eval ph ast_0 maybe-scp env Σ*))])

(define-metafunction Lloc
  extend-env* : env ((nam all-transform) ...) -> env
  [(extend-env* env ((nam all-transform) ...)) ((nam all-transform) ... . env)])

(define-metafunction Lloc
  unstop : all-transform -> all-transform
  [(unstop (TStop all-transform)) all-transform]
  [(unstop all-transform) all-transform])

(define-metafunction Lloc
  resolve* : ph val Σ -> (nam ...)
  [(resolve* ph () Σ) ()]
  [(resolve* ph (Cons id val) Σ)
   ((resolve ph id Σ) nam ...)
   (where (nam ...) (resolve* ph val Σ))])


;; ----------------------------------------
;; The expander:

(define-metafunction Lloc
  expand : ph stx env Σ* -> (values stx Σ*)
  
  ;; stops
  [(expand ph (Stx (Cons id_stop stl_args) ctx) env Σ*)
   (values (Stx (Cons id_stop stl_args) ctx) Σ*)
   (where (Tup Σ _ _) Σ*)
   (where (TStop _) (lookup-env env (resolve ph id_stop Σ)))]
  
  ;; lambda (unchanged)
  [;  (Stx (List id_lam (Stx (List id_arg) ctx_0) stx_body) ctx)
   (expand ph (Stx (Cons id_lam (Cons (Stx stl_args ctx_0)
                                      (Cons stx_body ()))) ctx) env (Tup Σ scps_p scps_u))
   (values    (Stx (Cons id_lam (Cons (Stx stl_args2 ctx_0)
                                      (Cons stx_body2 ()))) ctx) (Tup Σ_3 scps_p scps_u))
   (where lambda (resolve ph id_lam Σ))
   (where (values scp_new Σ_1) (alloc-scope Σ))
   (where (values stl_args2 env_new Σ_2)
          (regist-vars ph scp_new stl_args env Σ_1))
   (where Σ*_2 (Tup Σ_2 (union (Set scp_new) scps_p) (Set)))
   (where (values stx_body2 (Tup Σ_3 _ _))
          (expand ph (add ph stx_body scp_new) env_new Σ*_2))]

  ;; let
  [(expand ph (Stx (Cons id_let
                         (Cons (Stx stl_binds ctx_1)
                               (Cons stx_body ()))) ctx_2) env (Tup Σ scps_p scps_u))
   (values    (Stx (Cons id_let
                         (Cons (Stx (zip stl_vars2 stl_rhs2 ctx_1) ctx_1)
                               (Cons stx_body2 ()))) ctx_2) (Tup Σ_4 scps_p scps_u))
   (where let (resolve ph id_let Σ))
   (where (values stl_vars stl_rhs) (unzip stl_binds))
   (where (values scp_new Σ_1) (alloc-scope Σ))
   (where (values stl_vars2 env_new Σ_2)
          (regist-vars ph scp_new stl_vars env Σ_1))
   (where Σ*_2 (Tup Σ_2 (union (Set scp_new) scps_p) (Set)))
   (where (values stx_body2 (Tup Σ_3 _ _))
          (expand ph (add ph stx_body scp_new) env_new Σ*_2))
   (where Σ*_3 (Tup Σ_3 scps_p scps_u))
   (where (values stl_rhs2 Σ_4) (expand* ph stl_rhs env Σ*_3))]

  ;; quote (unchanged)
  [;  (Stx (List id_quote stx) ctx)
   (expand ph (Stx (Cons id_quote (Cons stx ())) ctx) env Σ*)
   (values    (Stx (Cons id_quote (Cons stx ())) ctx) Σ*)
   (where (Tup Σ _ _) Σ*)
   (where quote (resolve ph id_quote Σ))]
  
  ;; syntax (unchanged)
  [;  (Stx (List id_syntax stx) ctx)
   (expand ph (Stx (Cons id_syntax (Cons stx ())) ctx) env Σ*)
   (values    (Stx (Cons id_syntax (Cons stx_pruned ())) ctx) Σ*)
   (where (Tup Σ scps_p scps_u) Σ*)
   (where syntax (resolve ph id_syntax Σ))
   (where stx_pruned (prune ph stx scps_p))]

  ;; macro creation (eval gets more and updates store)
  [#; (Stx (List id_ls
                 (Stx (List (Stx (List id stx_rhs) _)) _)
                 stx_body) ctx)
   (expand ph (Stx (Cons id_ls
                         (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ())) _) ()) _)
                               (Cons stx_body ()))) ctx) env (Tup Σ scps_p scps_u))
   (values stx_result (Tup Σ_6 scps_p scps_u))
   (where let-syntax (resolve ph id_ls Σ))
   (where (values nam_new Σ_1) (alloc-name id Σ))
   (where (values scp_new Σ_2) (alloc-scope Σ_1))
   (where id_new (add ph id scp_new))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where (values stx_exp (Tup Σ_4 _ _))
          (expand (plus ph 1) stx_rhs (primitives-env) (Tup Σ_3 (Set) (Set))))
   (where (values val_exp (Tup Σ_5 _ _))
          (eval ph (parse (plus ph 1) stx_exp Σ_4) no-scope env (Tup Σ_4 scps_p (Set))))
   (where env_new (extend-env env nam_new val_exp))
   (where stx_body2 (add ph stx_body scp_new))
   (where (values stx_result (Tup Σ_6 _ _))
          (expand ph stx_body2 env_new (Tup Σ_5 (union (Set scp_new) scps_p) (Set))))]

  ;; macro invocation (eval gets more and updates store)
  [(expand ph stx_macapp env (Tup Σ scps_p scps_u))
   (expand ph (flip ph stx_exp scp_i) env Σ*_3)
   (where (Stx (Cons id_mac stl_args) ctx) stx_macapp)
   (where val (lookup-env env (resolve ph id_mac Σ)))
   (where (values scp_u Σ_1) (alloc-scope Σ))
   (where (values scp_i Σ_2) (alloc-scope Σ_1))
   (where Σ*_2 (Tup Σ_2 (union (Set scp_u) scps_p) (union (Set scp_u) scps_u)))
   (where stx_macapp2 (flip ph (add ph stx_macapp scp_u) scp_i))
   (where (values stx_exp Σ*_3)
          (eval ph (App val stx_macapp2) scp_i env Σ*_2))]
  
  ;; if
  [(expand ph (Stx (Cons id_if (Cons stx_test
                                     (Cons stx_then (Cons stx_else ())))) ctx)
           env (Tup Σ scps_p scps_u))
   (values (Stx (Cons id_if (Cons stx_test2 (Cons stx_then2 (Cons stx_else2 ())))) ctx)
           (Tup Σ_3 scps_p scps_u))
   (where if (resolve ph id_if Σ))
   (where (values stx_test2 (Tup Σ_1 _ _)) (expand ph stx_test env (Tup Σ scps_p (Set))))
   (where (values stx_then2 (Tup Σ_2 _ _)) (expand ph stx_then env (Tup Σ_1 scps_p (Set))))
   (where (values stx_else2 (Tup Σ_3 _ _)) (expand ph stx_else env (Tup Σ_2 scps_p (Set))))]

  ;; application (non-canonical #%app version, unchanged)
  [;  (Stx (List id_app stx_rtor stx_rnd ...) ctx)
   (expand ph (Stx (Cons id_app      (Cons stx_rtor    stl_rnds)) ctx) env (Tup Σ scps_p scps_u))
   (values    (Stx (Cons id_app (Stx (Cons stx_exprtor stl_exprnds) ctx)) ctx)
              (Tup Σ_2 scps_p scps_u))
   (where #%app (resolve ph id_app Σ))
   (where (values stx_exprtor (Tup Σ_1 _ _))
          (expand ph stx_rtor env (Tup Σ scps_p (Set))))
   (where (values stl_exprnds Σ_2)
          (expand* ph stl_rnds env (Tup Σ_1 scps_p (Set))))]

  ;; application (canonical #%app version, unchanged)
  [(expand ph (Stx (Cons id_app (Stx (Cons stx_rtor    stl_rnds)    ctx_1)) ctx)
           env (Tup Σ scps_p scps_u))
   (values    (Stx (Cons id_app (Stx (Cons stx_exprtor stl_exprnds) ctx_1)) ctx)
              (Tup Σ_2 scps_p scps_u))
   (where #%app (resolve ph id_app Σ))
   (where (values stx_exprtor (Tup Σ_1 _ _))
          (expand ph stx_rtor env (Tup Σ scps_p (Set))))
   (where (values stl_exprnds Σ_2)
          (expand* ph stl_rnds env (Tup Σ_1 scps_p (Set))))]

  ;; application (unchanged)
  [;  (Stx (List stx_rtor stx_rnd ...) ctx)
   (expand ph                (Stx (Cons stx_rtor    stl_rnds) ctx) env (Tup Σ scps_p scps_u))
   (values (Stx (Cons id_app (Stx (Cons stx_exprtor stl_exprnds) ctx)) ctx)
           (Tup Σ_2 scps_p scps_u))
   (where id_app (Stx (Sym #%app) ctx))
   (where (values stx_exprtor (Tup Σ_1 _ _))
          (expand ph stx_rtor env (Tup Σ scps_p (Set))))
   (where (values stl_exprnds Σ_2)
          (expand* ph stl_rnds env (Tup Σ_1 scps_p (Set))))]
  
  ;; reference (unchanged)
  [(expand ph id env Σ*)
   (values id_new Σ*)
   (where (Tup Σ _ _) Σ*)
   (where (TVar id_new) (lookup-env env (resolve ph id Σ)))]

  ;; literal (unchanged)
  [(expand ph (Stx atom ctx) env Σ*)
   (values (Stx (Cons (Stx (Sym quote) ctx) (Cons (Stx atom ctx) ())) ctx) Σ*)]
  )

(define-metafunction Lloc
  expand* : ph stl env Σ* -> (values stl Σ)
  [(expand* ph stx            env Σ*)
   (values stx_done Σ)
   (where (values stx_done (Tup Σ _ _)) (expand ph stx env Σ*))]
  [(expand* ph ()             env (Tup Σ _ _)) (values () Σ)]
  [(expand* ph (Cons stx stl) env (Tup Σ scps_p (Set)))
   (values (Cons stx_exp stl_exp) Σ_2)
   (where (values stx_exp (Tup Σ_1 _ _))
          (expand  ph stx env (Tup Σ   scps_p (Set))))
   (where (values stl_exp Σ_2)
          (expand* ph stl env (Tup Σ_1 scps_p (Set))))])

;; ----------------------------------------
;; Drivers

(define-helpers Lloc (Map)
  reader printer)

(define-metafunction Lloc
  stripper : (values stx Σ*) -> val
  [(stripper (values stx Σ*)) (strip stx)])

(define-metafunction Lloc
  expander : stx -> (values stx Σ*)
  [(expander stx) (expand 0 stx (primitives-env) (Tup (init-Σ) (Set) (Set)))])

(define-metafunction Lloc
  parse/values : (values stx Σ*) -> ast
  [(parse/values (values stx (Tup Σ _ _))) (parse 0 stx Σ)])

(define-metafunction Lloc
  evaluate : ast -> val
  [(evaluate ast)
   val
   (where (values val Σ*) (eval 0 ast no-scope (primitives-env) (Tup (init-Σ) (Set) (Set))))])

(define-runner run
  reader
  expander
  stripper printer
  evaluate
  parse/values)

;; ----------------------------------------
;; Examples:

(define ex-local-value
  '[local-value
    (let-syntax ([a '8])
      (let-syntax ([b '9])
        (let-syntax ([x (lambda (stx)
                          (datum->syntax
                           #'here
                           (list #'quote
                                 (datum->syntax
                                  #'here
                                  (syntax-local-value (second (syntax-e stx)))))))])
          (x a))))])
(define (raw-local-value)
  (let-syntax ([a '8])
    (let-syntax ([b '9])
      (let-syntax
          ([x (lambda (stx)
                (syntax-case stx ()
                  [(x id)
                   #`(quote #,(syntax-local-value #'id))]))])
        (x a)))))

(define ex-local-expand
  '[local-expand
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx) 
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (second (syntax-e (cdr (syntax-e (local-expand
                                                          (second (syntax-e stx))
                                                          'expression
                                                          '()))))))])
        (x (q))))])
(define (raw-local-expand)
  (let-syntax ([q (lambda (stx) #'(car 8))])
    (let-syntax ([x (lambda (stx) 
                      ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                      (let ([stx2 (syntax-e (local-expand
                                             (second (syntax-e stx))
                                             'expression
                                             '()))])
                        (second (syntax-e (cdr stx2)))))])
      (x (q)))))


(define ex-local-expand-stop
  '[local-expand-stop
    (let-syntax ([p (lambda (stx) '0)])
      (let-syntax ([q (lambda (stx) #'(car 8))])
        (let-syntax ([x (lambda (stx) 
                          ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                          (second (syntax-e (cdr (syntax-e (local-expand
                                                            (second (syntax-e stx))
                                                            'expression
                                                            (list #'p)))))))])
          (x (q)))))])
(define (raw-local-expand-stop)
  (let-syntax ([p (lambda (stx) '0)])
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx) 
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (let ([stx2 (syntax-e (local-expand
                                               (second (syntax-e stx))
                                               'expression
                                               (list #'p)))])
                          (second (syntax-e (cdr stx2)))))])
        (x (q))))))


(define ex-nested-local-expand
  '[nested-local-expand
    (let-syntax ([z (lambda (stx) #''0)])
      (let-syntax ([a (lambda (stx)
                        ;; When `b' forces `a', then `a'
                        ;; drops `z' form the stop list, so it
                        ;; should expand to 0
                        (local-expand (second (syntax-e stx))
                                      'expression
                                      '()))])
        (let-syntax ([b (lambda (stx)
                          (datum->syntax
                           stx
                           (list
                            #'quote 
                            (local-expand (second (syntax-e stx))
                                          'expression
                                          (list #'z)))))])
          (list (b (z)) (b (a (z)))))))])
(define (raw-nested-local-expand)
  (let-syntax ([z (lambda (stx) #''0)])
    (let-syntax ([a (lambda (stx)
                      ;; When `b' forces `a', then `a'
                      ;; drops `z' form the stop list, so it
                      ;; should expand to 0
                      (local-expand (second (syntax-e stx))
                                    'expression
                                    '()))])
      (let-syntax ([b (lambda (stx)
                        (datum->syntax
                         stx
                         (list
                          #'quote 
                          (local-expand (second (syntax-e stx))
                                        'expression
                                        (list #'z)))))])
        (list (b (z)) (b (a (z))))))))


(define ex-local-binder
  '[local-binder
    (let-syntax ([q (lambda (stx)
                      ;; quotes its argument
                      (datum->syntax
                       stx
                       (list #'quote (second (syntax-e stx)))))])
      (let-syntax ([a (lambda (stx)
                        ;; expands first argument, expected quoted name
                        ;; to use as binder with second arguments body
                        (datum->syntax
                         stx
                         (list
                          #'lambda
                          (datum->syntax
                           stx
                           (list (syntax-local-identifier-as-binding
                                  (second (syntax-e
                                           (local-expand (second (syntax-e stx))
                                                         'expression
                                                         '()))))))
                          (third (syntax-e stx)))))])
        ;; removing the syntax-local-identifier-as-binding call above
        ;; leaves the second `x` as unbound:
        ;; TODO: 実装と不一致．取り除いても↓の実装では unbound にならない
        ((a (q x) x) 'FOOOO)))])
(define (raw-local-binder)
  (let-syntax ([q (lambda (stx)
                    ;; quotes its argument
                    (datum->syntax
                     stx
                     (list #'quote (second (syntax-e stx)))))])
    (let-syntax ([a (lambda (stx)
                      ;; expands first argument, expected quoted name
                      ;; to use as binder with second arguments body
                      (datum->syntax
                       stx
                       (list
                        #'lambda
                        (datum->syntax
                         stx
                         (list (syntax-local-identifier-as-binding
                                (second (syntax-e
                                         (local-expand (second (syntax-e stx))
                                                       'expression
                                                       '()))))))
                        (third (syntax-e stx)))))])
      ;; removing the syntax-local-identifier-as-binding call above
      ;; leaves the second `x` as unbound:
      ;; TODO: 実装と不一致．取り除いても unbound にならない
      (a (q x) x))))

(define local:examples
  (list ex-local-value
        ex-local-expand
        ex-local-expand-stop
        ex-nested-local-expand
        ex-local-binder))


;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           ;pict
           "config.rkt")
  (provide (all-defined-out))
  
  (define eval-pict
    (if narrow-mode?
        ;; Independent form:
        (vl-append
         (metafunction-rule-gap-space)
         (parameterize ([metafunction-cases '(0)])
           (WR (metafunction->pict eval #:contract? #t)))
         (parameterize ([metafunction-cases '(1)])
           (WR (metafunction->pict eval)))
         (parameterize ([metafunction-cases '(2)])
           (WR (metafunction->pict eval))))
        ;; Table form:
        (parameterize ([metafunction-cases '(0 1 2)])
          (WR (metafunction->pict eval #:contract? #t)))))
  
  (define (make-expand-pict pos [contract? #f])
    (parameterize ([metafunction-cases (list pos)])
      (WR (metafunction->pict expand #:contract? contract?))))
  
  (define expand-stop-pict (make-expand-pict 0 #t))
  (define expand-macro-app-pict (make-expand-pict 5))
  (define expand-lambda-pict
    (parameterize ([linebreaks (list #t)])
      (make-expand-pict 1)))

  (define unstop-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict unstop #:contract? #t))))
  
  (define newer-nts '(maybe-scp Σ*))
  (define language-delta-pict
    (WR (language->pict Lloc #:nts newer-nts)))
  
  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Lloc lw))))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view language-delta-pict
        eval-pict
        expand-stop-pict
        expand-lambda-pict
        unstop-pict))

(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-model.rkt" pict) all-nts)
           (only-in (submod "phases-model.rkt" pict)
                    changed-nts
                    new-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Local-Expansion"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lloc #:nts (append newer-nts
                                              new-nts
                                              all-nts))))
     (WR (metafunction->pict eval #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict unstop #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict core:strip #:contract? #t)))
     (parameterize ([linebreaks '(#f #f #t #f #t #f #f #t #f)])
       (WR (metafunction->pict expand #:contract? #t)))
     (parameterize ([linebreaks '(#f #f #t)])
       (WR (metafunction->pict expand* #:contract? #t)))
     (WR (metafunction->pict prune #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
