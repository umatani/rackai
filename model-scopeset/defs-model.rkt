#lang racket
(require redex/reduction-semantics
         "common.rkt"
         (only-in "phases-model.rkt" stl->seq)
         (rename-in (except-in "local-model.rkt"
                               expand expand*
                               eval eval*)
                    [run local:run])
         (for-syntax racket/list))

(define-extended-language Ldef Lloc
  ;; Refines the third component of Σ:
  [boxes ([addr val] ...)]
  ;; Refines the fourth component of Σ:
  [def-envs ([addr env] ...)])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction Ldef
  eval : ph ast maybe-scp env Σ* -> (values val Σ*)

  ;; create definition context
  [(eval ph (App syntax-local-make-definition-context) scp_i env (Tup Σ scps_p scps_u))
   (values (Defs scp_defs addr) Σ*_3)
   (where (values scp_defs Σ_2) (alloc-scope Σ))
   (where (values addr Σ_3) (alloc-def-env Σ_2))
   (where Σ*_3 (Tup (def-env-update Σ_3 addr env) (union (Set scp_defs) scps_p) scps_u))]
  
  ;; create definition binding (for a variable)
  [(eval ph (App syntax-local-bind-syntaxes ast_ids ast_stx ast_defs) scp_i env Σ*)
   (values (Cons id_defs ()) (Tup Σ_7 scps_p4 scps_u4))
   (where (values (Defs scp_defs addr) Σ*_2) (eval ph ast_defs scp_i env Σ*))
   (where (values (Cons id_arg ()) Σ*_3) (eval ph ast_ids scp_i env Σ*_2))
   (where (values #f Σ*_4) (eval ph ast_stx scp_i env Σ*_3))
   (where (Tup Σ_4 scps_p4 scps_u4) Σ*_4)
   (where id_defs (add ph (prune ph (flip ph id_arg scp_i) scps_u4) scp_defs))
   (where (values nam_new Σ_5) (alloc-name id_defs Σ_4))
   (where Σ_6 (bind ph Σ_5 id_defs nam_new))
   (where env_defs (def-env-lookup Σ_6 addr))
   (where Σ_7 (def-env-update Σ_6 addr (extend-env env_defs nam_new (TVar id_defs))))]

  ;; create macro definition binding
  [(eval ph (App syntax-local-bind-syntaxes ast_ids ast_stx ast_defs) scp_i env Σ*)
   (values (Cons id_defs ()) Σ*_9)
   ;(side-condition (printf "bind-syntaxes start: ~a\n" (term ph)))
   (where (values (Defs scp_defs addr) Σ*_2) (eval ph ast_defs scp_i env Σ*))
   (where (values (Cons id_arg ()) Σ*_3) (eval ph ast_ids scp_i env Σ*_2))
   (where (values stx_arg Σ*_4) (eval ph ast_stx scp_i env Σ*_3))
   (where (Tup Σ_4 scps_p4 scps_u4) Σ*_4)
   (where stx_arg2 (add ph (flip ph stx_arg scp_i) scp_defs))
   (where (values stx_exp (Tup Σ_5 _ _))
          (expand (plus ph 1) stx_arg2 (primitives-env) (Tup Σ_4 (Set) (Set))))
   (where (values val_exp Σ*_6)
          (eval ph (parse (plus ph 1) stx_exp Σ_5) no-scope env (Tup Σ_5 scps_p4 (Set))))
   (where (Tup Σ_6 _ _) Σ*_6)
   (where env_defs (def-env-lookup Σ_6 addr))
   (where id_defs (add ph (prune ph (flip ph id_arg scp_i) scps_u4) scp_defs))
   ;(side-condition (printf "bind-syntaxes id_arg: ~a\n" (term id_arg)))
   ;(side-condition (printf "bind-syntaxes id_defs: ~a\n" (term id_defs)))
   (where (values nam_new Σ_7) (alloc-name id_defs Σ_6))
   (where Σ_8 (bind ph Σ_7 id_defs nam_new))
   (where Σ*_9 (Tup (def-env-update Σ_8 addr (extend-env env_defs nam_new val_exp)) scps_p4 scps_u4))]

  ;; local expand with definition context
  ;; - similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [(eval ph (App local-expand ast_expr any_contextv ast_stops ast_defs) scp_i env Σ*)
   (values stx_exp2 Σ*_5)
   ;(side-condition (printf "local-expand2 start: ~a ~a\n" (term ph) (term scp_i)))
   (where (values stx Σ*_2) (eval ph ast_expr scp_i env Σ*))
   (where (values val_idstops Σ*_3) (eval ph ast_stops scp_i env Σ*_2))
   (where (values (Defs scp_defs addr) Σ*_4) (eval ph ast_defs scp_i env Σ*_3))
   ;(side-condition (printf "local-expand2 stx: ~a\n" (term stx)))
   (where (Tup Σ_4 _ _) Σ*_4)
   (where env_defs (def-env-lookup Σ_4 addr))
   (where env_unstops
          ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p))))) (term env_defs)))
   (where (nam_stop ...) (resolve* ph val_idstops Σ_4))
   (where env_stops
          (extend-env* env_unstops ((nam_stop (TStop (lookup-env env_unstops nam_stop))) ...)))
   ; TODO?: 下の(flip ph stx scp_i)は間違い？？しかしdefsを使わない場合にもこれはある．．．
   ;   これがあると，少なくともunit-4が通らない
   ;   しかし，flipしなければdefs-begin-with-defnの挙動が実際の処理系と異なってしまう．
   (where (values stx_exp Σ*_5) (expand ph (add ph (flip ph stx scp_i) scp_defs) env_stops Σ*_4))
   ;(where (values stx_exp Σ*_5) (expand ph (add ph stx scp_defs) env_stops Σ*_4))
   (where stx_exp2 (flip ph stx_exp scp_i))]
  
  ;; local value with definition context
  ;; - similar to the basic local value case, but using definition
  ;;   context's environment
  ;; - Unlike the fourth argument to local-expand, the scopes associated with
  ;;   the provided definition contexts are not used to enrich id-stx’s
  ;;   lexical information.
  [(eval ph (App syntax-local-value ast_id #f ast_defs) scp_i env Σ*)
   (values val Σ*_3)
   ;(side-condition (printf "local-value2 start: ~a\n" (term ph)))
   (where (values id_result Σ*_2) (eval ph ast_id scp_i env Σ*))
   (where (values (Defs scp_defs addr) Σ*_3) (eval ph ast_defs scp_i env Σ*_2))
   (where (Tup Σ_3 _ _) Σ*_3)
   (where env_defs (def-env-lookup Σ_3 addr))
   (where nam (resolve ph id_result Σ_3))
   ;(side-condition (printf "local-value2 id_result: ~a\n" (term id_result)))
   ;(side-condition (printf "local-value2 Σ_3: ~a\n" (term Σ_3)))
   ;(side-condition (printf "local-value2 nam: ~a\n" (term nam)))
   (where val (lookup-env env_defs nam))
   ]


  ;; ----------------------------------------
  ;; Including boxes lets us implement recursive definitions as a
  ;; macro, including variable definitions that are in a recursive
  ;; binding scope with macros.

  ;; box
  [(eval ph (App box ast_val) maybe-scp env Σ*)
   (values addr (Tup (box-update Σ_3 addr val) scps_p2 scps_u2))
   (where (values val (Tup Σ_2 scps_p2 scps_u2)) (eval ph ast_val maybe-scp env Σ*))
   (where (values addr Σ_3) (alloc-box Σ_2))]
  ;; unbox
  [(eval ph (App unbox ast_box) maybe-scp env Σ*)
   (values (box-lookup Σ_2 addr) Σ*_2)
   (where (values addr Σ*_2) (eval ph ast_box maybe-scp env Σ*))
   (where (Tup Σ_2 scps_p2 scps_u2) Σ*_2)]
  ;; set-box!
  [(eval ph (App set-box! ast_box ast_val) maybe-scp env Σ*)
   (values val (Tup (box-update Σ_3 addr val) scps_p3 scps_u3))
   (where (values addr Σ*_2) (eval ph ast_box maybe-scp env Σ*))
   (where (values val Σ*_3) (eval ph ast_val maybe-scp env Σ*_2))
   (where (Tup Σ_3 scps_p3 scps_u3) Σ*_3)]
  
  ;; ----------------------------------------
  ;; The remaining caes are the same as for local-model
  
  ;; local value
  [(eval ph (App syntax-local-value ast_id) scp_i env Σ*)
   (values val Σ*_2)
   ;(side-condition (printf "local-value start: ~a\n" (term ph)))
   (where (values id_result Σ*_2) (eval ph ast_id scp_i env Σ*))
   (where (Tup Σ_2 _ _) Σ*_2)
   (where nam (resolve ph id_result Σ_2))
   ;(side-condition (printf "local-value id_result: ~a\n" (term id_result)))
   ;(side-condition (printf "local-value Σ_2: ~a\n" (term Σ_2)))
   ;(side-condition (printf "local-value nam: ~a\n" (term nam)))
   (where val (lookup-env env nam))]

  ;; local expand
  [(eval ph (App local-expand ast_expr any_contextv ast_stops) scp_i env Σ*)
   (values (flip ph stx_exp scp_i) Σ*_4)
   (where (values stx Σ*_2) (eval ph ast_expr scp_i env Σ*))
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

(define-metafunction Ldef
  eval* : ph (val ...) (ast ...) maybe-scp env Σ* -> (values (val ...) Σ*)
  [(eval* ph (val ...) () maybe-scp env Σ*)
   (values (val ...) Σ*)]
  [(eval* ph (val ...) (ast_0 ast_1 ...) maybe-scp env Σ*)
   (eval* ph (val ... val_0) (ast_1 ...) maybe-scp env Σ*_2)
   (where (values val_0 Σ*_2) (eval ph ast_0 maybe-scp env Σ*))])

(define-metafunction Ldef
  resolve* : ph val Σ -> (nam ...)
  [(resolve* ph () Σ) ()]
  [(resolve* ph (Cons id val) Σ)
   ((resolve ph id Σ) nam ...)
   (where (nam ...) (resolve* ph val Σ))])


;; ----------------------------------------
;; Box allocations and updates:

(define-metafunction Ldef
  alloc-box : Σ -> (values addr Σ)
  [(alloc-box (Sto number (binds ...) boxes def-envs))
   (values ,(string->symbol (format "bx:~a" (term number)))
           (Sto ,(add1 (term number )) (binds ...) boxes def-envs))])

(define-metafunction Ldef
  box-lookup : Σ addr -> val
  [(box-lookup (Sto _ _ (_ ... [addr val] _ ...) _) addr) val])

(define-metafunction Ldef
  box-update : Σ addr val -> Σ
  [(box-update (Sto number (binds ...) (any_1 ... [addr _] any_2 ...) def-envs) addr val)
   (Sto number (binds ...) (any_1 ... [addr val] any_2 ...) def-envs)]
  [(box-update (Sto number (binds ...) (any_1 ...) def-envs) addr val)
   (Sto number (binds ...) ([addr val] any_1 ...) def-envs)])

;; ----------------------------------------
;; Definition-context environment allocations and updates:

(define-metafunction Ldef
  alloc-def-env : Σ -> (values addr Σ)
  [(alloc-def-env (Sto number (binds ...) boxes def-envs))
   (values ,(string->symbol (format "env:~a" (term number)))
           (Sto ,(add1 (term number)) (binds ...) boxes def-envs))])

(define-metafunction Ldef
  def-env-lookup : Σ addr -> env
  [(def-env-lookup (Sto _ _ _ (_ ... [addr env] _ ...)) addr) env])

(define-metafunction Ldef
  def-env-update : Σ addr env -> Σ
  [(def-env-update (Sto number (binds ...) boxes (any_1 ... [addr _] any_2 ...)) addr env)
   (Sto number (binds ...) boxes (any_1 ... [addr env] any_2 ...))]
  [(def-env-update (Sto number (binds ...) boxes (any_1 ...)) addr env)
   (Sto number (binds ...) boxes ([addr env] any_1 ...))])

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define-metafunction Ldef
  parse : ph stx Σ -> ast
  ;; This parse is the same as the multi-phase one, just
  ;; repeated here to access the right `resolve`
  [; (lambda (id ...) stx_body)
   (parse ph (Stx (Cons id_lam (Cons (Stx stl_ids _)
                                     (Cons stx_body ()))) ctx) Σ)
   (Fun ((Var (resolve ph id Σ)) ...) (parse ph stx_body Σ))
   (where lambda (resolve ph id_lam Σ))
   (where (id ...) (stl->seq stl_ids))]
  [; (let ([id stx_rhs] ...) stx_body)
   (parse ph (Stx
              (Cons id_let
                    (Cons (Stx stl_binds ctx_1)
                          (Cons stx_body ()))) ctx_2) Σ)
   (App (Fun ((Var (resolve ph id Σ)) ...) (parse ph stx_body Σ))
        (parse ph stx_rhs Σ) ...)
   (where let (resolve ph id_let Σ))
   (where (values stl_ids stl_rhs) (unzip stl_binds))
   (where (id ...) (stl->seq stl_ids))
   (where (stx_rhs ...) (stl->seq stl_rhs))]
  [; (quote stx)
   (parse ph (Stx (Cons id_quote (Cons stx ())) ctx) Σ)
   (strip stx)
   (where quote (resolve ph id_quote Σ))]
  [; (syntax stx)
   (parse ph (Stx (Cons id_syntax (Cons stx ())) ctx) Σ)
   stx
   (where syntax (resolve ph id_syntax Σ))]
  [; (#%app stx_fun stx_arg ...) トップレベルがstx-pair (cdr部もstx)であることに注意
   (parse ph (Stx (Cons id_app (Stx (Cons stx_rator stl_rands) ctx_1)) ctx_2) Σ)
   (App (parse ph stx_rator Σ) ast_rand ...)
   (where #%app (resolve ph id_app Σ))
   (where (ast_rand ...) (parse* ph stl_rands Σ))]
  [; (if stx stx stx)
   (parse ph (Stx (Cons id_if (Cons stx_test (Cons stx_then (Cons stx_else ())))) ctx) Σ)
   (If (parse ph stx_test Σ) (parse ph stx_then Σ) (parse ph stx_else Σ))
   (where if (resolve ph id_if Σ))]
  [(parse ph id Σ)
   (Var (resolve ph id Σ))]
  [(parse ph (Stx atom ctx) Σ)
   atom])

(define-metafunction Ldef
  parse* : ph stl Σ -> (ast ...)
  [(parse* ph stx Σ) (parse ph stx Σ)]
  [(parse* ph () Σ) ()]
  [(parse* ph (Cons stx stl) Σ)
   ((parse ph stx Σ) ast ...)
   (where (ast ...) (parse* ph stl Σ))])

;; ----------------------------------------
;; The expander:

;; The expander is the same as in local-model, just copied
;; here to use the updated `eval`.

(define-metafunction Ldef
  expand : ph stx env Σ* -> (values stx Σ*)
  
  ;; stops
  [(expand ph (Stx (Cons id_stop stl_args) ctx) env Σ*)
   (values (Stx (Cons id_stop stl_args) ctx) Σ*)
   (where (Tup Σ _ _) Σ*)
   #;
   (side-condition (begin
                     (printf "lookup ~s\n resolves ~a\n" 
                             (term id_stop)
                             (term (resolve ph id_stop Σ)))
                     (pretty-print (term env))
                     (pretty-print (term Σ))))
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
   (where nam_mac (resolve ph id_mac Σ))
   ;(side-condition (printf "macro-inv start: ~a\n" (term ph)))
   ;(side-condition (printf "macro-inv id_mac: ~a\n" (term id_mac)))
   ;(side-condition (printf "macro-inv Σ: ~a\n" (term Σ)))
   ;(side-condition (printf "macro-inv nam_mac: ~a\n" (term nam_mac)))
   (where val (lookup-env env nam_mac))
   (where (values scp_u Σ_1) (alloc-scope Σ))
   (where (values scp_i Σ_2) (alloc-scope Σ_1))
   (where Σ*_2 (Tup Σ_2 (union (Set scp_u) scps_p) (union (Set scp_u) scps_u)))
   (where (values stx_exp Σ*_3)
          (eval ph (App val (flip ph (add ph stx_macapp scp_u) scp_i)) scp_i env Σ*_2))]
  
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

(define-metafunction Ldef
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

(define-helpers Ldef (Map)
  reader printer)

(define-metafunction Ldef
  stripper : (values stx Σ*) -> val
  [(stripper (values stx Σ*)) (strip stx)])

(define-metafunction Ldef
  expander : stx -> (values stx Σ*)
  [(expander stx) (expand 0 stx (primitives-env) (Tup (init-Σ) (Set) (Set)))])

(define-metafunction Ldef
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

(define ex-box
  '[box
    (let-syntax ([m (lambda (stx)
                      (datum->syntax
                       stx
                       (list
                        #'quote
                        (datum->syntax
                         stx
                         (let ([b (box 0)])
                           (unbox b))))))])
      (m))])


(define ex-set-box
  '[set-box
    (let-syntax ([m (lambda (stx)
                      (datum->syntax
                       stx
                       (list
                        #'quote
                        (datum->syntax
                         stx
                         (let ([b (box 0)])
                           (let ([x (set-box! b 1)])
                             (unbox b)))))))])
      (m))])


(define ex-defs-shadow
  '[defs-shadow
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([p (lambda (stx) #'0)])
         (let-syntax ([q (lambda (stx)
                           (let ([defs (syntax-local-make-definition-context)])
                             (let ([ignored (syntax-local-bind-syntaxes
                                             (list (second (syntax-e stx))) #f defs)])
                               (datum->syntax
                                #'here
                                (list
                                 #'lambda
                                 ; not necessary in this case, but sensible
                                 (datum->syntax
                                  #'here
                                  (list (syntax-local-identifier-as-binding 
                                         (second
                                          (syntax-e
                                           (local-expand (datum->syntax
                                                          #'here
                                                          (list #'quote
                                                                (second (syntax-e stx))))
                                                         'expression
                                                         '()
                                                         defs))))))
                                 (local-expand (third (syntax-e stx))
                                               'expression
                                               (list #'call)
                                               defs))))))])
           ((q p (call p)) (lambda () 'FOOOO)))))])
(define (raw-defs-shadow)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
    (let-syntax ([p (lambda (stx) #'0)])
      (let-syntax ([q (lambda (stx)
                        (let ([defs (syntax-local-make-definition-context)])
                          (let ([ignored
                                 (syntax-local-bind-syntaxes
                                  (list (second (syntax-e stx))) #f defs)])
                            (datum->syntax
                             #'here
                             (list
                              #'lambda
                              ; not necessary in this case, but sensible
                              (datum->syntax
                               #'here
                               (list (syntax-local-identifier-as-binding 
                                      (second
                                       (syntax-e
                                        (local-expand (datum->syntax
                                                       #'here
                                                       (list #'quote
                                                             (second (syntax-e stx))))
                                                      'expression
                                                      '()
                                                      defs))))))
                              (local-expand (third (syntax-e stx))
                                            'expression
                                            (list #'call)
                                            defs))))))])
        ((q p (call p)) (lambda () 'FOOOO))))))


;; Like the previous example, but using a macro that expands to `quote`:
(define ex-defs-shadow2
  '[defs-shadow2
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([qt (lambda (stx)
                          (datum->syntax
                           #'here
                           (list #'quote (second (syntax-e stx)))))])
         (let-syntax ([p (lambda (stx) #'0)])
           (let-syntax ([q (lambda (stx)
                             (let ([defs (syntax-local-make-definition-context)])
                               (let ([ignored 
                                      (syntax-local-bind-syntaxes
                                       (list (second (syntax-e stx))) #f defs)])
                                 (datum->syntax
                                  #'here
                                  (list
                                   #'lambda
                                   (datum->syntax
                                    #'here
                                    ; necessary in this case
                                    (list (syntax-local-identifier-as-binding
                                           (second
                                            (syntax-e
                                             (local-expand (datum->syntax
                                                            #'here
                                                            (list #'qt
                                                                  (second (syntax-e stx))))
                                                           'expression
                                                           '()
                                                           defs))))))
                                   (local-expand (third (syntax-e stx))
                                                 'expression
                                                 (list #'call)
                                                 defs))))))])
             ((q p (call p)) (lambda () 'BOOOOOON))))))])
(define (raw-defs-shadow2)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
    (let-syntax ([qt (lambda (stx)
                       (datum->syntax
                        #'here
                        (list #'quote (second (syntax-e stx)))))])
      (let-syntax ([p (lambda (stx) #'0)])
        (let-syntax ([q (lambda (stx)
                          (let ([defs (syntax-local-make-definition-context)])
                            (let ([ignored (syntax-local-bind-syntaxes
                                            (list (second (syntax-e stx)))
                                            #f
                                            defs)])
                              (datum->syntax
                               #'here
                               (list
                                #'lambda
                                (datum->syntax
                                 #'here
                                 ; necessary in this case
                                 (list (syntax-local-identifier-as-binding
                                        (second
                                         (syntax-e
                                          (local-expand (datum->syntax
                                                         #'here
                                                         (list #'qt
                                                               (second (syntax-e stx))))
                                                        'expression
                                                        '()
                                                        defs))))))
                                (local-expand (third (syntax-e stx))
                                              'expression
                                              (list #'call)
                                              defs))))))])
          ((q p (call p)) (lambda () 'BOOOOOON)))))))


(define ex-defs-local-macro
  '[defs-local-macro
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([p (lambda (stx) #'0)])
         (let-syntax ([q (lambda (stx)
                           (let ([defs (syntax-local-make-definition-context) ])
                             (let ([ignored 
                                    (syntax-local-bind-syntaxes
                                     (list (second (syntax-e stx)))
                                     (datum->syntax
                                      #'here
                                      (list #'lambda
                                            (datum->syntax #'here (list #'stx))
                                            (fourth (syntax-e stx))))
                                     defs)])
                               (datum->syntax                             
                                #'here
                                (list
                                 #'lambda
                                 (datum->syntax
                                  #'here
                                  (list
                                   (second
                                    (syntax-e
                                     (local-expand (datum->syntax
                                                    #'here
                                                    (list
                                                     #'quote
                                                     (second (syntax-e stx))))
                                                   'expression
                                                   '()
                                                   defs)))))
                                 (local-expand (third (syntax-e stx))
                                               'expression
                                               '()
                                               defs))))))])
           ((q p (call p) #'13) '0))))])
(define (raw-defs-local-macro)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
    (let-syntax ([p (lambda (stx) #'0)])
      (let-syntax ([q (lambda (stx)
                        (let ([defs (syntax-local-make-definition-context)])
                          (let ([ignored 
                                 (syntax-local-bind-syntaxes
                                  (list (second (syntax-e stx)))
                                  (datum->syntax
                                   #'here
                                   (list #'lambda
                                         (datum->syntax #'here (list #'stx))
                                         (fourth (syntax-e stx))))
                                  defs)])
                            (datum->syntax                             
                             #'here
                             (list
                              #'lambda
                              (datum->syntax
                               #'here
                               (list
                                (second
                                 (syntax-e
                                  (local-expand (datum->syntax
                                                 #'here
                                                 (list
                                                  #'quote
                                                  (second (syntax-e stx))))
                                                'expression
                                                '()
                                                defs)))))
                              (local-expand (third (syntax-e stx))
                                            'expression
                                            '()
                                            defs))))))])
        ((q p (call p) #'13) 0)))))

(define ex-defs-begin-with-defn
  '[defs-begin-with-defn
     (let-syntax ([bwd (lambda (stx)
                         (let ([;; create ctx
                                ctx (syntax-local-make-definition-context)]
                               [; the x in (define x '10)
                                id1 (second (syntax-e (second (syntax-e stx))))]
                               [; the 10 in (define x '10)
                                e1 (third (syntax-e (second (syntax-e stx))))]
                               [; the q in (define-syntax q (lambda (v) ...))
                                id2 (second (syntax-e (third (syntax-e stx))))]
                               [; the (lambda (v) ...) in (define-syntax q (lambda (v) ...))
                                e2 (third (syntax-e (third (syntax-e stx))))]
                               [; the last body expression, expands to (lambda (i) x)
                                e3 (fourth (syntax-e stx))])
                           (let ([; for side-effect of binding x in ctx
                                  ;; bind id1 (i.e., x)
                                  ignored (syntax-local-bind-syntaxes
                                           (list id1) #f ctx) ]
                                 [; for side-effect of binding q in ctx
                                  ;; bind id2 (i.e., q)
                                  ignored2 (syntax-local-bind-syntaxes
                                            (list id2) e2 ctx)]
                                 [; local-expand e3
                                  ;; local-expand e3 (i.e., the body expression):
                                  ee3 (local-expand e3
                                                    'expression
                                                    (list #'lambda)
                                                    ctx)]
                                 [; local-expand id1 (in a syntax form)
                                  ;; local-expand of id1 (to give it context from ctx):
                                  qid1 (local-expand (datum->syntax
                                                      #'here
                                                      (list #'quote
                                                            id1))
                                                     'expression
                                                     (list #'quote)
                                                     ctx)])
                             (let ([; extract expanded id1 from qid1
                                    eid1 (second (syntax-e qid1))])
                               ;; generate ((lambda (eid1) ee3) '10):
                               (datum->syntax
                                #'here
                                (list
                                 (datum->syntax
                                  #'here
                                  (list #'lambda
                                        (datum->syntax
                                         #'here
                                         (list eid1))
                                        ee3))
                                 e1))))))])
       ;; `bwd' is short for `begin-with-definitions', which
       ;; assumes a `define' followed by a `define-syntax' followed
       ;; by a body form
       ((bwd (define x '10)
             (define-syntax q (lambda (v) (syntax (lambda (i) x))))
             #;(lambda i x)
             (q)) 0))])

(define (raw-defs-begin-with-defn)
  (let-syntax
      ([bwd (lambda (stx)
              (syntax-case stx (define define-syntax)
                [(bwd (define id1 e1)
                      (define-syntax id2 e2)
                      e3)
                 (let* ([;; create ctx
                         ctx (syntax-local-make-definition-context)]
                        [; for side-effect of binding x in ctx
                         ;; bind id1 (i.e., x)
                         ignored (syntax-local-bind-syntaxes (list #'id1) #f ctx)]
                        [; for side-effect of binding q in ctx
                         ;; bind id2 (i.e., q)
                         ignored (syntax-local-bind-syntaxes (list #'id2) #'e2 ctx)]
                        [;; local-expand e3 (i.e., the body expression)
                         ee3 (local-expand #'e3
                                           'expression
                                           (list #'lambda)
                                           ctx)]
                        [; local-expand id1 (in a syntax form)
                         ;; local-expand of id1 (to give it context from ctx):
                         qid1 (local-expand (datum->syntax
                                             #'here
                                             (list #'quote
                                                   #'id1))
                                            'expression
                                            (list #'quote)
                                            ctx)]
                        [; extract expanded id1 from qid1
                         eid1 (second (syntax-e qid1))])
                   ;; generate ((lambda (eid1) ee3) '10):
                   (datum->syntax
                    #'here
                    (list
                     (datum->syntax
                      #'here
                      (list #'lambda
                            (datum->syntax
                             #'here
                             (list eid1))
                            ee3))
                     #'e1)))]))])
    ;; `bwd' is short for `begin-with-definitions', which
    ;; assumes a `define' followed by a `define-syntax' followed
    ;; by a body form
    ((bwd (define x '10)
          (define-syntax q (lambda (v) (syntax (lambda (i) x))))
          #;(lambda i x)
          (q)) 0)))   ;; TODO: ((bwd ...) 10) ではなく ((q) 0) とするとなぜか
                      ;; q が out-of-contextとエラーになる．スコープ関係？

(define defs:examples
  (list ex-box
        ex-set-box
        ex-defs-shadow
        ex-defs-shadow2
        ex-defs-local-macro
        ex-defs-begin-with-defn
        ))

(define main
  (let ([all-runs `([core ,core:run]
                    [phases ,phases:run]
                    [local ,local:run]
                    [defs ,run])]
        [all-examples (list core:examples
                            phases:examples
                            local:examples
                            defs:examples)])
    (run-all-examples all-runs all-examples)))


;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           ;pict
           "config.rkt")
  (provide (all-defined-out))
  
  (define (make-eval-pict pos)
    (parameterize ([metafunction-cases (list pos)])
      (WR (metafunction->pict eval))))
  
  (define eval-new-defs-pict (make-eval-pict 0))
  (define eval-def-bind-var-pict (make-eval-pict 1))
  (define eval-def-bind-syntax-pict (make-eval-pict 2))
  (define eval-local-expand-pict (make-eval-pict 3))

  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Ldef lw))))


(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view eval-new-defs-pict
        eval-def-bind-var-pict
        eval-def-bind-syntax-pict
        eval-local-expand-pict))


(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-model.rkt" pict) all-nts)
           (only-in (submod "phases-model.rkt" pict)
                    changed-nts
                    new-nts)
           (only-in (submod "local-model.rkt" pict)
                    newer-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Definition-Contexts"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lloc #:nts (append newer-nts
                                              new-nts
                                              all-nts))))
     (parameterize ([metafunction-cases (for/list ([n (in-range 0 10)]
                                                   #:unless (< 3 n 7))
                                          n)])
       (WR (metafunction->pict eval #:contract? #t)))
     (parameterize ([metafunction-cases (for/list ([n (in-range 10 13)])
                                          n)])
       (WR (metafunction->pict eval)))
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
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict prune #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))


;;;; unit tests

(define (unit-0)
 (run '(let-syntax
           ([f (lambda (stx)
                 (let ([defs (syntax-local-make-definition-context)])
                   (let ([id (syntax-local-bind-syntaxes
                              (list #'id) #f defs)])
                     #'(+ 1 2))))])
         (f)) 'eval))


;; これは本物の処理系でもダメ
(define (unit-1)
  (run '(let-syntax
            ([f (lambda (stx)
                  (syntax-case stx ()
                    [(f x)
                     (let ([defs (syntax-local-make-definition-context)])
                       (let ([ignored (syntax-local-bind-syntaxes
                                       (list #'x) #'#''id defs)])
                         (syntax-local-value #'x #f defs)))]))])
          (f a)) 'eval))

(define (unit-2)
  (run '(let-syntax
            ([m (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([ids (syntax-local-bind-syntaxes
                                (list #'f) #'#''id defs)])
                      (syntax-local-value (car ids) #f defs))))])
          (m)) 'eval))

(define (unit-3)
  (run '(let-syntax
            ([m (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([ignored (syntax-local-bind-syntaxes
                                    (list (second (syntax-e stx)))
                                    #'(lambda (stx) #'(+ 1 2))
                                    defs)])
                      (local-expand (datum->syntax
                                     #'here
                                     (list (second (syntax-e stx))))
                                    'expression '() defs))))])
          (m f)) 'eval))

(define (unit-4)
  (run '(let-syntax
            ([m (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([ids (syntax-local-bind-syntaxes
                                (list #'f)
                                #'(lambda (stx) #'(+ 1 2))
                                defs)])
                      (local-expand (datum->syntax
                                     #'here
                                     (list (car ids)))
                                    'expression '() defs))))])
          (m)) 'eval))
