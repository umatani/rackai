#lang racket
(require redex/reduction-semantics
         "common.rkt"
         slideshow/pict
         (for-syntax racket/list))

(provide L stl->seq unzip zip
         eval expand
         subst substs
         plus minus
         δ/stx δ
         addremove
         flip add strip
         subtract union
         bind resolve
         lookup-Σ binding-lookup biggest-subset
         parse lookup-env extend-env
         alloc-name alloc-scope
         primitives-env init-Σ
         run core:examples)


(define-language L
  
  ;; Executable AST and values:
  [ast val
       var
       (App ast ast ...)
       (If ast ast ast)]
  [var (Var nam)]
  [val (Fun (var ...) ast) atom (Cons val val) stx]

  ;; Syntax objects (a subset of values):
  [stx (Stx atom ctx) (Stx (Cons stx stl) ctx)]
  [id (Stx sym ctx)]
  [ctx scps]
  [scps (Set scp ...)]
  [stl stx () (Cons stx stl)] ;; syntax tail
  
  ;; Literal values:
  [atom () sym prim desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number
                   boolean
                   ;; Not used until definition-context model:
                   addr (Defs scp addr)]
  [sym (Sym nam)]
  [prim syntax-e datum->syntax desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim + - * / < = eq?
                   cons car cdr list second third fourth
                   ;; Not implemented at first, but it's simplest
                   ;; to include these in the grammar from the start:
                   syntax-local-value local-expand
                   syntax-local-identifier-as-binding
                   box unbox set-box!
                   syntax-local-make-definition-context
                   syntax-local-bind-syntaxes]

  ;; Expand-time environment:
  [env desc-env] ; `desc-env' typesets as prose
  [desc-env ((nam all-transform) ...)]
  [transform (TVar id) val ; lambda let let-syntax quote syntax
             ]
  ;; The `TStop' transform type is not used at first:
  [all-transform transform (TStop all-transform) not-found]

  ;; Expand-time store:
  [Σ desc-store] ; `desc-store' typesets as prose
  [desc-store (Sto number      ; for alloc
                   (binds ...) ; binding store
                   boxes       ; for later model
                   def-envs)]  ; for later model
  [binds [nam (StoBind scps nam) ...]]
  
  ;; Use names for vars, addrs, and scopes
  [nam desc-name] ; `desc-name' typesets as prose
  [desc-name variable-not-otherwise-mentioned
             ;lambda let let-syntax quote syntax
             ]
  [scp desc-scope] ; `desc-scope' typesets as prose
  [desc-scope nam]
  
  [boxes any] ; refined in a later model
  [def-envs any]  ; refined in a later model
  [addr nam]  ; used in a later model
  
  [nam-or-false nam #f])


;; ----------------------------------------
;; Utils

(define-metafunction L
  stl->seq : stl -> (stx ...)
  [(stl->seq ()) ()]
  [(stl->seq (Cons stx_0 stl))
   (stx_0 stx ...)
   (where (stx ...) (stl->seq stl))])

(define-metafunction L
  unzip : stl -> (values stl stl)
  [(unzip ()) (values () ())]
  [(unzip (Cons (Stx (Cons stx_left (Cons stx_right ())) ctx)
                stl_rest))
   (values (Cons stx_left stl_lefts) (Cons stx_right stl_rights))
   (where (values stl_lefts stl_rights) (unzip stl_rest))])

(define-metafunction L
  zip : stl stl ctx -> stl
  [(zip () () ctx) ()]
  [(zip (Cons stx_left stl_lefts) (Cons stx_right stl_rights) ctx)
   (Cons (Stx (Cons stx_left (Cons stx_right ())) ctx)
         (zip stl_lefts stl_rights ctx))])


;; ----------------------------------------
;; Non-capturing substitution for AST:

(define-metafunction L
  subst : ast var ast -> ast
  [(subst var var ast_v) ast_v]
  [(subst var_2 var ast_v) var_2]
  [(subst (App ast ...) var ast_v)
   (App (subst ast var ast_v) ...)]
  [(subst (If ast_1 ast_2 ast_3) var ast_v)
   (If (subst ast_1 var ast_v) (subst ast_2 var ast_v) (subst ast_3 var ast_v))]
  [(subst (Fun (var_0 ... var var_1 ...) ast) var ast_v)
   (Fun (var_0 ... var var_1 ...) ast)]
  [(subst (Fun (var_2 ...) ast) var ast_v)
   (Fun (var_3 ...) (subst (substs ast (var_2 ...) (var_3 ...))
                           var ast_v))
   (where ((Var nam_2) ...) (var_2 ...))
   (where (nam_3 ...) ,(variables-not-in (term ast_v) (term (nam_2 ...))))
   (where (var_3 ...) ((Var nam_3) ...))]
  [(subst atom var ast_v) atom]
  [(subst (Cons val_1 val_2) var ast_v) 
   (Cons (subst val_1 var ast_v) val_2)]
  [(subst stx var ast_v) stx])

(define-metafunction L
  substs : ast (var ...) (ast ...) -> ast
  [(substs ast () ()) ast]
  [(substs ast (var_0 var_1 ...) (ast_0 ast_1 ...))
   (subst (substs ast (var_1 ...) (ast_1 ...)) var_0 ast_0)])


;; ----------------------------------------
;; Implementation of primitives:

(define-metafunction L
  [(plus number ...) ,(apply + (term (number ...)))])
(define-metafunction L
  [(minus number ...) ,(apply - (term (number ...)))])
(define-metafunction L
  [(times number ...) ,(apply * (term (number ...)))])
(define-metafunction L
  [(div number ...) ,(apply / (term (number ...)))])
(define-metafunction L
  [(less-than number ...) ,(apply < (term (number ...)))])
(define-metafunction L
  [(num-eq number ...) ,(apply = (term (number ...)))])
(define-metafunction L
  [(sym-eq (Sym nam_1) (Sym nam_2)) ,(eq? (term nam_1) (term nam_2))])

(define-metafunction L
  δ/stx : prim (val ...) -> val
  [(δ/stx syntax-e ((Stx atom ctx))) atom]
  [(δ/stx syntax-e ((Stx (Cons stx stl) ctx))) (Cons stx stl)]

  [(δ/stx datum->syntax ((Stx val_0 ctx) stx)) stx]
  [(δ/stx datum->syntax ((Stx val_0 ctx) atom)) (Stx atom ctx)]
  [(δ/stx datum->syntax ((Stx val_0 ctx) (Cons val_1 val_2)))
   (Stx (Cons (δ/stx datum->syntax ((Stx val_0 ctx) val_1)) stl) ctx)
   (where (Stx stl ctx_2) (δ/stx datum->syntax ((Stx val_0 ctx) val_2)))])

(define-metafunction/extension δ/stx L
  δ : prim (val ...) -> val
  [(δ + (number ...)) (plus  number ...)]
  [(δ - (number ...)) (minus number ...)]
  [(δ * (number ...)) (times number ...)]
  [(δ / (number ...)) (div   number ...)]
  [(δ < (number ...)) (less-than number ...)]
  [(δ = (number ...)) (num-eq number ...)]
  [(δ eq? (sym_1 sym_2)) (sym-eq sym_1 sym_2)]

  [(δ cons (val_1 val_2)) (Cons val_1 val_2)]
  [(δ car ((Cons val_1 val_2))) val_1]
  [(δ cdr ((Cons val_1 val_2))) val_2]

  [(δ list ()) ()]
  [(δ list (val_1 val_2 ...)) (δ cons (val_1 (δ list (val_2 ...))))]
  [(δ second ((Cons val_1 (Cons val_2 val_3)))) val_2]
  [(δ third  ((Cons val_1 (Cons val_2 (Cons val_3 val_4))))) val_3]
  [(δ fourth ((Cons val_1 (Cons val_2 (Cons val_3 (Cons val_4 val_5)))))) val_4]
  )

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction L
  eval : ast -> val
  [(eval (App ast_fun ast_arg ...))
   (eval (substs ast_body (var ...) (eval* () (ast_arg ...))))
   (where (Fun (var ...) ast_body) (eval ast_fun))]
  [(eval (App prim ast_arg ...))
   (δ prim ((eval ast_arg) ...))]
  [(eval (If ast_test ast_then ast_else))
   (eval ast_else)
   (where #f (eval ast_test))]
  [(eval (If ast_test ast_then ast_else))
   (eval ast_then)]
  [(eval val) val])

(define-metafunction L
  eval* : (val ...) (ast ...) -> (val ...)
  [(eval* (val ...) ()) (val ...)]
  [(eval* (val ...) (ast_0 ast_1 ...))
   (eval* (val ... val_0) (ast_1 ...))
   (where val_0 (eval ast_0))])

;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction L
  add : stl scp -> stl
  ;; Simply pushes scopes down through a syntax object
  [(add (Stx atom ctx) scp)
   (Stx atom (union (Set scp) ctx))]
  [(add (Stx (Cons stx stl) ctx) scp)
   (Stx (Cons (add stx scp) (add stl scp)) (union (Set scp) ctx))]
  [(add () scp) ()]
  [(add (Cons stx stl) scp) (Cons (add stx scp) (add stl scp))])

(define-metafunction L
  ;; Adds or cancels a scope
  addremove : scp scps -> scps
  [(addremove scp_2 (Set scp_1 ... scp_2 scp_3 ...)) (Set scp_1 ... scp_3 ...)]
  [(addremove scp_1 (Set scp_2 ...)) (Set scp_1 scp_2 ...)])

(define-metafunction L
  flip : stl scp -> stl
  ;; Pushes flipping a scope down through a syntax object
  [(flip (Stx atom ctx) scp) 
   (Stx atom (addremove scp ctx))]
  [(flip (Stx (Cons stx stl) ctx) scp) 
   (Stx (Cons (flip stx scp) (flip stl scp)) (addremove scp ctx))]
  [(flip () scp) ()]
  [(flip (Cons stx stl) scp) (Cons (flip stx scp) (flip stl scp))])

(define-metafunction L
  strip : stl -> val
  ;; Recursively strips lexical context from a syntax object
  [(strip (Stx atom ctx))
   atom]
  [(strip (Stx (Cons stx stl) ctx)) 
   (Cons (strip stx) (strip stl))]
  [(strip ()) ()]
  [(strip (Cons stx stl)) (Cons (strip stx) (strip stl))])

(define-metafunction L
  subtract : scps scps -> scps
  [(subtract scps (Set)) scps]
  [(subtract (Set scp_1 ... scp scp_2 ...) (Set scp scp_3 ...))
   (subtract (Set scp_1 ... scp_2 ...) (Set scp scp_3 ...))]
  [(subtract scps (Set scp scp_1 ...))
   (subtract scps (Set scp_1 ...))])

(define-metafunction L
  union : scps scps -> scps
  [(union (Set scp_1 ...) (Set scp_2 ...)) (Set scp_1 ... scp_2 ...)])

(define-metafunction L
  bind : Σ id nam -> Σ
  ;; Add a binding using the name and scopes of an identifier, mapping
  ;; them in the store to a given name
  [(bind (Sto number
              (binds_1 ... [nam_1 (StoBind ctx_2 nam_2) ...] binds_2 ...)
              boxes
              def-envs)
         (Stx (Sym nam_1) ctx_1)
         nam_3)
   (Sto number
        (binds_1 ... [nam_1 (StoBind ctx_1 nam_3) (StoBind ctx_2 nam_2) ...] binds_2 ...)
        boxes
        def-envs)]
  [(bind (Sto number (binds ...) boxes def-envs)
         (Stx (Sym nam_1) ctx_1)
         nam_3)
   (Sto number
        ([nam_1 (StoBind ctx_1 nam_3)] binds ...)
        boxes
        def-envs)])

(define-metafunction L
  resolve : id Σ -> nam
  [(resolve (Stx (Sym nam) ctx) Σ)
   nam_biggest
   (where (Set (StoBind scps_bind nam_bind) ...) (lookup-Σ Σ nam))
   (where scps_biggest (biggest-subset ctx (Set scps_bind ...)))
   (where nam_biggest (binding-lookup (Set (StoBind scps_bind nam_bind) ...) scps_biggest))]
  [(resolve (Stx (Sym nam) ctx) Σ)
   nam])

(define-metafunction L
  lookup-Σ : Σ nam -> (Set (StoBind scps nam) ...)
  [(lookup-Σ (Sto number (_ ... [nam (StoBind scps_bind nam_bind) ...] _ ...) _ _) nam)
   (Set (StoBind scps_bind nam_bind) ...)]
  [(lookup-Σ Σ nam) (Set)])

(define-metafunction L
  binding-lookup : (Set (StoBind scps nam) ...) scps -> nam-or-false
  [(binding-lookup (Set _ ... (StoBind scps nam) _ ...) scps) nam]
  [(binding-lookup _ scps) #f])

(define-metafunction L
  biggest-subset : scps (Set scps ...) -> scps
  [(biggest-subset scps_ref (Set scps_bind ...))
   scps_biggest
   (where scps_biggest
          ;; The biggest-subset search seems easiest to write in Racket:
          ,(let* ([matching
                   (filter (lambda (scps_bind)
                             (subset? scps_bind (term scps_ref)))
                           (term (scps_bind ...)))]
                  [sorted
                   (sort matching
                         (lambda (a b)
                           (> (length a) (length b))))])
             ;; The binding is ambigious if the first scps in
             ;; `sorted` is not bigger than the others, or if
             ;; some scps in `sorted` is not a subset of the
             ;; first one.
             (if (or (empty? sorted)
                     (and (pair? (rest sorted))
                          (= (length (first sorted))
                             (length (second sorted))))
                     (ormap (lambda (b)
                              (not (subset? b (first sorted))))
                            (rest sorted)))
                 #f
                 (first sorted))))]
  [(biggest-subset _ _) (Set)])

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

(define-metafunction L
  parse : stx Σ -> ast
  [; (lambda (id ...) stx_body)
   (parse (Stx (Cons id_lam (Cons (Stx stl_ids _) (Cons stx_body ()))) ctx) Σ)
   (Fun ((Var (resolve id Σ)) ...) (parse stx_body Σ))
   (where lambda (resolve id_lam Σ))
   (where (id ...) (stl->seq stl_ids))]
  [; (let ([id stx_rhs] ...) stx_body)
   (parse (Stx
           (Cons id_let
                 (Cons (Stx stl_binds ctx_1)
                       (Cons stx_body ()))) ctx_2) Σ)
   (App (Fun ((Var (resolve id Σ)) ...) (parse stx_body Σ))
        (parse stx_rhs Σ) ...)
   (where let (resolve id_let Σ))
   (where (values stl_ids stl_rhs) (unzip stl_binds))
   (where (id ...) (stl->seq stl_ids))
   (where (stx_rhs ...) (stl->seq stl_rhs))]
  [; (quote stx)
   (parse (Stx (Cons id_quote (Cons stx ())) ctx) Σ)
   (strip stx)
   (where quote (resolve id_quote Σ))]
  [; (syntax stx)
   (parse (Stx (Cons id_syntax (Cons stx ())) ctx) Σ)
   stx
   (where syntax (resolve id_syntax Σ))]
  [; (#%app stx_fun stx_arg ...) トップレベルがstx-pair (cdr部もstx)であることに注意
   (parse (Stx (Cons id_app (Stx (Cons stx_fun stl_args) ctx_1)) ctx_2) Σ)
   (App (parse stx_fun Σ) ast_arg ...)
   (where #%app (resolve id_app Σ))
   (where (ast_arg ...) (parse* stl_args Σ))]
  [; (if stx stx stx)
   (parse (Stx (Cons id_if (Cons stx_test (Cons stx_then (Cons stx_else ())))) ctx) Σ)
   (If (parse stx_test Σ) (parse stx_then Σ) (parse stx_else Σ))
   (where if (resolve id_if Σ))]
  [; reference
   (parse id Σ)
   (Var (resolve id Σ))]
  [; literal
   (parse (Stx atom ctx) Σ)
   atom]
  )

(define-metafunction L
  parse* : stl Σ -> (ast ...)
  [(parse* stx Σ) (parse stx Σ)]
  [(parse* () Σ) ()]
  [(parse* (Cons stx stl) Σ)
   ((parse stx Σ) ast ...)
   (where (ast ...) (parse* stl Σ))])

;; ----------------------------------------
;; Expand-time environment operations:

(define-metafunction L
  lookup-env : env nam -> all-transform
  [(lookup-env ((nam all-transform) any_2 ...) nam) all-transform]
  [(lookup-env (any_1 any_2 ...) nam) (lookup-env (any_2 ...) nam)]
  [(lookup-env () nam) not-found #;nam])

(define-metafunction L
  extend-env : env nam all-transform -> env
  [(extend-env env nam all-transform) ((nam all-transform) . env)])

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

(define-metafunction L
  alloc-name : id Σ -> (values nam Σ)
  [(alloc-name (Stx (Sym nam) ctx) (Sto number (binds ...) boxes def-envs))
   (values ,(string->symbol (format "~a:~a" (term nam) (term number)))
           (Sto ,(add1 (term number )) (binds ...) boxes def-envs))])

(define-metafunction L
  alloc-scope : Σ -> (values scp Σ)
  [(alloc-scope (Sto number any boxes def-envs))
   (values ,(string->symbol (format "scp:~a" (term number)))
           (Sto ,(add1 (term number)) any boxes def-envs))])

(define-metafunction L
  regist-vars : scp stl env Σ -> (values stl env Σ)
  [(regist-vars scp () env Σ) (values () env Σ)]
  [(regist-vars scp (Cons id stl) env Σ)
   (values (Cons id_new stl_reg) env_2 Σ_3)
   (where (values stl_reg env_1 Σ_1) (regist-vars scp stl env Σ))
   (where (values nam_new Σ_2) (alloc-name id Σ_1))
   (where id_new (add id scp))
   (where Σ_3 (bind Σ_2 id_new nam_new))
   (where env_2 (extend-env env_1 nam_new (TVar id_new)))])

;; ----------------------------------------
;; The expander:

(define-metafunction L
  expand : stx env Σ -> (values stx Σ)

  ;; lambda
  [;  (Stx (List id_lam (Stx (List id_arg) ctx_0) stx_body) ctx)
   ;  
   (expand (Stx (Cons id_lam (Cons (Stx stl_args ctx_0)
                                   (Cons stx_body ()))) ctx) env Σ)
   (values (Stx (Cons id_lam (Cons (Stx stl_args2 ctx_0)
                                   (Cons stx_body2 ()))) ctx) Σ_3)
   (where lambda (resolve id_lam Σ))
   (where (values scp_new Σ_1) (alloc-scope Σ))
   (where (values stl_args2 env_new Σ_2)
          (regist-vars scp_new stl_args env Σ_1))
   (where (values stx_body2 Σ_3) (expand (add stx_body scp_new) env_new Σ_2))]

  ;; let
  [(expand (Stx (Cons id_let
                      (Cons (Stx stl_binds ctx_1)
                            (Cons stx_body ()))) ctx_2) env Σ)
   (values (Stx (Cons id_let 
                      (Cons (Stx (zip stl_vars2 stl_rhs2 ctx_1) ctx_1)
                            (Cons stx_body2 ()))) ctx_2) Σ_4)
   (where let (resolve id_let Σ))
   (where (values stl_vars stl_rhs) (unzip stl_binds))
   (where (values scp_new Σ_1) (alloc-scope Σ))
   (where (values stl_vars2 env_new Σ_2)
          (regist-vars scp_new stl_vars env Σ_1))
   (where (values stx_body2 Σ_3) (expand (add stx_body scp_new) env_new Σ_2))
   (where (values stl_rhs2 Σ_4) (expand* stl_rhs env Σ_3))]

  ;; quote
  [;  (Stx (List id_quote stx) ctx)
   (expand (Stx (Cons id_quote (Cons stx ())) ctx) env Σ)
   (values (Stx (Cons id_quote (Cons stx ())) ctx) Σ)
   (where quote (resolve id_quote Σ))]
  
  ;; syntax
  [;  (Stx (List id_syntax stx) ctx)
   (expand (Stx (Cons id_syntax (Cons stx ())) ctx) env Σ)
   (values (Stx (Cons id_syntax (Cons stx ())) ctx) Σ)
   (where syntax (resolve id_syntax Σ))]

  ;; macro creation
  [#; (expand (Stx (List id_ls
                      (Stx (List (Stx (List id stx_rhs) _)) _)
                      stx_b) ctx) env Σ)
   (expand (Stx (Cons id_ls
                      (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ())) _) ()) _)
                            (Cons stx_body ()))) ctx) env Σ)
   (expand stx_body2 env_2 Σ_3)
   (where let-syntax (resolve id_ls Σ))
   (where (values nam_new Σ_1) (alloc-name id Σ))
   (where (values scp_new Σ_2) (alloc-scope Σ_1))
   (where id_new (add id scp_new))
   (where Σ_3 (bind Σ_2 id_new nam_new))
   (where (values stx_exp Σ_4) (expand stx_rhs (primitives-env) Σ_3))
   (where env_2 (extend-env env nam_new (eval (parse stx_exp Σ_3))))
   (where stx_body2 (add stx_body scp_new))]

  ;; macro invocation
  [(expand stx_macapp env Σ)
   (expand (flip stx_exp scp_i) env Σ_2)
   (where (Stx (Cons id_mac stl_args) ctx) stx_macapp)
   (where val (lookup-env env (resolve id_mac Σ)))
   (where (values scp_u Σ_1) (alloc-scope Σ))
   (where (values scp_i Σ_2) (alloc-scope Σ_1))
   (where stx_exp (eval (App val (flip (add stx_macapp scp_u) scp_i))))]
  
  ;; if
  [(expand (Stx (Cons id_if (Cons stx_test (Cons stx_then (Cons stx_else ())))) ctx) env Σ)
   (values (Stx (Cons id_if (Cons stx_test2 (Cons stx_then2 (Cons stx_else2 ())))) ctx) Σ_3)
   (where if (resolve id_if Σ))
   (where (values stx_test2 Σ_1) (expand stx_test env Σ))
   (where (values stx_then2 Σ_2) (expand stx_then env Σ_1))
   (where (values stx_else2 Σ_3) (expand stx_else env Σ_2))]
  
  ;; application (non-canonical #%app version)
  [;  (expand (Stx (List id_app stx_fun stx_arg ...) ctx) env Σ)
   (expand (Stx (Cons id_app      (Cons stx_fun stl_args))  ctx) env Σ)
   (values (Stx (Cons id_app (Stx (Cons stx_fun2 stl_args2) ctx)) ctx) Σ_2)
   (where #%app (resolve id_app Σ))
   (where (values stx_fun2 Σ_1) (expand stx_fun env Σ))
   (where (values stl_args2 Σ_2) (expand* stl_args env Σ_1))]

  ;; application (canonical #%app version)
  [(expand (Stx (Cons id_app (Stx (Cons stx_fun  stl_args)  ctx_1)) ctx) env Σ)
   (values (Stx (Cons id_app (Stx (Cons stx_fun2 stl_args2) ctx_1)) ctx) Σ_2)
   (where #%app (resolve id_app Σ))
   (where (values stx_fun2 Σ_1) (expand stx_fun env Σ))
   (where (values stl_args2 Σ_2) (expand* stl_args env Σ_1))]

  ;; application
  [;  (expand (Stx (List stx_fun stx_arg ...) ctx) env Σ)
   (expand                   (Stx (Cons stx_fun  stl_args)  ctx) env Σ)
   (values (Stx (Cons id_app (Stx (Cons stx_fun2 stl_args2) ctx)) ctx) Σ_2)
   (where id_app (Stx (Sym #%app) ctx))
   (where (values stx_fun2 Σ_1) (expand stx_fun env Σ))
   (where (values stl_args2 Σ_2) (expand* stl_args env Σ_1))]

  ;; reference
  [(expand id env Σ)
   (values id_new Σ)
   (where (TVar id_new) (lookup-env env (resolve id Σ)))]

  ;; literal
  [(expand (Stx atom ctx) env Σ)
   (values (Stx (Cons (Stx (Sym quote) ctx) (Cons (Stx atom ctx) ())) ctx) Σ)]
  )

(define-metafunction L
  expand* : stl env Σ -> (values stl Σ)
  [(expand* stx env Σ) (expand stx env Σ)]
  [(expand* () env Σ) (values () Σ)]
  [(expand* (Cons stx stl) env Σ)
   (values (Cons stx_exp stl_exp) Σ_2)
   (where (values stx_exp Σ_1) (expand stx env Σ))
   (where (values stl_exp Σ_2) (expand* stl env Σ_1))])


;; ----------------------------------------
;; Drivers

(define-metafunction L
  primitives-env : -> env
  [(primitives-env) ()])

(define-metafunction L
  init-Σ : -> Σ
  [(init-Σ) (Sto 0 () () ())])

(define-helpers L (Set)
  reader printer)

(define-metafunction L
  stripper : (values stx Σ) -> val
  [(stripper (values stx Σ)) (strip stx)])

(define-metafunction L
  expander : stx -> (values stx Σ)
  [(expander stx) (expand stx (primitives-env) (init-Σ))])

(define-metafunction L
  parse/values : (values stx Σ) -> ast
  [(parse/values (values stx Σ)) (parse stx Σ)])

(define-runner run
  reader
  expander
  stripper printer
  eval
  parse/values)


;; ----------------------------------------
;; Examples:

(define ex-<
  '[<
    (< 3 5)])

(define ex-eq?
  '[eq?
    (eq? 'a 'a)])

(define ex-let
  '[let-x
    (let ([x 1]) (+ x 2))])

(define ex-if-#t
  '[if-#t
    (if (< 0 1) 'foo 'bar)])

(define ex-if-#f
  '[if-#f
    (let ([x 3] [y 2])
      (if (< x y) (+ x y) (* x y)))])

(define ex-simple
  '[simple
    (let-syntax ([x (lambda (stx) #'2)])
      (x 1))])
(define (raw-simple)
  (let-syntax ([x (lambda (stx) #'2)])
    (x 1)))

(define ex-reftrans
  '[reftrans
    (let ([z 1])
      ((let-syntax ([x (lambda (stx) #'z)])
         (lambda (z) (x))) 2))])
(define (raw-reftrans)
  (lambda (z)
    (let-syntax ([x (lambda (stx) #'z)])
      (lambda (z) (x)))))

(define ex-hyg
  '[hyg
    (let ([z 1])
      ((let-syntax
           ([x (lambda (stx)
                 (#%app datum->syntax
                        #'here
                        (#%app list #'lambda (#%app datum->syntax #'here (#%app list #'z))
                               (#%app second (#%app syntax-e stx)))))])
         (x z)) 2))])
(define (raw-hyg)
  (lambda (z)
    (let-syntax ([x (lambda (stx)
                      #`(lambda (z) #,(second (syntax-e stx))))])
      (x z))))


(define ex-thunk
  '[thunk
    (let-syntax
        ([thunk (lambda (stx)
                  (#%app datum->syntax
                         stx
                         (#%app list #'lambda (#%app datum->syntax stx (#%app list #'a)) 
                                (#%app second (#%app syntax-e stx)) ;; #'(+ a 1)
                                )))])
      ((let ([a 5])
         (thunk (+ a 1))) 0))])
(define (raw-thunk)
  (let-syntax ([thunk (lambda (stx)
                        #`(lambda (a)
                            #,(second (syntax-e stx)) ;; #'(+ a 1)
                            ))])
    (((lambda (a) (thunk (+ a 1))) 5) 0)))


(define ex-get-identity
  '[get-identity
    (let-syntax
        ([get-identity (lambda (stx)
                         (#%app datum->syntax
                                stx
                                (#%app list #'lambda 
                                       (#%app datum->syntax stx (#%app list #'a))
                                       (#%app datum->syntax
                                              stx
                                              (#%app list #'lambda
                                                     (#%app datum->syntax
                                                            stx
                                                            (#%app list
                                                                   (#%app second (#%app syntax-e stx)) ;; #'a
                                                                   ))
                                                     #'a)))))])
      (((get-identity a) 1) 2))])
(define (raw-get-identity)
  (let-syntax ([get-identity (lambda (stx)
                               #`(lambda (a)
                                   (lambda (#,(second (syntax-e stx))) ;; #'a
                                     a)))])
    (get-identity a)))

(define core:examples
  (list ex-<
        ex-eq?
        ex-let
        ex-if-#t ex-if-#f
        ex-simple
        ex-reftrans
        ex-hyg
        ex-thunk
        ex-get-identity))


;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           ;pict
           "config.rkt")
  (provide (all-defined-out))

  (define base-nts '(ast var val
                     stx id
                     atom
                     sym
                     nam))
  (define eval-language-pict
    (WR (language->pict L #:nts base-nts)))
  (define eval-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict eval #:contract? #t))))

  (define prim-nts '(prim))
  (define prim-language-pict
    (WR (language->pict L #:nts prim-nts)))
  (define δ-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict δ/stx))))

  (define parse-pict
    (WR (metafunction->pict parse #:contract? #t)))
  (define resolve-nts '(scps ctx
                        Σ
                        scp))
  (define resolve-language-pict
    (WR (language->pict L #:nts resolve-nts)))
  (define resolve-pict
    (vl-append
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (blank 0 (metafunction-gap-space)))
     (WR
      (parameterize ([where-combine (lambda (l r) r)]
                     [metafunction-cases '(0)])
        (metafunction->pict biggest-subset #:contract? #t)))))

  (define (make-expand-pict pos [contract? #f] #:narrower? [narrower? #f])
    (parameterize ([metafunction-cases (list pos)]
                   [linebreaks (append
                                (if contract? '(#f) '())
                                (list (and narrow-mode? narrower?)))])
      (WR (metafunction->pict expand #:contract? contract?))))
  
  (define expand-quote-pict (make-expand-pict 1 #t))
  (define expand-syntax-pict (make-expand-pict 2))
  (define expand-lambda-pict (make-expand-pict 0 #:narrower? #t))
  (define expand-var-pict (make-expand-pict 6))
  (define expand-let-syntax-pict (make-expand-pict 3))
  (define expand-macro-app-pict (make-expand-pict 4))
  (define expand-app-pict
    (vl-append
     (make-expand-pict 5 #:narrower? #t)
     (WR (blank 0 (metafunction-gap-space)))
     (WR (metafunction->pict expand* #:contract? #t))))
  
  (define expand-nts '(env transform))
  (define expand-language-pict
    (WR (language->pict L #:nts expand-nts)))

  (define add+flip-pict
    (vl-append
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (WR (blank 0 (metafunction-gap-space)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t)))))

  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict L lw)))
  
  (define all-nts (append base-nts
                          prim-nts
                          resolve-nts
                          expand-nts)))

(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view eval-language-pict
        (hc-append
         40
         (vl-append
          expand-language-pict
          expand-app-pict)
         parse-pict)
        resolve-pict
        add+flip-pict))

;; Providing this file to `scribble` will render the model.
;; Set the `SCOPE_SETS_TO_PDF` environment variable to get
;; the right scale for PDF output.
(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict))
  (provide doc)
  (define doc
    (make-model-doc
     "Single-Phase"
     (WR (language->pict L #:nts all-nts))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict eval #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict strip #:contract? #t)))
     (WR (metafunction->pict expand #:contract? #t))
     (WR (metafunction->pict expand* #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
