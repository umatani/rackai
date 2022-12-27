#lang racket
(require redex
         "rewrites.rkt"
         "define-example.rkt"
         (except-in slideshow/pict explain))

(define-language mini
  
  ;; Executable AST and values:
  [ast var (App ast ast ...) val]
  [var (Var nam)]
  [val (Fun var ast) atom
       (List val ...) stx (Defs σ)]
  [pre-val desc-other-atom (Defs σ)]
  
  ;; Syntax objects (a subset of values):
  [stx (Stx atom ctx)
       (Stx (List stx ...) ctx)]
  [id (Stx sym ctx)]
  [pre-ctx desc-other-atom (Rename ctx id nam σ) (Defs ctx σ)]
  [ctx •
       (Rename ctx id nam σ)
       (Mark ctx mrk)
       (Defs ctx σ)]
       
  ;; Literal values:
  [atom sym prim tprim desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number]
  [sym (Sym nam)]
  [prim SE MKS desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim CONS CAR CDR LIST + -]
  [tprim desc-old-tprim NEW-DEFS DEF-BIND]
  [desc-old-tprim LOCAL-VALUE LOCAL-EXPAND]
  ;; for typesetting:
  [all-tprim NEW-DEFS DEF-BIND 
             LOCAL-VALUE LOCAL-EXPAND]

  ;; Expand-time environment:
  [env desc-env] ; `desc-env' typesets as prose
  [desc-env ((nam transform) ...)]
  [transform TFun TLet-Syntax TQuote
             (TVar id) val TStop]
  
  ;; Definition-context and store:
  [σ addr NULL]
  [Σ desc-Σ] ; `desc-Σ' typesets as prose
  [desc-Σ ((σ (id nam) ...) ...)]  
  [S desc-S] ; `desc-S' typesets as prose
  [desc-S (σ ...)]

  ;; Use names for vars, addrs, and marks
  [nam desc-name] ; `desc-name' typesets as prose
  [(addr mrk) nam]
  [desc-name variable-not-otherwise-mentioned]
  
  ;; For name generation:
  [gen (number ...)]
  [env+gen (Fst env gen)]
  [mrk+gen (Fst mrk gen)])

;; ----------------------------------------
;; Non-capturing substitution for AST:

(define-metafunction mini
  subst : ast var ast -> ast
  [(subst var var ast_v) ast_v]
  [(subst var_2 var ast_v) var_2]
  [(subst (App ast ...) var ast_v)
   (App (subst ast var ast_v) ...)]
  [(subst (Fun var ast) var ast_v)
   (Fun var ast)]
  [(subst (Fun var_2 ast) var ast_v)
   (Fun var_3 (subst (subst ast var_2 var_3) var ast_v))
   (where (Var nam_2) var_2)
   (where var_3 (Var ,(variable-not-in (term ast_v) (term nam_2))))]
  [(subst atom var ast_v) atom]
  [(subst (List val ...) var ast_v) 
   (List (subst val var ast_v) ...)]
  [(subst stx var ast_v) stx]
  [(subst (Defs σ) var ast_v) (Defs σ)])

;; ----------------------------------------
;; Generic metafunctions that typeset as set operations:

(define-metafunction mini
  [(is-in? any_1 ()) #f]
  [(is-in? any_1 (any_1 any_2 ...)) #t]
  [(is-in? any_1 (any_2 any_3 ...)) (is-in? any_1 (any_3 ...))])

(define-metafunction mini
  [(emptyset) ()])

(define-metafunction mini
  [(add-elem any_1 any_2) (any_1 . any_2)])

(define-metafunction mini
  [(same? any_1 any_2) ,(equal? (term any_1) (term any_2))])

;; ----------------------------------------
;; Implementation of primitives:

(define-metafunction mini
  [(plus number_1 number_2) ,(+ (term number_1) (term number_2))])
(define-metafunction mini
  [(minus number_1 number_2) ,(+ (term number_1) (term number_2))])

(define-metafunction mini
  δ/stx : prim (val ...) -> val
  [(δ/stx SE ((Stx val ctx))) val]
  [(δ/stx MKS (atom (Stx val ctx))) (Stx atom ctx)]
  [(δ/stx MKS ((List stx ...) (Stx val ctx))) (Stx (List stx ...) ctx)])

(define-metafunction/extension δ/stx mini
  δ : prim (val ...) -> val
  [(δ + (number_1 number_2)) (plus number_1 number_2)]
  [(δ - (number_1 number_2)) (minus number_1 number_2)]
  [(δ CONS (val_1 (List val_2 ...))) (List val_1 val_2 ...)]
  [(δ CAR ((List val_1 val_2 ...))) val_1]
  [(δ CDR ((List val_1 val_2 ...))) (List val_2 ...)]
  [(δ LIST (val ...)) (List val ...)])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction mini
  eval : ast env mrk+gen Σ -> (StE val env Σ)
  
  ;; standard eval
  [(eval (App (Fun var ast_body) ast_arg) env (Fst mrk gen) Σ)
   (eval (subst ast_body var val) env_1 [Fst mrk (1 . gen)] Σ_1)
   (where [StE val env_1 Σ_1] (eval ast_arg env (Fst mrk (0 . gen)) Σ))]
  [(eval (App prim ast_arg ...) env (Fst mrk gen) Σ)
   [StE (δ prim (val ...)) env_1 Σ_1]
   (where [StE (val ...) env_1 Σ_1] (eval* () (ast_arg ...) env (Fst mrk (0 . gen)) Σ))]
  [(eval (App ast_op ast_arg ...))
   (eval (App (eval ast_op) ast_arg ...))]
  [(eval val env (Fst mrk gen) Σ) [StE val env Σ]]

  ;; local value
  [(eval (App LOCAL-VALUE ast) env (Fst mrk gen) Σ)
   [StE (lookup env (resolve stx Σ_1)) env_1 Σ_1]
   (where [StE stx env_1 Σ_1] (eval ast env (Fst mrk gen) Σ))]

  ;; local expand
  [(eval (App LOCAL-EXPAND ast_expr ast_stops ast_defs) env (Fst mrk gen) Σ)
   [StE (mark (defs stx σ) mrk) env_3 Σ_4]
   (where [StE stx_expr env_1 Σ_1] (eval ast_expr env (Fst mrk (0 . gen)) Σ))
   (where [StE (List id_stop ...) env_2 Σ_2] (eval ast_stops env_1 (Fst mrk (1 . gen)) Σ_1))
   (where [StE (Defs σ) env_3 Σ_3] (eval ast_defs env_2 (Fst mrk (2 . gen)) Σ_2))
   (where env_stops (extend* (nostops env_3) (((resolve id_stop Σ_3) TStop) ...)))
   (where stx_new (defs (mark stx_expr mrk) σ))
   (where [St stx Σ_4] (expand stx_new (Fst env_stops (3 . gen)) Σ_3))]
  
  ;; create definition context
  [(eval (App NEW-DEFS) env (Fst mrk gen) Σ)
   [StE (Defs σ) env (alloc Σ σ)]
   (where σ (fresh-name (Stx (Sym σ) •) gen))]

  ;; bind name in definition context
  [(eval (App DEF-BIND ast_defs ast_id) env (Fst mrk gen) Σ)
   [StE 0 (extend env_2 nam_new (TVar id_new)) Σ_3]
   (where [StE (Defs σ) env_1 Σ_1] (eval ast_defs env (Fst mrk (0 . gen)) Σ))
   (where [StE id env_2 Σ_2] (eval ast_id env_1 (Fst mrk (1 . gen)) Σ_1))
   (where nam_new (fresh-name id gen))
   (where id_new (rename id id nam_new))
   (where Σ_3 (store Σ_2 σ (mark id mrk) nam_new) )]

  ;; bind expand-time value in definition context
  [(eval (App DEF-BIND ast_defs ast_id ast_stx) env (Fst mrk gen) Σ)
   [StE 0 (extend env_4 nam_new val) Σ_5]
   (where [StE (Defs σ) env_1 Σ_1] (eval ast_defs env (Fst mrk (0 . gen)) Σ))
   (where [StE id env_2 Σ_2] (eval ast_id env_1 (Fst mrk (1 . gen)) Σ_1))
   (where [StE stx env_3 Σ_3] (eval ast_stx env_2 (Fst mrk (2 . gen)) Σ_2))
   (where [StE val env_4 Σ_4] (eval (parse (defs (mark stx mrk) σ) Σ_3) env_3 (Fst mrk (3 . gen)) Σ_3))
   (where nam_new (fresh-name id gen))
   (where id_new (rename id id nam_new))
   (where Σ_5 (store Σ_4 σ (mark id mrk) nam_new))])

(define-metafunction mini
  eval* : (val ...) (ast ...) env mrk+gen Σ -> (StE (val ...) env Σ)
  [(eval* (val ...) () env (Fst mrk gen) Σ) (StE (val ...) env Σ)]
  [(eval* (val ...) (ast_0 ast_1 ...) env [Fst mrk (number_0 number ...)] Σ)
   (eval* (val ... val_0) (ast_1 ...) env_1 [Fst mrk (number_1 number ...)] Σ_1)
   (where [StE val_0 env_1 Σ_1] (eval ast_0 env [Fst mrk (number_0 number ...)] Σ))
   (where/hidden number_1 ,(add1 (term number_0)))])
  
;; ----------------------------------------
;; Definition-context store operations:

(define-metafunction mini
  alloc : Σ σ -> Σ
  [(alloc Σ σ) ((σ) . Σ)])

(define-metafunction mini
  store : Σ σ id nam -> Σ
  [(store ((σ any_1 ...) any_2 ...) σ id nam) 
   ((σ (id nam) any_1 ...) any_2 ...)]
  [(store (any_1 any_2 ...) σ id nam)
   ,(cons (term any_1) (term (store (any_2 ...) σ id nam)))])

(define-metafunction mini
  [(store-lookup (any_1 ... (σ (id nam_new) ...) any_2 ...) σ)
   (store-val ((id nam_new) ...))]
  [(store-lookup any_1 any_2) #f])

;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction mini
  ;; Adds or cancels a mark
  addremove : mrk (mrk ...) -> (mrk ...)
  [(addremove mrk_1 (mrk_1 mrk_2 ...)) (mrk_2 ...)]
  [(addremove mrk_1 (mrk_2 ...)) (mrk_1 mrk_2 ...)])

(define-metafunction mini
  ;; Extracts the range of a store value
  rng : (store-val ((id nam) ...)) -> (nam ...)
  [(rng (store-val ((id nam) ...))) (nam ...)])

(define-metafunction mini
  ;; Extracts all marks in order, removing cancelling pairs
  marksof : id Σ nam -> (mrk ...)
  [(marksof (Stx val •) Σ nam) ()]
  [(marksof (Stx val (Mark ctx mrk)) Σ nam)
   (addremove mrk (marksof (Stx val ctx) Σ nam))]
  [(marksof (Stx val (Rename ctx id nam_2 σ)) Σ nam) (marksof (Stx val ctx) Σ nam)]
  [(marksof (Stx val (Defs ctx σ)) Σ nam) ()
   (side-condition (term (is-in? nam (rng (store-lookup Σ σ)))))]
  [(marksof (Stx val (Defs ctx σ)) Σ nam) (marksof (Stx val ctx) Σ nam)])

(define-metafunction mini
  ;; Unrolls a set of definition-context renamings into `Reaname's
  renames : σ ((id nam) ...) ctx -> ctx
  [(renames σ () ctx) ctx]
  [(renames σ ((id nam) (id_2 nam_2) ...) ctx)
   (renames σ ((id_2 nam_2) ...) 
            .((Rename ctx id nam σ)))])

(define (bad-resolve-partial-pict)
  (define-metafunction mini
    ;; Resolves an identifier to a name; this is the heart of
    ;;  the syntax-object support for lexical scope
    resolve : id Σ -> nam
    [(resolve (Stx val (Defs ctx σ)) Σ)
     (resolve (Stx val ctx_new) Σ)
     (where (store-val ((id nam_new) ...)) (store-lookup Σ σ))
     (where ctx_new (renames σ ((id nam_new) ...) ctx))])
  (metafunction->pict resolve))

(define (resolve*-partial-pict)
  (define-metafunction mini
    ;; Resolves an identifier to a name; this is the heart of
    ;;  the syntax-object support for lexical scope
    resolve* : id Σ S S -> nam
    [(resolve* (Stx (Sym nam) (Rename ctx id_orig nam_new σ)) Σ S_spine S_branch)
     nam_new
     (where nam_1 (resolve* id_orig Σ S_branch S_branch))
     (where nam_1 (resolve* (Stx (Sym nam) ctx) Σ (add-elem σ S_spine) S_branch))
     (side-condition (term (same? (marksof id_orig nam_1) (marksof (Stx (Sym nam) ctx) nam_1))))]
    [(resolve* (Stx (Sym nam) (Defs ctx σ)) Σ S_spine S_branch)
     (resolve* (Stx (Sym nam) ctx) 
               Σ S_spine S_branch)
     (side-condition (term (is-in? σ S_spine)))]
    [(resolve* (Stx (Sym nam) (Defs ctx σ)) Σ S_spine S_branch)
     (resolve* (Stx (Sym nam) ctx_new) 
               Σ S_spine (add-elem σ S_branch))
     (where (store-val ((id nam_new) ...)) (store-lookup Σ σ))
     (where ctx_new (renames σ ((id nam_new) ...) ctx))])
  (metafunction->pict resolve*))

(define-metafunction mini
  ;; Resolves an identifier to a name; this is the heart of
  ;;  the syntax-object support for lexical scope
  resolve* : id Σ S S -> nam
  [(resolve* (Stx (Sym nam) •) Σ S_spine S_branch) nam]
  [(resolve* (Stx (Sym nam) (Mark ctx mrk)) Σ S_spine S_branch)
   (resolve* (Stx (Sym nam) ctx) 
             Σ S_spine S_branch)]
  [(resolve* (Stx (Sym nam) (Rename ctx id_orig nam_new σ)) Σ S_spine S_branch)
   nam_new
   (where nam_1 (resolve* id_orig Σ S_branch S_branch))
   (where nam_1 (resolve* (Stx (Sym nam) ctx) Σ (add-elem σ S_spine) S_branch))
   (side-condition (term (same? (marksof id_orig Σ nam_1) (marksof (Stx (Sym nam) ctx) Σ nam_1))))]
  [(resolve* (Stx (Sym nam) (Rename ctx id_orig nam_2 σ)) Σ S_spine S_branch) 
   (resolve* (Stx (Sym nam) ctx) 
             Σ S_spine S_branch)]
  [(resolve* (Stx (Sym nam) (Defs ctx σ)) Σ S_spine S_branch)
   (resolve* (Stx (Sym nam) ctx) 
             Σ S_spine S_branch)
   (side-condition (term (is-in? σ S_spine)))]
  [(resolve* (Stx (Sym nam) (Defs ctx σ)) Σ S_spine S_branch)
   (resolve* (Stx (Sym nam) ctx_new) 
             Σ S_spine (add-elem σ S_branch))
   (where (store-val ((id nam_new) ...)) (store-lookup Σ σ))
   (where ctx_new (renames σ ((id nam_new) ...) ctx))])

(define-metafunction mini
  resolve : stx Σ -> nam
  ;; Starts `resolve*' with empty skip sets
  [(resolve stx Σ) (resolve* stx Σ (emptyset) (emptyset))])

(define-metafunction mini
  rename : stx id nam -> stx
  ;; Simply pushes `Rename's down through a syntax object
  [(rename (Stx atom ctx) id nam) 
   (Stx atom (Rename ctx id nam NULL))]
  [(rename (Stx (List stx ...) ctx) id nam) 
   (Stx (List (rename stx id nam) ...) 
        (Rename ctx id nam NULL))])

(define-metafunction mini
  mark : stx mrk -> stx
  ;; Simply pushes `Mark's down through a syntax object
  [(mark (Stx atom ctx) mrk) 
   (Stx atom (Mark ctx mrk))]
  [(mark (Stx (List stx ...) ctx) mrk) 
   (Stx (List (mark stx mrk) ...) (Mark ctx mrk))])

(define-metafunction mini
  defs : stx σ -> stx
  ;; Simply pushes `Def's down through a syntax object
  [(defs (Stx atom ctx) σ) 
   (Stx atom (Defs ctx σ))]
  [(defs (Stx (List stx ...) ctx) σ) 
   (Stx (List (defs stx σ) ...) (Defs ctx σ))])

(define-metafunction mini
  strip : stx -> val
  ;; Recursively strips lexical context from a syntax object
  [(strip (Stx atom ctx))
   atom]
  [(strip (Stx (List stx ...) ctx)) 
   (List (strip stx) ...)])

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

(define-metafunction mini
  parse : stx Σ -> ast
  [(parse (Stx (List id_lambda id_arg stx_body) ctx) Σ)
   (Fun (Var (resolve id_arg Σ)) (parse stx_body Σ))
   (where lambda (resolve id_lambda Σ))]
  [(parse (Stx (List id_quote stx) ctx) Σ)
   (strip stx)
   (where quote (resolve id_quote Σ))]
  [(parse (Stx (List id_syntax stx) ctx) Σ)
   stx
   (where syntax (resolve id_syntax Σ))]
  [(parse (Stx (List stx_rator stx_rand ...) ctx) Σ)
   (App (parse stx_rator Σ) (parse stx_rand Σ) ...)]
  [(parse id Σ)
   (Var (resolve id Σ))])

;; ----------------------------------------
;; Expand-time environment operations:

(define-metafunction mini
  lookup : env nam -> transform
  [(lookup ((nam transform) any_2 ...) nam) transform]
  [(lookup (any_1 any_2 ...) nam) (lookup (any_2 ...) nam)])

(define-metafunction mini
  extend : env nam transform -> env
  [(extend env nam transform) ((nam transform) . env)])

(define-metafunction mini
  extend* : env ((nam transform) ...) -> env
  [(extend* env ((nam transform) ...)) ((nam transform) ... . env)])

(define-metafunction mini
  nostops : env -> env
  [(nostops env)
   ,(filter (lambda (p)
              (not (eq? (cadr p) (term TStop))))
            (term env))])

;; ----------------------------------------
;; Fresh-name helper for expander:

(define-metafunction mini
  [(fresh-name (Stx (Sym nam) ctx) gen)
   ,(string->symbol (format "~a~a"
                            (term nam)
                            (apply
                             string-append
                             (for/list ([c (reverse (term gen))])
                               (format ":~a" c)))))])

(define-metafunction mini
  [(enumerate number) ()]
  [(enumerate number any any_2 ...)
   ,(cons (term number)
          (term (enumerate ,(add1 (term number)) any_2 ...)))])

;; ----------------------------------------
;; The expander:

(define-metafunction mini
  expand : stx env+gen Σ -> (St stx Σ)

  ;; lambda
  [(expand (Stx (List id_lam id_arg stx_body) ctx) [Fst env gen] Σ)
   [St (Stx (List id_lam id_new stx_expbody) ctx) Σ_1]
   (where TFun (lookup env (resolve id_lam Σ)))
   (where nam_new (fresh-name id_arg gen))
   (where id_new (rename id_arg id_arg nam_new))
   (where stx_newbody (rename stx_body id_arg nam_new))
   (where env_new (extend env nam_new (TVar id_new)))
   (where [St stx_expbody Σ_1] (expand stx_newbody [Fst env_new (0 . gen)] Σ))]
  
  ;; quote & syntax
  [(expand (Stx (List id_quote stx) ctx) [Fst env gen] Σ)
   [St (Stx (List id_quote stx) ctx) Σ]
   (where TQuote (lookup env (resolve id_quote Σ)))]
  
  ;; macro binding
  [(expand (Stx (List id_ls id_mac stx_rhs stx_body) ctx) [Fst env gen] Σ)
   (expand stx_newbody [Fst (extend env nam_new val) (1 . gen)] Σ_1)
   (where TLet-Syntax (lookup env (resolve id_ls Σ)))
   (where nam_new (fresh-name id_mac gen))
   (where [StE val env_1 Σ_1] (eval (parse stx_rhs Σ) env (Fst no-mrk (0 . gen)) Σ))
   (where stx_newbody (rename stx_body id_mac nam_new))]

  ;; macro application
  [(expand stx_macapp [Fst env gen] Σ)
   (expand (mark stx_exp mrk_new) [Fst env (1 . gen)] Σ_1)
   (where (Stx (List id_mac stx_arg ...) ctx) stx_macapp)
   (where val (lookup env (resolve id_mac Σ)))
   (where mrk_new (fresh-name (Stx (Sym mrk) •) gen))   
   (where [StE stx_exp env_1 Σ_1] (eval (App val (mark stx_macapp mrk_new)) env [Fst mrk_new (0 . gen)] Σ))]

  ;; stops
  [(expand (Stx (List id_stop stx ...) ctx) [Fst env gen] Σ)
   [St (Stx (List id_stop stx ...) ctx) Σ]
   (where TStop (lookup env (resolve id_stop Σ)))]

  ;; application
  [(expand (Stx (List stx_rtor stx_rnd ...) ctx) [Fst env gen] Σ)
   [St (Stx (List stx_exprtor stx_exprnd ...) ctx) Σ_1]
   (where [St (stx_exprtor stx_exprnd ...) Σ_1] (expand* () (stx_rtor stx_rnd ...) [Fst env (0 . gen)] Σ))]
  
  ;; reference
  [(expand id [Fst env gen] Σ)
   [St id_new Σ]
   (where (TVar id_new) (lookup env (resolve id Σ)))])

(define-metafunction mini
  expand* : (stx ...) (stx ...) [Fst env gen] Σ -> (St (stx ...) Σ)
  [(expand* (stx_done ...) () [Fst env gen] Σ) (St (stx_done ...) Σ)]
  [(expand* (stx_done ...) (stx_0 stx_1 ...) [Fst env (number_0 number ...)] Σ)
   (expand* (stx_done ... stx_done0) (stx_1 ...) [Fst env (number_1 number ...)] Σ_1)
   (where [St stx_done0 Σ_1] (expand stx_0 [Fst env (number_0 number ...)] Σ))
   (where/hidden number_1 ,(add1 (term number_0)))])

;; ----------------------------------------
;; Helpers for writing examples:

(define-metafunction mini
  preamble-env : -> env
  [(preamble-env) ((lambda TFun)
                   (let-syntax TLet-Syntax)
                   (quote TQuote)
                   (syntax TQuote))])

;; ----------------------------------------
;; Examples:

(define-metafunction mini
  [(parse/st [St stx Σ]) (parse stx Σ)])

(define-example-definer define-example
  mini 
  (lambda (t) (term (expand ,t (Fst (preamble-env) ()) ())))
  parse/st)

(define-example simple-macro-example
  (let-syntax x (lambda z (syntax (quote 2))) (x 1))
  2)

(define-example reftrans-macro-example
  (lambda z (let-syntax x (lambda s (syntax z)) (lambda z (x))))
  (Fun (Var z) (Fun (Var z:0:1) (Var z))))

(define-example hyg-macro-example
  (lambda z (let-syntax x (lambda s 
                            ('MKS
                             ('LIST (syntax lambda)
                                    (syntax z)
                                    ('CAR ('CDR ('SE s))))
                             (syntax here)))
              (x z)))
  (Fun (Var z) (Fun (Var z:0:1:1) (Var z))))

(define-example local-value-example
  (let-syntax a '8
    (let-syntax b '9
      (let-syntax x (lambda s 
                      ('MKS
                       ('LIST (syntax quote)
                              ('MKS ('LOCAL-VALUE ('CAR ('CDR ('SE s))))
                                    (syntax here)))
                       (syntax here)))
        (x a))))
  8)

(define-example local-expand-example
  (let-syntax q (lambda s (syntax ('CAR '8)))
    (let-syntax x (lambda s 
                    ;; Used as (x (q)) => extracts '8 from ('CAR '8)
                    ('CAR ('CDR ('SE ('LOCAL-EXPAND ('CAR ('CDR ('SE s))) 
                                                    ('LIST)
                                                    ('NEW-DEFS))))))
      (x (q))))
  8)

(define-example local-expand-stop-example
  (let-syntax p (lambda s (quote 0))
    (let-syntax q (lambda s (syntax ('CAR '8)))
      (let-syntax x (lambda s 
                      ;; Used as (x (q)) => extracts '8 from ('CAR '8)
                      ('CAR ('CDR ('SE ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                                      ('LIST (syntax p))
                                                      ('NEW-DEFS))))))
        (x (q)))))
  8)

(define-example nested-local-expand-example
  (let-syntax z (lambda s (syntax '0))
    (let-syntax a (lambda s
                    ;; When `b' forces `a', then `a'
                    ;; drops `z' form the stop list, so it
                    ;; should expand to 0
                    ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                   ('LIST)
                                   ('NEW-DEFS)))
      (let-syntax b (lambda s
                      ('MKS
                       ('LIST
                        (syntax quote)
                        ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                       ('LIST (syntax z))                                       
                                       ('NEW-DEFS)))
                       s))
        ('LIST (b (z)) (b (a (z)))))))
  (App LIST (List (Sym z)) (List (Sym quote) 0)))

(define-example defs-shadow-example
  (let-syntax call (lambda s ('MKS ('LIST ('CAR ('CDR ('SE s))))
                                   (syntax here)))
    (let-syntax p (lambda s (syntax '0))
      (let-syntax q (lambda s
                      ((lambda ctx
                         ((lambda ignored
                            ('MKS
                             ('LIST
                              (syntax lambda)
                              ('CAR ('CDR
                                     ('SE
                                      ('LOCAL-EXPAND ('MKS
                                                      ('LIST (syntax syntax)
                                                             ('CAR ('CDR ('SE s))))
                                                      (syntax here))
                                                     ('LIST)
                                                     ctx))))
                              ('LOCAL-EXPAND ('CAR ('CDR ('CDR ('SE s))))
                                             ('LIST (syntax call))
                                             ctx))
                             (syntax here)))
                          ('DEF-BIND ctx ('CAR ('CDR ('SE s))))))
                       ('NEW-DEFS)))
        (q p (call p)))))
  (Fun (Var p:1:1:1:1) (App (Var p:1:1:1:1))))

(define-example defs-local-macro-example
  (let-syntax call (lambda s ('MKS ('LIST ('CAR ('CDR ('SE s)))) 
                                   (syntax here)))
    (let-syntax p (lambda s (syntax '0))
      (let-syntax q (lambda s
                      ((lambda ctx
                         ((lambda ignored
                            ('MKS
                             ('LIST
                              (syntax lambda)
                              ('CAR ('CDR
                                     ('SE
                                      ('LOCAL-EXPAND ('MKS 
                                                      ('LIST (syntax syntax)
                                                             ('CAR ('CDR ('SE s))))
                                                      (syntax here))
                                                     ('LIST)
                                                     ctx))))
                              ('LOCAL-EXPAND ('CAR ('CDR ('CDR ('SE s))))
                                             ('LIST)
                                             ctx))
                             (syntax here)))
                          ('DEF-BIND ctx ('CAR ('CDR ('SE s)))
                                     ('MKS
                                      ('LIST (syntax lambda)
                                             (syntax s)
                                             ('CAR ('CDR ('CDR ('CDR ('SE s))))))
                                      (syntax here)))))
                       ('NEW-DEFS)))
        (q p (call p) (syntax '13)))))
  (Fun (Var p:1:1:1:1) 13))

(define-example defs-begin-with-defn-example
  (let-syntax bwd (lambda s
                    ((lambda ctx ; the int-def context
                       ((lambda id1 ; the x in (define x '10)
                          ((lambda e1 ; the '10 in (define x '10)
                             ((lambda id2 ; the q n (define-syntax q (lambda  v ...))
                                ((lambda e2 ; the (lambda v ...) in (define-syntax q (lambda  v ...))
                                   ((lambda e3 ; the last body expression, expands to (lambda i x)
                                      ((lambda ignored ; for side-effect of binding x in ctx
                                         ((lambda ignored ; for side-effect of binding q in ctx
                                            ((lambda ee3 ; local-expand e3
                                               ((lambda qid1 ; local-expand id1 (in a syntax form)
                                                  ((lambda eid1 ; extract expanded id1 from qid1
                                                     ;; generate ((lambda eid1 ee3) '10):
                                                     ('MKS ('LIST
                                                            ('MKS ('LIST (syntax lambda)
                                                                         eid1
                                                                         ee3)
                                                                  (syntax here))
                                                            e1)
                                                           (syntax here)))
                                                   ('CAR ('CDR ('SE qid1)))))
                                                ;; local-expand of id1 (to give it context from ctx):
                                                ('LOCAL-EXPAND ('MKS ('LIST (syntax quote)
                                                                            id1)
                                                                     (syntax here))
                                                               ('LIST (syntax quote))
                                                               ctx)))
                                             ;; local-expand e3 (i.e., the body expression):
                                             ('LOCAL-EXPAND e3 ('LIST (syntax lambda)) ctx)))
                                          ;; bind id2 (i.e., q)
                                          ('DEF-BIND ctx id2 e2)))
                                       ;; bind id1 (i.e., x)
                                       ('DEF-BIND ctx id1)))
                                    ;; extract e3
                                    ('CAR ('CDR ('CDR ('CDR ('SE s)))))))
                                 ;; extract e2
                                 ('CAR ('CDR ('CDR ('SE ('CAR ('CDR ('CDR ('SE s))))))))))
                              ;; extract id2
                              ('CAR ('CDR ('SE ('CAR ('CDR ('CDR ('SE s)))))))))
                           ;; extract e1
                           ('CAR ('CDR ('CDR ('SE ('CAR ('CDR ('SE s)))))))))
                        ;; extract id1
                        ('CAR ('CDR ('SE ('CAR ('CDR ('SE s))))))))
                     ;; create ctx
                     ('NEW-DEFS)))
    ;; `bwd' is short for `begin-with-definitions', which
    ;; assumes a `define' followed by a `define-syntax' followed
    ;; by a body form
    (bwd (define x '10)
         (define-syntax q (lambda v (syntax (lambda i x))))
         #;(lambda i x)
         (q)))
  (App (Fun (Var x:1:1:0) (Fun (Var i:1:1:0:0) (Var x:1:1:0))) 10))

;; ----------------------------------------
;; Typesetting:

(define (lang->pict)
  (with-rewrites
   (lambda ()
     (ht-append
      20
      (vl-append
       10
       (language->pict mini #:nts '(ast var val atom sym prim all-tprim σ))
       (language->pict mini #:nts '(Σ env S)))
      (vl-append
       10
       (language->pict mini #:nts '(stx id ctx))
       (language->pict mini #:nts '(transform))
       (vl-append
        1
        (language->pict mini #:nts '(nam))
        (language->pict mini #:nts '(addr mrk))))))))

(define (delta->pict)
  (with-rewrites
   (lambda ()
     (vl-append (metafunction->pict δ/stx) (text "..." 'default (default-font-size))))))

(define (parse->pict)
  (with-rewrites
   (lambda ()
     (metafunction->pict parse))))

(define (resolve->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (metafunction->pict resolve)
      (parameterize (#;[linebreaks '(#f #t #f #t #t #t)])
        (metafunction->pict resolve*))))))

(define (metas->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      20
      (metafunction->pict renames)
      (metafunctions->pict mark)
      (metafunctions->pict rename)
      (metafunctions->pict defs)
      (metafunctions->pict marksof)
      (metafunctions->pict addremove)
      (metafunctions->pict strip)
      (metafunctions->pict nostops)))))

(define (eval->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
     (metafunction->pict eval)
     (metafunction->pict eval*)))))

(define (expand->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      20
      (metafunction->pict expand)
      (metafunction->pict expand*)))))

(define (tprim->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(tprim)))))

(define (ctx->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (language->pict mini #:nts '(pre-ctx pre-val))
      (language->pict mini #:nts '(σ Σ S))))))

(define (bad-resolve->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([linebreaks '(#f)])
       (bad-resolve-partial-pict)))))

(define (renames->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([linebreaks '(#f #f)])
       (metafunction->pict renames)))))

(define (resolve*->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (metafunction->pict resolve)
      (vl-append
       (text "...." 'roman (default-font-size))
       (parameterize ([metafunction-cases '(0)]
                      [linebreaks '(#f)])
         (resolve*-partial-pict))
       (parameterize ([metafunction-cases '(1 2)]
                      [linebreaks '(#f #f)])
         (resolve*-partial-pict)))))))

(define (marksof-fixed->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([linebreaks '(#f #f)]
                    [metafunction-cases '(3 4)])
       (metafunction->pict marksof)))))

(define (new-defs-eval->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(6)]
                    [linebreaks '(#f)])
       (metafunction->pict eval)))))

(define (def-var-eval->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(7)]
                    [linebreaks '(#f)])
       (metafunction->pict eval)))))

(define (def-exp-eval->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(8)]
                    [linebreaks '(#f)])
       (metafunction->pict eval)))))

(define (local-eval->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(5)]
                    [linebreaks '(#f)])
       (metafunction->pict eval)))))

(define (example-ids->pict)
  (with-rewrites
   (lambda ()
     (let-syntax ([ex (syntax-rules ()
                        [(_ e) (lw->pict mini (to-lw e))])])
       (vl-append
        10
        (vl-append
         (ex (Stx x (Defs (Defs • σ_1) σ_1)))
         (ex (Stx x (Defs (Defs (Defs (Defs • σ_1) σ_2) σ_2) σ_1))))
        (ex (Plain Σ = (Set (Bind σ_1 (Set (Bind (Stx x (Defs • σ_1)) (Comma x1))
                                           (Bind (Stx x (Defs (Defs (Defs (Defs • σ_1) σ_2) σ_2) σ_1)) x3)))
                            (Bind σ_2 (Set (Bind (Defs (Defs (Defs • σ_1) σ_2) σ_2)) x2))))))))))

(provide mini
         
         lang->pict
         delta->pict
         parse->pict
         resolve->pict
         metas->pict
         eval->pict
         expand->pict
         
         tprim->pict
         ctx->pict
         bad-resolve->pict
         renames->pict
         resolve*->pict
         marksof-fixed->pict
         new-defs-eval->pict
         def-var-eval->pict
         def-exp-eval->pict
         local-eval->pict
         example-ids->pict)
