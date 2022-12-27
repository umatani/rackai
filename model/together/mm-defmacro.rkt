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
       (List val ...) stx]

  ;; Syntax objects (a subset of values):
  [stx (Stx atom ctx)
       (Stx (List stx ...) ctx)]
  [id (Stx sym ctx)]
  [ctx •]
       
  ;; Literal values:
  [atom sym prim desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number]
  [sym (Sym nam)]
  [prim SE MKS desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim + - CONS CAR CDR LIST]

  ;; Expand-time environment:
  [env desc-env] ; `desc-env' typesets as prose
  [desc-env ((nam transform) ...)]
  [transform desc-other-trans (TVar id) val]  ; `desc-other-trans' typesets as "...."
  [desc-other-trans TFun TLet-Syntax TQuote]
  
  ;; Use names for vars, addrs, and marks
  [(nam) desc-name] ; `desc-name' typesets as prose
  [desc-name variable-not-otherwise-mentioned]
  
  ;; For name generation:
  [gen (number ...)]
  [env+gen (Fst env gen)])

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
  [(subst stx var ast_v) stx])

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
  eval : ast -> val
  [(eval (App (Fun var ast_body) ast_arg))
   (eval (subst ast_body var (eval ast_arg)))]
  [(eval (App prim ast_arg ...))
   (δ prim ((eval ast_arg) ...))]
  [(eval val) val])

;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction mini
  ;; Resolves an identifier to a name; this is the heart of
  ;;  the syntax-object support for lexical scope
  resolve : id -> nam
  [(resolve (Stx (Sym nam) •)) nam])

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
  parse : stx -> ast
  [(parse (Stx (List id_lambda id_arg stx_body) ctx))
   (Fun (Var (resolve id_arg)) (parse stx_body))
   (where lambda (resolve id_lambda))]
  [(parse (Stx (List id_quote stx) ctx))
   (strip stx)
   (where quote (resolve id_quote))]
  [(parse (Stx (List id_syntax stx) ctx))
   stx
   (where syntax (resolve id_syntax))]
  [(parse (Stx (List stx_rator stx_rand ...) ctx))
   (App (parse stx_rator) (parse stx_rand) ...)]
  [(parse id)
   (Var (resolve id))])

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
  [(enumerate number stx stx_2 ...)
   ,(cons (term number)
          (term (enumerate ,(add1 (term number)) stx_2 ...)))])

;; ----------------------------------------
;; The expander:

(define-metafunction mini
  expand : stx env+gen -> stx

  ;; lambda
  [(expand (Stx (List id_lam id_arg stx_body) ctx) [Fst env gen])
   (Stx (List id_lam id_new (expand stx_body [Fst env_new (0 . gen)])) ctx)
   (where TFun (lookup env (resolve id_lam)))
   (where nam_new (fresh-name id_arg gen))
   (where id_new (Stx (Sym nam_new) •))
   (where env_new (extend env (resolve id_arg) (TVar id_new)))]

  ;; quote & syntax
  [(expand (Stx (List id_quote stx) ctx) [Fst env gen])
   (Stx (List id_quote stx) ctx)
   (where TQuote (lookup env (resolve id_quote)))]
  
  ;; Macro creation:
  [(expand (Stx (List id_ls id_mac stx_rhs stx_body) ctx) [Fst env gen])
   (expand stx_body [Fst env_1 gen])
   (where TLet-Syntax (lookup env (resolve id_ls)))
   (where env_1 (extend env (resolve id_mac) (eval (parse stx_rhs))))]

  ;; Macro invocation:
  [(expand stx_macapp [Fst env gen])
   (expand (eval (App val stx_macapp)) [Fst env gen])
   (where (Stx (List id_mac stx_arg ...) ctx) stx_macapp)
   (where val (lookup env (resolve id_mac)))]
  
  ;; application
  [(expand (Stx (List stx_rator stx_rand ...) ctx) [Fst env gen])
   (Stx (List (expand stx_rator [Fst env (0 . gen)]) (expand stx_rand [Fst env (number . gen)]) ...) ctx)
   (where/hidden (number ...) (enumerate 1 stx_rand ...))]
  
  ;; reference
  [(expand id [Fst env gen])
   id_new
   (where (TVar id_new) (lookup env (resolve id)))])

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

(define-example-definer define-example
  mini 
  (lambda (t) (term (expand ,t [Fst (preamble-env) ()])))
  parse)

(define-example simple-parse-example
  (lambda x (lambda y (y ('+ x '5))))
  (Fun (Var x) (Fun (Var y:0) (App (Var y:0) (App + (Var x) 5)))))

(define-example shadow-parse-example
  (lambda lambda (lambda lambda lambda))
  (Fun (Var lambda) (Fun (Var lambda) (Var lambda))))

(define-example simple-macro-example
  (let-syntax x (lambda z (syntax (quote 2))) (x 1))
  2)

(define-example reftrans-macro-example
  (lambda z (let-syntax x (lambda s (syntax z)) (lambda z (x))))
  (Fun (Var z) (Fun (Var z:0) (Var z:0))))

(define-example hyg-macro-example
  (lambda z (let-syntax x (lambda s 
                            ('MKS
                             ('LIST (syntax lambda)
                                    (syntax z)
                                    ('CAR ('CDR ('SE s))))
                             (syntax here)))
              (x z)))
  (Fun (Var z) (Fun (Var z:0) (Var z:0))))

;; ----------------------------------------
;; Typesetting:

(define (lang->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(transform)))))

(define (metas->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (vl-append (metafunction->pict δ/stx) (text "..." 'default (default-font-size)))
      (metafunction->pict parse)
      (metafunction->pict resolve)
      (metafunctions->pict strip)))))

(define (example->pict)
  (with-rewrites
   (lambda ()
     (let-syntax ([step (syntax-rules ()
                          [(_ e) (lw->pict mini (to-lw e))])])
       (append-steps
        (step (expand (lambda a (thunk ('+ a '1))) env_0))
        (step (lambda a2 (expand (thunk ('+ a '1)) (elem env_1 = (extend env_0 a a2)))))
        (step-... "calling the" (step thunk) "transformer")
        (step (lambda a2 (expand (lambda a ('+ a '1)) env_1)))
        (step (lambda a2 (lambda a3 (expand ('+ a '1) (elem env_2 = (extend env_1 a a3))))))
        (step-... "expanding the body, no more extensions to" (step env_2))
        (step (lambda a2 (lambda a3 ('+ a3 '1)))))))))

(define (macro-bind-expand->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(2)]
                    [linebreaks '(#f)])
       (metafunction->pict expand)))))

(define (macro-app-expand->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(3)]
                    [linebreaks '(#f)])
       (metafunction->pict expand)))))

(provide lang->pict
         metas->pict
         
         macro-bind-expand->pict
         macro-app-expand->pict
         
         example->pict
        
         simple-parse-example
         shadow-parse-example
         simple-macro-example
         reftrans-macro-example
         hyg-macro-example)
