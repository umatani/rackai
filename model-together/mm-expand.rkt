#lang racket
(require redex
         "rewrites.rkt"
         "define-example.rkt"
         slideshow/pict)

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
  [transform TFun TQuote (TVar id)]
  
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
   (Stx (List id_lam id_new stx_expbody) ctx)
   (where TFun (lookup env (resolve id_lam)))
   (where nam_new (fresh-name id_arg gen))
   (where id_new (Stx (Sym nam_new) •))
   (where env_new (extend env (resolve id_arg) (TVar id_new)))
   (where stx_expbody (expand stx_body [Fst env_new (0 . gen)]))]

  ;; quote & syntax
  [(expand (Stx (List id_quote stx) ctx) [Fst env gen])
   (Stx (List id_quote stx) ctx)
   (where TQuote (lookup env (resolve id_quote)))]
  
  ;; application
  [(expand (Stx (List stx_rator stx_rand ...) ctx) [Fst env gen])
   (Stx (List stx_exprator stx_exprand ...) ctx)
   (where/hidden (number ...) (enumerate 1 stx_rand ...))
   (where stx_exprator (expand stx_rator [Fst env (0 . gen)]))
   (where (Plain stx_exprand ...) (Plain (expand stx_rand [Fst env (number . gen)]) ...))]
  
  ;; reference
  [(expand id [Fst env gen])
   id_new
   (where (TVar id_new) (lookup env (resolve id)))])

;; ----------------------------------------
;; Helpers for writing examples:

(define-metafunction mini
  preamble-env : -> env
  [(preamble-env) ((lambda TFun)
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

(define-example another-example
  ((lambda x (x x))
   (lambda x ((lambda y y) x)))
  (App (Fun (Var x:0) (App (Var x:0) (Var x:0)) )
       (Fun (Var x:1) (App (Fun (Var y:1:0:0) (Var y:1:0:0)) (Var x:1)))))

;; ----------------------------------------
;; Typesetting:

(define (lang->pict)
  (with-rewrites
   (lambda ()
     (ht-append
      20
      (vl-append
       10
       (language->pict mini #:nts '(ast val atom sym prim))
       (language->pict mini #:nts '(stx id ctx)))
      (vl-append
       10
       (language->pict mini #:nts '(transform))
       (vl-append
        (language->pict mini #:nts '(env))
        (language->pict mini #:nts '(var))))))))

(define (metas->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (vl-append (metafunction->pict δ/stx) (text "..." 'default (default-font-size)))
      (metafunction->pict parse)
      (metafunction->pict resolve)
      (metafunctions->pict strip)))))

(define (env->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      (language->pict mini #:nts '(env))
      (language->pict mini #:nts '(transform))))))

(define (expand->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      (parameterize ([metafunction-cases '(0 1 2 3)]
                     [linebreaks '(#f #f #f #f)])
        (metafunction->pict expand))))))

(define (example->pict)
  (with-rewrites
   (lambda ()
     (append-steps
      (lw->pict mini (to-lw (expand (lambda lambda lambda) env_0)))
      (lw->pict mini (to-lw (lambda lambda2 (expand lambda (extend env_0 lambda lambda2)))))
      (lw->pict mini (to-lw (lambda lambda2 lambda2)))))))

(provide lang->pict
         metas->pict
         
         env->pict
         expand->pict
         example->pict)
