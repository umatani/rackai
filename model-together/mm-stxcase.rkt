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
  [pre-ctx • (Rename ctx id nam)]
  [ctx • (Mark ctx mrk) (Rename ctx id nam)]
       
  ;; Literal values:
  [atom sym prim desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number]
  [sym (Sym nam)]
  [prim SE MKS desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim + - CONS CAR CDR LIST]

  ;; Expand-time environment:
  [env desc-env] ; `desc-env' typesets as prose
  [desc-env ((nam transform) ...)]
  [transform TFun TLet-Syntax TQuote
             (TVar id) val]
  
  ;; Use names for vars, addrs, and marks
  [(nam) desc-name] ; `desc-name' typesets as prose
  [mrk nam]
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
  ;; Adds or cancels a mark
  addremove : mrk (mrk ...) -> (mrk ...)
  [(addremove mrk_1 (mrk_1 mrk_2 ...)) (mrk_2 ...)]
  [(addremove mrk_1 (mrk_2 ...)) (mrk_1 mrk_2 ...)])

(define (original-marksof-pict)
  (define-metafunction mini
    ;; Extracts all marks in order, removing cancelling pairs
    marksof : id -> (mrk ...)
    [(marksof (Stx sym •)) ()]
    [(marksof (Stx sym (Mark ctx mrk)))
     (addremove mrk (mrk_2 ...))
     (where (mrk_2 ...) (marksof (Stx sym ctx)))]
    [(marksof (Stx sym (Rename ctx id_2 nam_2))) (marksof (Stx sym ctx))])
  (metafunction->pict marksof))

(define-metafunction mini
  ;; Extracts all marks in order, removing cancelling pairs
  marksof : id nam -> (mrk ...)
  [(marksof (Stx sym •) nam) ()]
  [(marksof (Stx sym (Mark ctx mrk)) nam)
   (addremove mrk (mrk_2 ...))
   (where (mrk_2 ...) (marksof (Stx sym ctx) nam))]
  [(marksof (Stx sym (Rename ctx id_2 nam)) nam) ()]
  [(marksof (Stx sym (Rename ctx id_2 nam_2)) nam) 
   (marksof (Stx sym ctx) nam)])

(define (original-resolve-pict)
  (define-metafunction mini
    ;; Resolves an identifier to a name; this is the heart of
    ;;  the syntax-object support for lexical scope
    resolve : id -> nam
    [(resolve (Stx (Sym nam) •)) nam]
    [(resolve (Stx (Sym nam) (Mark ctx mrk)))
     (resolve (Stx (Sym nam) ctx))]
    [(resolve (Stx (Sym nam) (Rename ctx id nam_new)))
     nam_new
     (where nam_1 (resolve id))
     (where nam_1 (resolve (Stx (Sym nam) ctx)))
     (side-condition (term (same? (marksof id) (marksof (Stx (Sym nam) ctx)))))]
    [(resolve (Stx (Sym nam) (Rename ctx id nam_2)))
     (resolve (Stx (Sym nam) ctx))])
  (metafunction->pict resolve))

(define-metafunction mini
  ;; Resolves an identifier to a name; this is the heart of
  ;;  the syntax-object support for lexical scope
  resolve : id -> nam
  [(resolve (Stx (Sym nam) •)) nam]
  [(resolve (Stx (Sym nam) (Mark ctx mrk)))
   (resolve (Stx (Sym nam) ctx))]
  [(resolve (Stx (Sym nam) (Rename ctx id nam_new)))
   nam_new
   (where nam_1 (resolve id))
   (where nam_1 (resolve (Stx (Sym nam) ctx)))
   (side-condition (term (same? (marksof id nam_1) (marksof (Stx (Sym nam) ctx) nam_1))))]
  [(resolve (Stx (Sym nam) (Rename ctx id nam_2)))
   (resolve (Stx (Sym nam) ctx))])

(define-metafunction mini
  rename : stx id nam -> stx
  ;; Simply pushes `Rename's down through a syntax object
  [(rename (Stx atom ctx) id nam) 
   (Stx atom (Rename ctx id nam))]
  [(rename (Stx (List stx ...) ctx) id nam) 
   (Stx (List (rename stx id nam) ...)
        (Rename ctx id nam))])

(define-metafunction mini
  mark : stx mrk -> stx
  ;; Simply pushes `Mark's down through a syntax object
  [(mark (Stx atom ctx) mrk) 
   (Stx atom (Mark ctx mrk))]
  [(mark (Stx (List stx ...) ctx) mrk) 
   (Stx (List (mark stx mrk) ...) 
        (Mark ctx mrk))])

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
  [(parse (Stx (List id_quote atom) ctx))
   atom
   (where quote (resolve id_quote))]
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
   (where id_new (rename id_arg id_arg nam_new))
   (where env_new (extend env nam_new (TVar id_new)))
   (where stx_expbody (expand (rename stx_body id_arg nam_new) [Fst env_new (0 . gen)]))]

  ;; quote & syntax
  [(expand (Stx (List id_quote stx) ctx) [Fst env gen])
   (Stx (List id_quote stx) ctx)
   (where TQuote (lookup env (resolve id_quote)))]
  
  ;; Macro creation:
  [(expand (Stx (List id_ls id stx_rhs stx_body) ctx) [Fst env gen])
   (expand (rename stx_body id nam_new) [Fst env_new (0 . gen)])
   (where TLet-Syntax (lookup env (resolve id_ls)))
   (where nam_new (fresh-name id gen))
   (where env_new (extend env nam_new (eval (parse stx_rhs))))]

  ;; Macro invocation:
  [(expand stx_macapp [Fst env gen])
   (expand (mark stx_exp mrk_new) [Fst env (0 . gen)])
   (where (Stx (List id_mac stx_arg ...) ctx) stx_macapp)
   (where val (lookup env (resolve id_mac)))
   (where mrk_new (fresh-name (Stx (Sym mrk) •) gen))
   (where stx_exp (eval (App val (mark stx_macapp mrk_new))))]
  
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

(define-example simple-macro-example
  (let-syntax x (lambda z (syntax (quote 2))) (x 1))
  2)

(define-example reftrans-macro-example
  (lambda z (let-syntax x (lambda s (syntax z)) (lambda z (x))))
  (Fun (Var z) (Fun (Var z:0:0) (Var z))))

(define-example hyg-macro-example
  (lambda z (let-syntax x (lambda s 
                            ('MKS
                             ('LIST (syntax lambda)
                                    (syntax z)
                                    ('CAR ('CDR ('SE s))))
                             (syntax here)))
              (x z)))
  (Fun (Var z) (Fun (Var z:0:0:0) (Var z))))

(define-example thunk-example
  (let-syntax 
      thunk (lambda e
              ('MKS
               ('LIST (syntax lambda) 
                      (syntax a) 
                      ('CAR ('CDR ('SE e))))
               e))
    (((lambda a (thunk ('+ a '1))) '5) '0))
  (App (App (Fun (Var a:0:0:0) (Fun (Var a:0:0:0:0:0) (App + (Var a:0:0:0) 1))) 5) 0))

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
       (language->pict mini #:nts '(stx id ctx))
       (language->pict mini #:nts '(env)))
      (vl-append
       10
       (language->pict mini #:nts '(transform))
       (language->pict mini #:nts '(nam mrk)))))))

(define (metas->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (vl-append (metafunction->pict δ/stx) (text "..." 'default (default-font-size)))
      (metafunction->pict parse)
      (metafunction->pict resolve)
      (metafunctions->pict mark rename)
      (metafunctions->pict marksof addremove)
      (metafunctions->pict strip)))))

(define (pre-ctx->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(pre-ctx)))))

(define (ctx->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(ctx mrk)))))

(define (resolve->pict)
  (with-rewrites
   (lambda ()
     (original-resolve-pict))))

(define (marksof->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (original-marksof-pict)
      (metafunctions->pict addremove)))))

(define (shortcut-resolve->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(2)])
       (vl-append
        (lw->pict mini (to-lw ...))
        (metafunction->pict resolve)
        (lw->pict mini (to-lw ...)))))))

(define (shortcut-marksof->pict)
  (with-rewrites
   (lambda ()
     (metafunctions->pict marksof))))

(define (expand->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      (parameterize ([metafunction-cases '(0)] ; lambda
                     [linebreaks '(#t)])
        (metafunction->pict expand))
      (parameterize ([metafunction-cases '(1)] ; quote
                     [linebreaks '(#f)])
        (metafunction->pict expand))
      (parameterize ([metafunction-cases '(2)] ; macro bind
                     [linebreaks '(#t)])
        (metafunction->pict expand))
      (parameterize ([metafunction-cases '(3)] ; macro app
                     [linebreaks '(#t)])
        (metafunction->pict expand))
      (parameterize ([metafunction-cases '(4)] ; app
                     [linebreaks '(#t)])
        (metafunction->pict expand))
      (parameterize ([metafunction-cases '(5)] ; ref
                     [linebreaks '(#f)])
        (metafunction->pict expand))))))

(define (lambda-expand->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      (parameterize ([metafunction-cases '(0)] ; lambda
                     [linebreaks '(#f)])
        (metafunction->pict expand))))))

(define (macro-bind-expand->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      (parameterize ([metafunction-cases '(2)] ; macro bind
                     [linebreaks '(#f)])
        (metafunction->pict expand))))))

(define (macro-app-expand->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      (parameterize ([metafunction-cases '(3)] ; macro app
                     [linebreaks '(#f)])
        (metafunction->pict expand))))))

(define (mark->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([linebreaks '(#f #f)])
       (metafunction->pict mark)))))

(define (rename->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([linebreaks '(#f #f)])
       (metafunction->pict rename)))))

(define (unmarked-lambda->pict)
  (with-rewrites
   (lambda ()
     (lw->pict mini (to-lw (Stx (Sym lambda) •))))))

(define (marked-lambda->pict)
  (with-rewrites
   (lambda ()
     (lw->pict mini (to-lw (Stx (Sym lambda) (Mark • mrk_2)))))))

(define (reftrans-example->pict)
  (with-rewrites
   (lambda ()
     (let-syntax ([step (syntax-rules ()
                          [(_ e) (lw->pict mini (to-lw e))])])
       (append-steps
        (step (expand (lambda a (thunk ('+ a '1))) env_0))
        (step (lambda a2 (expand (thunk ('+ a2 '1)) (elem env_1 = (extend env_0 a2 TVar)))))
        (step-... "calling the" (step thunk) "transformer")
        (step (lambda a2 (expand (lambda a ('+ a2 '1)) env_1)))
        (step (lambda a2 (lambda a3 (expand ('+ a2 '1) (extend env_1 a3 TVar)))))
        (step-... "expanding the body")
        (step (lambda a2 (lambda a3 ('+ a2 '1)))))))))

(define (reftrans-example2->pict)
  (with-rewrites
   (lambda ()
     (let-syntax ([step (syntax-rules ()
                          [(_ e) (lw->pict mini (to-lw e))])])
       (append-steps
        (step (expand (lambda a (thunk ('+ (Stx a •) '1))) env_0))
        (step (lambda (Stx a (Rename • a a2))
                (expand (thunk ('+ (Stx a (Rename • a a2)) '1)) 
                        (elem env_1 = (extend env_0 a2 TVar)))))
        (step-... "calling the" (step thunk) "transformer")
        (step (lambda (Stx a (Rename • a a2))
                (expand (lambda (Stx a •) ('+ (Stx a (Rename • a a2)) '1)) 
                        env_1)))
        (step (lambda (Stx a (Rename • a a2))
                (lambda (Stx a (Rename • a a3))
                  (expand ('+ (Stx a (Rename (Rename • a a2) a a3)) '1) 
                          (elem env_2 = (extend env_1 a3 TVar))))))
        (step-... "expanding the body")
        (step (lambda (Stx a (Rename • a a2))
                (lambda (Stx a (Rename • a a3))
                  ('+ (expand (Stx a (Rename (Rename • a a2) a a3)) env_2) '1))))
        (cons (blank) (explain "where the inner" (step Rename) "takes precedence in the reference to" (step a) ":"))
        (step (lambda (Stx a (Rename • a a2))
                (lambda (Stx a (Rename • a a3))
                  ('+ (Stx a (Rename • a a2)) '1))))
        (cons (blank) (explain "which parses as"))
        (cons (step =) (step (lambda a2 (lambda a3 ('+ a2 '1))))))))))

(define (hyg-example->picts)
  (with-rewrites
   (lambda ()
     (let-syntax ([step (syntax-rules ()
                          [(_ e) (lw->pict mini (to-lw e))])])
       (together
        (append-steps
         (step (expand (lambda a (let-syntax 
                                     thunk (lambda e .... (Stx a •) ....) 
                                     (thunk ('+ (Stx a •) '1)))) 
                       env_0))
         (step (lambda (Stx a (Rename • a a2))
                 (expand (let-syntax 
                             thunk (lambda e .... (Stx a (Rename • a a2)) ....) 
                             (thunk ('+ (Stx a (Rename • a a2)) '1)))
                         (elem env_1 = (extend env_0 a2 TVar)))))
         (step-... "evaluating the" (step thunk) "binding")
         (step (lambda (Stx a (Rename • a a2))
                 (expand (thunk ('+ (Stx a (Rename • a a2)) '1))
                         (elem env_2 = (extend env_1 thunk ....)))))
         (step-... "calling the" (step thunk) "transformer")
         (cons (blank) (explain "the macro-introduced" (step a) "has the marked context" (step ctx)))
         (step (lambda (Stx a (Rename • a a2))
                 (expand (lambda (Stx a ctx = (Mark (Rename • a a2) mrk_1))
                           ('+ (Stx a (Rename • a a2)) '1))
                         env_2)))
         (cons (blank) (explain "and the rename to" (step a3) "applies to" (step a) "with marked context" (step ctx)))
         (step (lambda (Stx a (Rename • a a2))
                 (lambda (Stx a (Rename ctx (Stx a ctx) a3))
                   (expand ('+ id_body '1) (elem env_3 = (extend env_2 a3 TVar)))))))
        (append-steps
         #:init values
         (cons (blank)
               (explain "where" (step id_body) "=" (step (Stx a (Rename (Rename • a a2) (Stx a ctx) a3)))))
         (step-... "expanding the body")
         (step (lambda (Stx a (Rename • a a2))
                 (lambda (Stx a (Rename ctx (Stx a ctx) a3))
                   ('+ (expand id_body env_3) '1))))
         (cons (blank) (explain "where the" (step a3) "renaming does not apply, since" (step id_body) "is not marked"))
         (step (lambda (Stx a (Rename • a a2))
                 (lambda (Stx a (Rename ctx (Stx a ctx) a3))
                   ('+ (Stx a (Rename • a a2)) '1))))))))))

(provide lang->pict
         metas->pict
         
         pre-ctx->pict
         ctx->pict
         resolve->pict
         marksof->pict
         shortcut-resolve->pict
         shortcut-marksof->pict
         lambda-expand->pict
         macro-bind-expand->pict
         macro-app-expand->pict
         mark->pict
         rename->pict
         unmarked-lambda->pict
         marked-lambda->pict
         reftrans-example->pict
         reftrans-example2->pict
         hyg-example->picts)

