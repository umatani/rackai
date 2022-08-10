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
  [ctx •
       (Mark ctx mrk)
       (Rename ctx id nam)]
       
  ;; Literal values:
  [atom desc-other-atom tprim] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number sym prim]
  [sym (Sym nam)]
  [prim desc-other-prim SE MKS] ; `desc-other-prim' typesets as "...."
  [desc-other-prim + - CONS CAR CDR LIST]
  [tprim LOCAL-VALUE LOCAL-EXPAND]

  ;; Expand-time environment:
  [env desc-env] ; `desc-env' typesets as prose
  [desc-env ((nam transform) ...)]
  [transform TFun TLet-Syntax TQuote
             (TVar id) val TStop]
  
  ;; Use names for vars, addrs, and marks
  [(nam mrk) desc-name] ; `desc-name' typesets as prose
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

;; New variant to support LOCAL-VALUE and LOCAL-EXPAND:
(define-metafunction mini
  eval : ast env mrk+gen -> val

  ;; standard eval
  [(eval (App (Fun var ast_body) ast_arg) env (Fst mrk gen))
   (eval (subst ast_body var (eval ast_arg env (Fst mrk (0 . gen)))) env [Fst mrk (1 . gen)])]
  [(eval (App prim ast_arg ...) env (Fst mrk gen))
   (δ prim ((eval ast_arg env (Fst mrk (number . gen))) ...))
   (where/hidden (number ...) (enumerate 1 ast_arg ...))]
  [(eval val env (Fst mrk gen)) val]

  ;; local value
  [(eval (App LOCAL-VALUE ast) env (Fst mrk gen))
   ;;(lookup env (resolve (mark (eval ast env (Fst mrk gen)) mrk)))
   (lookup env (resolve id_result))
   (where id_result (eval ast env (Fst mrk gen)))]

  ;; local expand
  [(eval (App LOCAL-EXPAND ast_expr ast_stops) env (Fst mrk gen))
   (mark (expand (mark stx mrk) (Fst env_stops (2 . gen))) mrk)
   (where stx (eval ast_expr env (Fst mrk (1 . gen))))
   (where (List id_stop ...) (eval ast_stops env (Fst mrk (0 . gen))))
   (where env_stops (extend* (nostops env) (((resolve id_stop) TStop) ...)))])

;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction mini
  ;; Adds or cancels a mark
  addremove : mrk (mrk ...) -> (mrk ...)
  [(addremove mrk_1 (mrk_1 mrk_2 ...)) (mrk_2 ...)]
  [(addremove mrk_1 (mrk_2 ...)) (mrk_1 mrk_2 ...)])

(define-metafunction mini
  ;; Extracts all marks in order, removing cancelling pairs
  marksof : id nam -> (mrk ...)
  [(marksof (Stx val •) nam) ()]
  [(marksof (Stx val (Mark ctx mrk)) nam)
   (addremove mrk (marksof (Stx val ctx) nam))]
  [(marksof (Stx val (Rename ctx id nam)) nam) ()]
  [(marksof (Stx val (Rename ctx id nam_2)) nam) (marksof (Stx val ctx) nam)])

(define-metafunction mini
  ;; Resolves an identifier to a name; this is the heart of
  ;;  the syntax-object support for lexical scope
  resolve : id -> nam
  [(resolve (Stx (Sym nam) •)) nam]
  [(resolve (Stx val (Mark ctx mrk)))
   (resolve (Stx val ctx))]
  [(resolve (Stx val (Rename ctx id_orig nam_new)))
   nam_new
   (where nam_1 (resolve id_orig))
   (where nam_1 (resolve (Stx val ctx)))
   (side-condition (term (same? (marksof id_orig nam_1) (marksof (Stx val ctx) nam_1))))]
  [(resolve (Stx val (Rename ctx id_orig nam)))
   (resolve (Stx val ctx))])

(define-metafunction mini
  rename : stx id nam -> stx
  ;; Simply pushes `Rename's down through a syntax object
  [(rename (Stx atom ctx) id nam) 
   (Stx atom (Rename ctx id nam))]
  [(rename (Stx (List stx ...) ctx) id nam) 
   (Stx (List (rename stx id nam) ...) (Rename ctx id nam))])

(define-metafunction mini
  mark : stx mrk -> stx
  ;; Simply pushes `Mark's down through a syntax object
  [(mark (Stx atom ctx) mrk) 
   (Stx atom (Mark ctx mrk))]
  [(mark (Stx (List stx ...) ctx) mrk) 
   (Stx (List (mark stx mrk) ...) (Mark ctx mrk))])

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
  expand : stx env+gen -> stx

  ;; lambda
  [(expand (Stx (List id_lam id_arg stx_body) ctx) [Fst env gen])
   (Stx (List id_lam id_new (expand stx_newbody [Fst env_new (0 . gen)])) ctx)
   (where TFun (lookup env (resolve id_lam)))
   (where nam_new (fresh-name id_arg gen))
   (where id_new (rename id_arg id_arg nam_new))
   (where stx_newbody (rename stx_body id_arg nam_new))
   (where env_new (extend env nam_new (TVar id_new)))]

  ;; quote & syntax
  [(expand (Stx (List id_quote stx) ctx) [Fst env gen])
   (Stx (List id_quote stx) ctx)
   (where TQuote (lookup env (resolve id_quote)))]
  
  ;; macro binding
  [(expand (Stx (List id_ls id_mac stx_rhs stx_body) ctx) [Fst env gen])
   (expand (rename stx_body id_mac nam_new) [Fst (extend env nam_new (eval (parse stx_rhs) env (Fst no-mrk (0 . gen)))) (1 . gen)])
   (where TLet-Syntax (lookup env (resolve id_ls)))
   (where nam_new (fresh-name id_mac gen))]

  ;; macro application
  [(expand stx_macapp [Fst env gen])
   (expand (mark (eval (App val (mark stx_macapp mrk_new)) env [Fst mrk_new (0 . gen)]) mrk_new) [Fst env (1 . gen)])
   (where (Stx (List id_mac stx_arg ...) ctx) stx_macapp)
   (where val (lookup env (resolve id_mac)))
   (where mrk_new (fresh-name (Stx (Sym mrk) •) gen))]

  ;; stops
  [(expand (Stx (List id_stop stx ...) ctx) [Fst env gen])
   (Stx (List id_stop stx ...) ctx)
   (where TStop (lookup env (resolve id_stop)))]

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
                                                    ('LIST))))))
      (x (q))))
  8)

(define-example local-expand-stop-example
  (let-syntax p (lambda s (quote 0))
    (let-syntax q (lambda s (syntax ('CAR '8)))
      (let-syntax x (lambda s 
                      ;; Used as (x (q)) => extracts '8 from ('CAR '8)
                      ('CAR ('CDR ('SE ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                                      ('LIST (syntax p)))))))
        (x (q)))))
  8)

(define-example nested-local-expand-example
  (let-syntax z (lambda s (syntax '0))
    (let-syntax a (lambda s
                    ;; When `b' forces `a', then `a'
                    ;; drops `z' form the stop list, so it
                    ;; should expand to 0
                    ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                   ('LIST)))
      (let-syntax b (lambda s
                      ('MKS
                       ('LIST
                        (syntax quote)
                        ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                       ('LIST (syntax z))))
                       s))
        ('LIST (b (z)) (b (a (z)))))))
  (App LIST (List (Sym z)) (List (Sym quote) 0)))

;; ----------------------------------------
;; Typesetting:

(define (lang->pict)
  (with-rewrites
   (lambda ()
     (ht-append
      20
      (vl-append
       10
       (language->pict mini #:nts '(ast val atom sym prim tprim))
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

(define (tprim->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(atom tprim)))))

(define (value-eval->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(3)]
                    [linebreaks '(#f)])
       (metafunction->pict eval)))))

(define (expand-eval->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(4)]
                    [linebreaks '(#f)])
       (metafunction->pict eval)))))

(define (stop-expand->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([metafunction-cases '(4)]
                    [linebreaks '(#f)])
       (metafunction->pict expand)))))

(define (nostops->pict)
  (with-rewrites
   (lambda ()
     (metafunction->pict nostops))))

(define (example->pict)
  (with-rewrites
   (lambda ()
     (let-syntax ([step (syntax-rules ()
                          [(_ e) (lw->pict mini (to-lw e))])])
       (append-steps
        (step (expand (let-syntax SPC public ....
                        (let-syntax SPC class .... (class (public '8))))
                      env_0))
        (step-... "evaluate transformer expression for" (step public))
        (step (expand (let-syntax SPC class .... (class (public '8)))
                      (elem env_1 = (extend env_0 public ....))))
        (step-... "evaluate transformer expression for" (step class))
        (step (expand (class (public '8)) (elem env_2 = (extend env_1 class ....))))
        (step-... "apply the" (step class) "transformer")
        (step (eval .... ('LOCAL-EXPAND (syntax (public '8))
                                        ('LIST (syntax public))) ....
                    env_2 mrk_1))
        (cons (blank) (explain "expansion stops immediately at" (step public) ":"))
        (step (eval .... (syntax (public '8)) .... env_2 mrk_1))
        (step-... "transformer strips the" (step public) "form away")
        (step (eval (syntax '8) env_2 mrk_1))
        (step (expand '8 env_2)))))))

;; Begin code extending expand-> to metafunction

(provide lang->pict
         metas->pict
         
         tprim->pict
         value-eval->pict
         expand-eval->pict
         stop-expand->pict
         nostops->pict
         example->pict)
