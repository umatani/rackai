#lang racket
(require redex
         "rewrites.rkt"
         (except-in slideshow/pict explain))

(define-language mini
  
  ;; Executable AST and values:
  [ast var (App ast ast ...) val]
  [var (Var nam)]
  [val desc-other-val (Fun var ast)]
  [desc-other-val atom (List val ...) stx]

  ;; Syntax objects (a subset of values):
  [stx (Stx atom ctx) (Stx (List stx ...) ctx)]
  [id (Stx sym ctx)]
  [ctx •]
       
  ;; Literal values:
  [atom sym prim desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number]
  [sym (Sym nam)]
  [prim SE MKS desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim + - CONS CAR CDR LIST]
  
  ;; Use names for vars, addrs, and marks
  [(nam) desc-name] ; `desc-name' typesets as prose
  [desc-name variable-not-otherwise-mentioned])

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
   (where nam_2 (Var var_2))
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

(define make-simple-parse-pict
  (lambda ()
    (define-metafunction mini
      parse : stx -> ast
      [(parse (Stx (List (Stx (Sym lambda) •) (Stx (Sym nam) •) stx) •))
       (Fun (Var nam) (parse stx))]
      [(parse (Stx (List (Stx (Sym quote) •) stx) •))
       (strip stx)]
      [(parse (Stx (List (Stx (Sym syntax) •) stx) •))
       stx]
      [(parse (Stx (List stx_rator stx_rand ...) •))
       (App (parse stx_rator) (parse stx_rand) ...)]
      [(parse (Stx nam •))
       (Var nam)])
    (metafunction->pict parse)))

;; ----------------------------------------
;; Helpers for writing examples:

(define-metafunction mini
  as-syntax : any -> val
  [(as-syntax nam) (Stx (Sym nam) •)]
  [(as-syntax number) (Stx number •)]
  [(as-syntax prim) (Stx prim •)]
  [(as-syntax tprim) (Stx tprim •)]
  [(as-syntax (any ...)) (Stx (List (as-syntax any) ...) •)])

;; ----------------------------------------
;; Examples:

(test-equal (term (parse (Stx (List (Stx (Sym lambda) •)
                                    (Stx (Sym x) •)
                                    (Stx (Sym x) •))
                              •)))
            (term (Fun (Var x) (Var x))))
(test-equal (term (parse (as-syntax (lambda x x))))
            (term (Fun (Var x) (Var x))))
(test-equal (term (parse (as-syntax (lambda x ('LIST x 
                                                     (syntax y) 
                                                     '5)))))
            (term (Fun (Var x) (App LIST (Var x) (Stx (Sym y) •) 5))))
 
;; ----------------------------------------
;; Typesetting:

(define (eval->pict)
  (with-rewrites
   (lambda () 
     (metafunction->pict eval))))

(define (lang->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (language->pict mini #:nts '(ast val atom sym prim))
      (language->pict mini #:nts '(stx id ctx))
      (language->pict mini #:nts '(E))))))

(define (metas->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (vl-append (metafunction->pict δ/stx) (text "..." 'default (default-font-size)))
      (metafunction->pict parse)
      (metafunction->pict resolve)
      (metafunctions->pict strip)))))

(define (new-grammar->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(ast val)))))

(define (stx-grammar->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(stx id ctx)))))

(define (parse->pict)
  (with-rewrites
   (lambda ()
     (parameterize ([linebreaks '(#f #f #f #f #f)])
       (metafunction->pict parse)))))

(define (simple-parse->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      (parameterize ([metafunction-cases '(0)]
                     [linebreaks '(#f)])
        (make-simple-parse-pict))
      (parameterize ([metafunction-cases '(1 2 3 4)]
                     [linebreaks '(#f #f #f #f)])
        (make-simple-parse-pict))))))

(define (δ->pict)
  (with-rewrites 
   (lambda ()
     (vl-append (metafunction->pict δ/stx) (text "..." 'default (default-font-size))))))

(define (strip->pict)
  (with-rewrites
   (lambda ()
     (metafunctions->pict strip))))

(define (resolve->pict)
  (with-rewrites
   (lambda ()
     (metafunction->pict resolve))))

(define (example->pict)
  (with-rewrites
   (lambda ()
     (let-syntax ([step (syntax-rules ()
                          [(_ e) (lw->pict mini (to-lw e))])])
       (append-steps
        (step (eval (App SE (App MKS (Sym x) (Stx (Sym y) •)))))
        (step (δ SE ((eval (App MKS (Sym x) (Stx (Sym y) •))))))
        (step (δ SE ((δ MKS ((Sym x) (Stx (Sym y) •))))))
        (step (δ SE ((Stx (Sym x) •))))
        (step (Sym x)))))))

(define (representation->pict) 
  (with-rewrites
   (lambda ()
     (lw->pict 
      mini 
      (to-lw
       (Stx (List (Stx (Sym lambda) •)
                  (Stx (Sym x) •)
                  (Stx (List (Stx (List (Stx (Sym quote) •) 
                                        (Stx + •)) •)
                             (Stx (Sym x) •)
                             (List (Stx (Sym quote) •) 
                                   (Stx 1 •)))
                       •))
            •))))))

(define (parsed->pict) 
  (with-rewrites
   (lambda ()
     (lw->pict mini (to-lw (Fun (Var x) (App + (Var x) 1)))))))

(define (list-of-sym->pict) 
  (with-rewrites
   (lambda ()
     (lw->pict mini (to-lw (List (Sym lambda) (List (Sym x)) (Sym y)))))))

(define (syntax-literal->pict) 
  (with-rewrites
   (lambda ()
     (lw->pict mini (to-lw (Stx (List (Stx (Sym lambda) •) (Stx (List (Stx (Sym x) •)) •) (Stx (Sym y) •)) •))))))

(provide eval->pict
         lang->pict
         metas->pict
         
         new-grammar->pict
         stx-grammar->pict
         δ->pict
         simple-parse->pict
         parse->pict
         strip->pict
         resolve->pict
         example->pict
         representation->pict
         parsed->pict
         list-of-sym->pict
         syntax-literal->pict)
