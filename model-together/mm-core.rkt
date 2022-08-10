#lang racket
(require redex
         "rewrites.rkt"
         slideshow/pict)

(define-language mini
  
  ;; Executable AST and values:
  [ast var (App ast ast ...) val]
  [var (Var nam)]
  [val (Fun var ast) (List val ...) atom]

  ;; Literal values:
  [atom sym prim desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number]
  [sym (Sym nam)]
  [prim CONS CAR CDR LIST desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim + -]

  ;; Use names for vars, addrs, and marks;
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
  δ : prim (val ...) -> val
  [(δ CONS (val_1 (List val_2 ...))) (List val_1 val_2 ...)]
  [(δ CAR ((List val_1 val_2 ...))) val_1]
  [(δ CDR ((List val_1 val_2 ...))) (List val_2 ...)]
  [(δ LIST (val ...)) (List val ...)])

(define-metafunction/extension δ mini
  δ/stx : prim (val ...) -> val
  [(δ/stx + (number_1 number_2)) (plus number_1 number_2)]
  [(δ/stx - (number_1 number_2)) (minus number_1 number_2)])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction mini
  eval : ast -> val
  [(eval (App (Fun var ast_body) ast_arg))
   (eval (subst ast_body var (eval ast_arg)))]
  [(eval (App prim ast_arg ...))
   (δ/stx prim ((eval ast_arg) ...))]
  [(eval (App ast_op ast_arg ...))
   (eval (App (eval ast_op) ast_arg ...))]
  [(eval val) val])

;; ----------------------------------------
;; Examples:

(test-equal (term (eval 0)) (term 0))
(test-equal (term (eval (App (Fun (Var x) (Var x)) 1))) (term 1))
(test-equal (term (eval (App (Fun (Var y) (Fun (Var x) (Var y))) 2))) (term (Fun (Var x) 2)))
(test-equal (term (eval (App (App (Fun (Var z) (Fun (Var x) (App (Var z) 3))) (Fun (Var x) (Var x))) 4))) (term 3))

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
      (language->pict mini #:nts '(ast val atom sym prim var nam))))))

(define (metas->pict)
  (with-rewrites
   (lambda ()
     (vl-append
      10
      (vl-append (metafunction->pict δ) (text "..." 'default (default-font-size)))))))

(define (new-grammar->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(ast val)))))

(define (δ->pict)
  (with-rewrites 
   (lambda ()
     (vl-append (metafunction->pict δ) (text "..." 'default (default-font-size))))))

(provide eval->pict
         lang->pict
         metas->pict
         δ->pict)
