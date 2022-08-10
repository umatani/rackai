#lang racket
(require redex
         "rewrites.rkt"
         slideshow/pict)

(define-language mini
  
  ;; Executable AST and values:
  [ast var (App ast ast ...) val]
  [var (Var nam)]
  [val desc-other-val stx] ; `desc-other-val' typesets as "...."
  [desc-other-val (List val ...) atom (Fun var ast)]

  ;; Syntax objects (a subset of values):
  [stx (Stx atom ctx) (Stx (List stx ...) ctx)]
  [id (Stx sym ctx)]
  [ctx •]
       
  ;; Literal values:
  [atom desc-other-atom sym prim] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number]
  [sym (Sym nam)]
  [prim desc-other-prim SE MKS] ; `desc-other-prim' typesets as "...."
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
   (where nam2 (Var var_2))
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
;; Examples:

(test-equal (term (eval (App SE (Stx (Sym x) •)))) (term (Sym x)))
(test-equal (term (eval (App LIST (App MKS (Sym y) 
                                       (Stx (Sym x) •)))))
            (term (List (Stx (Sym y) •))))
(test-equal (term (eval (App CONS 
                             (App CAR (List (Sym z)))
                             (App LIST
                                  (App CDR (List (Sym y)))))))
            (term (List (Sym z) (List))))
 
;; ----------------------------------------
;; Typesetting:

(define (grammar->pict)
  (with-rewrites
   (lambda ()
     (language->pict mini #:nts '(val prim)))))

(define (representation->pict) 
  (with-rewrites
   (lambda ()
     (lw->pict mini (to-lw (Stx (List (Stx (Sym lambda) •) (Stx (Sym x) •) (Stx (Sym x) •)) •))))))

(provide grammar->pict
         representation->pict)
