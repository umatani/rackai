#lang racket
(require "term.rkt")
(provide (all-defined-out))

;;;; Language

(define-signature terms^
  (;;;; Language
   Var% Fun% App% If%
   ;; Value
   Val% Atom% VFun% LBind2%
   ;; Literal values
   Bool% Num% Sym%
   ;; Defs is used only in full
   Defs%
   ;; Syntax objects (a subset of values)
   Stx%
   ;; Eval-time continuation, environment, and store
   AstEnv% Store% KApp% KIf% SApp% SIf%
   ;; SSeq is used only in full
   SSeq%
   ;; Expand-time environment
   TVar% TStop%
   ;; Expand-time store
   Σ% StoBind% 𝓁%
   ;; Expand-time continuation
   Stxξ% Hole% κ%
   ;; Expand-time state (configuration)
   InEval% ζ%))

(define-unit terms@
  (import) (export terms^)

  (define-term Var     (nam))
  (define-term Fun     (vars ast))
  (define-term App     (lbl rator rands)) ; unique lbl is assigned at parse
  (define-term If      (lbl tst thn els)) ; unique lbl is assigned at parse

  ;; Value
  ; abstract term
  (define-term Val ())
  (define-term Atom Val ())

  (define-term VFun Val (vars ast env))
  ;; LBind2 is used only in full
  (define-term LBind2 Val (scps_p scps_u))

  ;; Literal values
  (define-term Bool Atom (b))
  (define-term Num  Atom (n))
  (define-term Sym  Atom (nam))

  ;; Defs is used only in full
  (define-term Defs Atom (scp 𝓁))

  ;; Syntax objects (a subset of values)
  (define-term Stx     (e ctx))

  ;; Eval-time continuation, environment, and store
  (define-term AstEnv  (ast env))
  (define-term Store   (size tbl))
  (define-term KApp    (lbl vals tms loc))
  (define-term KIf     (lbl thn els loc))
  (define-term SApp    (lbl vals tms))
  (define-term SIf     (lbl tst thn els))
  ;; SSeq is used only in full
  (define-term SSeq    (tms))

  ;; Expand-time environment
  (define-term TVar    (id))
  (define-term TStop   (all-transform))

  ;; Expand-time store
  (define-term Σ       (size tbl))
  (define-term StoBind (scps nam))
  (define-term 𝓁 Atom  (nam))

  ;; Expand-time continuation
  (define-term Stxξ    (stx ξ))
  (define-term Hole    ())
  (define-term κ       (stx ex? 𝓁))

  ;; Expand-time state (configuration)
  (define-term InEval  (state ξ))
  (define-term ζ       (stx ex? κ Σ)))

(define-syntax #%term-forms
  '((Var     nam)
    (Fun     vars ast)
    (App     lbl rator rands)
    (If      lbl tst thn els)
    (VFun    vars ast env)
    (LBind2  scps_p scps_u)
    (Val)
    (Atom)
    (Bool    b)
    (Num     n)
    (Sym     nam)
    (Defs    scp 𝓁)
    (Stx     e ctx)
    (AstEnv  ast env)
    (Store   size tbl)
    (KApp    lbl vals tms loc)
    (KIf     lbl thn els loc)
    (SApp    lbl vals tms)
    (SIf     lbl tst thn els)
    (SSeq    tms)
    (TVar    id)
    (TStop   all-transform)
    (Σ       size tbl)
    (StoBind scps nam)
    (𝓁       nam)
    (Stxξ    stx ξ)
    (Hole)
    (κ       stx ex? 𝓁)
    (InEval  state ξ)
    (ζ       stx ex? κ Σ)))

;;;; Extra predicates

(define-signature terms-extra^
  (id stx? stl? proper-stl? id? atom? prim?
   stx-prim? val? nam? state? tm? cont? ser?))

(define-unit terms-extra@
  (import (only terms^
                Val% Atom% Sym% Stx% Stxξ% Hole% 𝓁% Defs% VFun% LBind2% Store%
                KApp% KIf% AstEnv% SApp% SIf% SSeq%))
  (export terms-extra^)

  (use-terms Val Atom Sym Stx Stxξ Hole 𝓁 Defs VFun LBind2 Store KApp KIf
             AstEnv SApp SIf SSeq)

  (define (id nam ctx) (Stx (Sym nam) ctx))

  ;; Additional predicates
  (define (val? x)
    (or (Val? x)
        (atom? x)
        (and (pair? x) (val? (car x)) (val? (cdr x)))
        (stx? x)))

  (define (stx? x)
    (or (and (Stx? x) (atom? (Stx-e x)))
        (and (Stx? x) (pair? (Stx-e x))
             (stx? (car (Stx-e x)))
             (stl? (cdr (Stx-e x))))
        (and (Stx? x) (proper-stl? (Stx-e x)))
        (Stxξ? x)
        (Hole? x)
        (and (Stx? x) (Hole? (Stx-e x)))))

  (define (stl? x)
    (or (null? x)
        (stx? x)
        (and (pair? x) (stx? (car x)) (stl? (cdr x)))
        (Hole? x)))

  (define (proper-stl? x)
    (or (null? x)
        (and (pair? x) (stx? (car x)) (proper-stl? (cdr x)))))

  (define (id? x) (and (Stx? x) (Sym? (Stx-e x))))

  (define (atom? x)
    (or (Atom? x)
        (null? x)
        (prim? x)))

  (define (prim? x)
    (or (member x '(syntax-e datum->syntax + - * / < = eq?
                             cons car cdr list second third fourth
                             printe ;; for debug
                             ))
        (stx-prim? x)))

  (define (stx-prim? x)
    (member x '(syntax-local-value local-expand
                                   syntax-local-identifier-as-binding
                                   box unbox set-box!
                                   syntax-local-make-definition-context
                                   syntax-local-bind-syntaxes)))

  (define (nam? x) (symbol? x))

  (define (state? x)
    (and (list? x) (= (length x) 3)
         (tm? (first x))
         (cont? (second x))
         (Store? (third x))))

  (define (tm? x) (or (val? x) (ser? x)))

  (define (cont? x) (or (eq? x '•) (KApp? x) (KIf? x)))

  (define (ser? x) (or (AstEnv? x) (SApp? x) (SIf? x) (SSeq? x))))
