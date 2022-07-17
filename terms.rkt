#lang racket
(require
 (for-syntax syntax/parse)
 (only-in "term.rkt" define-term use-terms)
 (only-in "prim.rkt" prim?))
(provide terms^ terms@ #%term-forms
         use-lst-form
         terms-extra^ terms-extra@)

;;;; Language

(define-signature terms^
  (;;;; Language
   Var% Fun% App% If%
   ;; Value
   Val% Atom% List%
   VFun% LBind2%
   ;; Literal values
   Bool% Num% Sym% Prim% Null% Pair%
   ;; Syntax objects (a subset of values)
   Stx%
   ;; Defs is used only in full
   Defs%
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

  (define-term Var        (nam))
  (define-term Fun        (vars ast))
  (define-term App        (lbl rator rands)) ; unique lbl is assigned at parse
  (define-term If         (lbl tst thn els)) ; unique lbl is assigned at parse

  ;; Value
  ; abstract term
  (define-term Val        ())
  (define-term Atom Val   ())
  (define-term List Val   ())

  (define-term Prim Val  (nam))
  (define-term VFun Val   (vars ast env))
  ;; LBind2 is used only in full
  (define-term LBind2 Val (scps_p scps_u))

  ;; Literal values
  (define-term Bool Atom (b))
  (define-term Num  Atom (n))
  (define-term Sym  Atom (nam))
  (define-term Null List ())
  (define-term Pair List (a d))

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
    (List)
    (Bool    b)
    (Num     n)
    (Sym     nam)
    (Prim    nam)
    (Null)
    (Pair    a d)
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

;;;; Extra utils

;; Lst patter/constructor
(define-syntax-rule (use-lst-form Lst List? Null Pair lst->list)
  (... (define-match-expander Lst
         (λ (stx)
           (syntax-case stx (... ...)
             [(_ p (... ...)) #'(app lst->list (list p (... ...)))]
             [p (syntax-parse #'p
                  #:datum-literals [|.|]
                  [(_) #'(Null)]
                  [(_ p ps ...) #'(Pair p (Lst ps ...))]

                  [(_ . x:id) #'(? List? x)]
                  [(_ p ps ... . x:id) #'(Pair p (Lst ps ... . x))]

                  [p (syntax-case #'p (... ...)
                       [(_ p (... ...))
                        #'(app lst->list (list p (... ...)))])])]))
         (λ (stx) (syntax-parse stx
                     [(_) #'(Null)]
                     [(_ x xs ...) #'(Pair x (Lst xs ...))]

                     [(_ . xs:id)  #'(and (List? xs) xs)]
                     [(_ y ys ... . x:id)  #'(Pair y (Lst ys ... . x))])))))


(define-signature terms-extra^
  (id lst->list list->lst snoc stx? stl? proper-stl? id? val?))

(define-unit terms-extra@
  (import (only terms^
                Val% Atom% Sym% Null% Pair% Stx% Stxξ% Hole% 𝓁% Defs%
                VFun% LBind2% Store% KApp% KIf% AstEnv% SApp% SIf% SSeq%))
  (export terms-extra^)

  (use-terms Val Atom Sym Null Pair Stx Stxξ Hole 𝓁 Defs VFun LBind2
             Store KApp KIf AstEnv SApp SIf SSeq)

  ;; Additional constructor
  (define (id nam ctx) (Stx (Sym nam) ctx))

  ;; Additional matcher & List utils

  (define (lst->list l)
  (match l
    [(Null) '()]
    [(Pair a d) (cons a (lst->list d))]))

  (define (list->lst l)
    (match l
      ['() (Null)]
      [(cons a d) (Pair a (list->lst d))]))

  ; snoc : ProperStl Stx -> ProperStl
  (define (snoc stl stx)
    (cond
      [(Null? stl) (Pair stx (Null))]
      [(Pair? stl) (Pair (Pair-a stl) (snoc (Pair-d stl) stx))]
      [else (error "no such case")]))
  
  ;; Additional predicates
  (define (val? x)
    (or (Val? x)
        (and (Pair? x) (val? (Pair-a x)) (val? (Pair-d x)))
        (stx? x)))

  (define (stx? x)
    (or (and (Stx? x) (Atom? (Stx-e x)))
        (and (Stx? x) (prim? (Stx-e x)))
        (and (Stx? x) (Pair? (Stx-e x))
             (stx? (Pair-a (Stx-e x)))
             (stl? (Pair-d (Stx-e x))))
        (and (Stx? x) (proper-stl? (Stx-e x)))
        (Stxξ? x)
        (Hole? x)
        (and (Stx? x) (Hole? (Stx-e x)))))

  (define (stl? x)
    (or (Null? x) (stx? x)
        (and (Pair? x) (stx? (Pair-a x)) (stl? (Pair-d x)))
        (Hole? x)))

  (define (proper-stl? x)
    (or (Null? x)
        (and (Pair? x) (stx? (Pair-a x)) (proper-stl? (Pair-d x)))))

  (define (id? x)
    (match x
      [(Stx (Sym _) _) #t]
      [_ #f]))


  ;;;; not in use
  (define (nam? x) (symbol? x))
  (define (state? x)
    (match x
      [(list (? tm?) (? cont?) (? Store?)) #t]
      [_ #f]))
  (define (tm? x) (or (val? x) (ser? x)))
  (define (cont? x) (or (eq? x '•) (KApp? x) (KIf? x)))
  (define (ser? x) (or (AstEnv? x) (SApp? x) (SIf? x) (SSeq? x))))
