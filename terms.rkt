#lang racket
(require
 (for-syntax syntax/parse)
 (only-in "term.rkt" define-term use-terms))
(provide (all-defined-out))

;;;; ----------------------------------------
;;;; Language

(define-term Var        (nam))
(define-term Fun        (vars ast))
(define-term App        (lbl rator rands)) ; unique lbl is assigned at parse
(define-term If         (lbl tst thn els)) ; unique lbl is assigned at parse

;; Value
; abstract term
(define-term Val        ())
(define-term Atom Val   ())
(define-term List Val   ())

(define-term Prim Val   (nam stx))
(define-term VFun Val   (vars ast env))
;; LBind2 is used only in full
(define-term LBind2 Val (scps_p scps_u))

;; Literal values
(define-term Bool Atom  (b))
(define-term Num  Atom  (n))
(define-term Sym  Atom  (nam))
(define-term Null List  ())
(define-term Pair List  (a d))

;; Defs, 𝓁 is used only in full
(define-term Defs Atom  (scp 𝓁))

;(define-term 𝓁    Atom (nam))
(define 𝓁% (class* Atom% (equal<%>)
             (inspect #f)
             (init-field nam)
             (super-new)
             (define/public (equal-to? other recur)
               (eq? nam (get-field nam other)))
             (define/public (equal-hash-code-of hash-code)
               (eq-hash-code nam))
             (define/public (equal-secondary-hash-code-of hash-code)
               (eq-hash-code nam))))

;; Syntax objects (a subset of values)
(define-term Stx Atom  (e ctx))
(define-term Stxξ      (stx ξ))

;; Expand-time continuation
(define-term Hole      ())


;; Lst pattern/constructor
(define-match-expander Lst
  (λ (stx)
    (syntax-case stx (... ...)
      [(_ p (... ...))
       #'(? List? (app lst->list (list p (... ...))))]
      [p (syntax-parse #'p
           #:datum-literals [|.|]
           [(_) #'(Null)]
           [(_ p ps ...) #'(Pair p (Lst ps ...))]

           [(_ . x:id) #'(? List? x)]
           [(_ p ps ... . x:id) #'(Pair p (Lst ps ... . x))]

           [p (syntax-case #'p (... ...)
                [(_ p (... ...))
                 #'(? List? (app lst->list (list p (... ...))))])])]))
  (λ (stx) (syntax-parse stx
              [(_) #'(Null)]
              [(_ x xs ...) #'(Pair x (Lst xs ...))]

              [(_ . xs:id)  #'(and (List? xs) xs)]
              [(_ y ys ... . x:id)  #'(Pair y (Lst ys ... . x))])))


;;;; ----------------------------------------
;;;; Internal Configuration

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

;; Expand-time continuation
(define-term κ       (stx ex? 𝓁))

;; Expand-time state (configuration)
(define-term InEval  (state ξ))
(define-term ζ       (stx ex? κ Σ))


(define-syntax #%term-forms
  (append '((Var     nam)
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
            (Prim    nam stx)
            (Null)
            (Pair    a d)
            (Defs    scp 𝓁)
            (𝓁       nam)
            (Stx     e ctx)
            (Stxξ    stx ξ)
            (Hole))
          '((AstEnv  ast env)
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
            (Stxξ    stx ξ)
            (κ       stx ex? 𝓁)
            (InEval  state ξ)
            (ζ       stx ex? κ Σ))))


;;;; Extra utils

(use-terms Val Atom Sym List Null Pair Stx Stxξ Hole)

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

(define (lst->list/recur x)
  (match x
    [(Null) '()]
    [(Pair a d) (cons (lst->list/recur a) (lst->list/recur d))]
    [_ x]))


; snoc : ProperStl Stx -> ProperStl
(define (snoc stl stx)
  (cond
    [(Null? stl) (Pair stx (Null))]
    [(Pair? stl) (Pair (Pair-a stl) (snoc (Pair-d stl) stx))]
    [else (error "no such case")]))
  
;; Additional predicates
(define (id? x)
  (match x
    [(Stx (Sym _) _) #t]
    [_ #f]))

(define (prim? x)
  (or (member x '(syntax-e syntax->datum
                  datum->syntax + - * / < = eq?
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

;; syntax->datum (especially useful for displaying κ)

(define (stx->datum stx)
  (cond
    [(Hole? stx) (Sym '□)]
    [(Stxξ? stx) (stx->datum (Stxξ-stx stx))]
    [(Stx? stx) (let ([e (Stx-e stx)])
                  (cond
                    [(prim? e) (Sym e)]
                    [(Stx? e)  (stx->datum e)]
                    [(Atom? e) e]
                    [(Null? e) (Null)]
                    [(Pair? e) (Pair (stx->datum (Pair-a e))
                                     (stl->datum (Pair-d e)))]
                    [else e]))]
    [else stx]))

(define (stl->datum stl)
  (cond
    [(Hole? stl) (Sym '□)]
    [(Null? stl) (Null)]
    [(Stx? stl)  (stx->datum stl)]
    [(Pair? stl) (Pair (stx->datum (Pair-a stl))
                       (stl->datum (Pair-d stl)))]
    [else stl]))
