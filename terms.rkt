#lang racket/base
(require
 (for-syntax racket/base syntax/parse)
 (only-in racket/match match define-match-expander)
 racket/class
 (only-in "term.rkt" define-term use-terms))
(provide (all-defined-out))

;;;; ----------------------------------------
;;;; Language Constructs

(define-term Var        (nam))
(define-term Fun        (vars ast))
(define-term App        (lbl rator rands)) ; unique lbl is assigned at parse
(define-term If         (lbl tst thn els)) ; unique lbl is assigned at parse

;; Value
; abstract super terms
(define-term Val        ())
(define-term Atom Val   ())
(define-term List Val   ())

(define-term Bool Atom  (b))
(define-term Num  Atom  (n))
(define-term Sym  Atom  (nam))
(define-term Stx  Atom  (e ctx))        ;; Syntax objects
(define-term VFun Atom  (vars ast env)) ;; lambda
(define-term Prim Atom  (nam stx))      ;; primitive functions
                                        ;;   stx is used for alloc-box
                                        ;;   and alloc-def-ξ

(define-term Null List  ())
(define-term Pair List  (a d))

;;;; ----------------------------------------
;;;; Internal Configurations

;; Eval-time continuation, environment, and store
(define-term AstEnv     (ast env))
(define-term Store      (size tbl))
(define-term Cont       ())
(define-term KApp  Cont (vals tms env loc))  ;; (v ... □ t ...)
(define-term KApp′ Cont (vals     env loc))  ;; (v ...)
(define-term KIf   Cont (thn els env loc))   ;; (if □ t t)

;; Expand-time environment
(define-term TVar       (id))
(define-term TStop      (all-transform))

;; Expand-time store
(define-term Σ          (size tbl))
(define-term StoBind    (scps nam))

;; Expand-time continuation
(define-term κ          (stxξ 𝓁))
(define-term Hole       ())

;; Expand-time state (configuration)
(define-term InEval     (state ζ))
(define-term ζ          (stxξ κ Σ))
(define-term Stxξ       (stx ξ))

(define 𝓁% (class* Atom% (equal<%>) ;(define-term 𝓁    Atom (nam))
             (inspect #f)
             (init-field nam)
             (super-new)
             (define/public (equal-to? other recur)
               (eq? nam (get-field nam other)))
             (define/public (equal-hash-code-of hash-code)
               (eq-hash-code nam))
             (define/public (equal-secondary-hash-code-of hash-code)
               (eq-hash-code nam))))

;; for compact use-term(s)
(define-syntax #%term-forms
  (append '((Var     nam)
            (Fun     vars ast)
            (App     lbl rator rands)
            (If      lbl tst thn els)
            (VFun    vars ast env)
            (Val)
            (Atom)
            (List)
            (Bool    b)
            (Num     n)
            (Sym     nam)
            (Prim    nam stx)
            (Null)
            (Pair    a d)
            (Stx     e ctx))
          '((AstEnv  ast env)
            (Store   size tbl)
            (Cont)
            (KApp    vals tms env loc)
            (KApp′   vals     env loc)
            (KIf     thn els env loc)
            (TVar    id)
            (TStop   all-transform)
            (Σ       size tbl)
            (StoBind scps nam)
            (κ       stxξ 𝓁)
            (Hole)
            (InEval  state ζ)
            (ζ       stxξ κ Σ)
            (Stxξ    stx ξ)
            (𝓁       nam)
            )))

(use-terms Var Fun App If VFun Val Atom List Bool Num Sym Prim Null
           Pair 𝓁 Stx Hole AstEnv Store Cont KApp KApp′ KIf
           TVar TStop Σ StoBind κ InEval ζ Stxξ)


;;;; Extra utils

;; Lst pattern/constructor
(define-match-expander Lst
  (λ (stx)
    (syntax-case stx (... ...)
      [(_ p (... ...))
       #'(? List? (app lst→list (list p (... ...))))]
      [p (syntax-parse #'p
           #:datum-literals [|.|]
           [(_) #'(Null)]
           [(_ p ps ...) #'(Pair p (Lst ps ...))]

           [(_ . xs:id) #'(? List? xs)]
           [(_ p ps ... . xs:id) #'(Pair p (Lst ps ... . xs))])]))
  (λ (stx)
    (syntax-parse stx
      [(_) #'(Null)]
      [(_ x xs ...) #'(Pair x (Lst xs ...))]

      [(_ . xs:id)  #'(and (List? xs) xs)]
      [(_ y ys ... . xs:id)  #'(Pair y (Lst ys ... . xs))])))

;; List utils

(define (lst→list l)
  (match l
    [(Null) '()]
    [(Pair a d) (cons a (lst→list d))]))

(define (list→lst l)
  (match l
    ['() (Null)]
    [(cons a d) (Pair a (list→lst d))]))

(define (lst→list/recur x)
  (match x
    [(Null) '()]
    [(Pair a d) (cons (lst→list/recur a) (lst→list/recur d))]
    [_ x]))

;; Additional constructor
(define (id nam ctx) (Stx (Sym nam) ctx))

;; Additional predicates
(define (id? x)
  (match x
    [(Stx (Sym _) _) #t]
    [_ #f]))

(define (cont? x)
  (or (Cont? x) (eq? x '●)))

(define (prim? x)
  (or (member x '(syntax-e
                  syntax->datum
                  datum->syntax + - * / < = eq?
                  cons car cdr list second third fourth
                  printe ;; for debug
                  ))
      (stx-prim? x)))

(define (stx-prim? x)
  (member x '(syntax-local-value 
              local-expand
              syntax-local-identifier-as-binding
              box unbox set-box!
              syntax-local-make-definition-context
              syntax-local-bind-syntaxes
              syntax-local-bind-syntaxes2 ;; used internally
              )))
