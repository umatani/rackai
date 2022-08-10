#lang racket
(require
 (for-syntax syntax/parse)
 (only-in "term.rkt" define-term use-terms))
(provide (all-defined-out))

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

(define-term Prim Val  (nam stx))
(define-term VFun Val   (vars ast env))
;; LBind2 is used only in full
(define-term LBind2 Val (scps_p scps_u))

;; Literal values
(define-term Bool Atom (b))
(define-term Num  Atom (n))
(define-term Sym  Atom (nam))
(define-term Null List ())
(define-term Pair List (a d))

;; Defs, 𝓁 is used only in full
(define-term Defs Atom (scp 𝓁))

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
(define-term Stx       (e ctx))

;; Expand-time continuation
(define-term Hole      ())


(module+ test
  (use-terms Var Fun App If Val Atom List Prim VFun LBind2
             Bool Num Sym Null Pair Defs 𝓁 Stx Hole)
)

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
    (Prim    nam stx)
    (Null)
    (Pair    a d)
    (Defs    scp 𝓁)
    (𝓁       nam)
    (Stx     e ctx)
    (Hole)))

;;;; Extra utils

;; Lst patter/constructor
(define-syntax-rule (use-lst-form Lst List? Null Pair lst->list)
  (... (define-match-expander Lst
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
                     [(_ y ys ... . x:id)  #'(Pair y (Lst ys ... . x))])))))


(use-terms Val Atom Sym List Null Pair Stx Hole)

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
(define (id? x)
  (match x
    [(Stx (Sym _) _) #t]
    [_ #f]))

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

