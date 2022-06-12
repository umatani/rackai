#lang racket/unit
(require racket
         "../../../struct-sig.rkt")

(import)
(export struct^)

;;;; Language

(define-struct Var (nam)  #:transparent #:constructor-name var)
(struct Fun (vars ast)    #:transparent #:constructor-name fun)
(struct App (rator rands) #:transparent #:constructor-name app)
(struct If  (tst thn els) #:transparent #:constructor-name iif)

;; Value
(struct VFun (vars ast env) #:transparent #:constructor-name vfun)
;; LBind2 is used only in full
(struct LBind2 (scps_p scps_u) #:transparent #:constructor-name lbind2)

;; Literal values
(struct Sym (nam) #:transparent #:constructor-name sym)

;; Defs is used only in full
(struct Defs (scp 𝓁) #:transparent #:constructor-name defs)

;; Syntax objects (a subset of values)
(struct Stx (e ctx) #:transparent #:constructor-name stx)
(define (id nam ctx) (stx (sym nam) ctx))

;; Eval-time continuation, environment, and store
(struct AstEnv (ast env)    #:transparent #:constructor-name ast&env)
(struct Store (size tbl)    #:transparent #:constructor-name store)
(struct KApp (vals tms loc) #:transparent #:constructor-name kapp)
(struct KIf (thn els loc)   #:transparent #:constructor-name kif)
(struct SApp (vals tms)     #:transparent #:constructor-name sapp)
(struct SIf (tst thn els)   #:transparent #:constructor-name sif)
;; SSeq is used only in full
(struct SSeq (tms)          #:transparent #:constructor-name sseq)

;; Expand-time environment
(struct TVar (id)             #:transparent #:constructor-name tvar)
(struct TStop (all-transform) #:transparent #:constructor-name tstop)

;; Expand-time store
(struct Σ (size tbl)       #:transparent #:constructor-name mk-Σ)
(struct StoBind (scps nam) #:transparent #:constructor-name stobind)
(struct Θ (size tbl)       #:transparent #:constructor-name mk-Θ)
(struct 𝓁 (nam)            #:transparent #:constructor-name mk-𝓁)

;; Expand-time continuation
(struct Stxξ (stx ξ)  #:transparent #:constructor-name stx&ξ)
(struct Hole ()       #:transparent #:constructor-name hole)
(struct κ (stx ex? 𝓁) #:transparent #:constructor-name mk-κ)

;; Expand-time state (configuration)
(struct InEval (state ξ)  #:transparent #:constructor-name in-eval)
(struct ζ (stx ex? κ Θ Σ) #:transparent #:constructor-name mk-ζ)

;; Additional predicates
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
  (or (null? x) (and (pair? x) (stx? (car x)) (proper-stl? (cdr x)))))

(define (id? x) (and (Stx? x) (Sym? (Stx-e x))))

(define (atom? x)
  (or (null? x) (boolean? x) (real? x) (Sym? x) (prim? x)
      (𝓁? x) (Defs? x) ;; used only in full
      ))

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

(define (val? x)
  (or (VFun? x)
      (atom? x)
      (and (pair? x) (val? (car x)) (val? (cdr x)))
      (stx? x)
      (LBind2? x)))

(define (nam? x) (symbol? x))

(define (state? x)
  (and (list? x)
       (= (length x) 3)
       (tm? (first x))
       (cont? (second x))
       (Store? (third x))))

(define (tm? x) (or (val? x) (ser? x)))

(define (cont? x) (or (eq? x '•) (KApp? x) (KIf? x)))

(define (ser? x) (or (AstEnv? x) (SApp? x) (SIf? x) (SSeq? x)))
