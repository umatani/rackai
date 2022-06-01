#lang racket
(provide (all-defined-out))

;;;; Language

(struct Var (nam)         #:transparent)
(struct Fun (vars ast)    #:transparent)
(struct App (rator rands) #:transparent)
(struct If  (tst thn els) #:transparent)

;; Value
(struct VFun (vars ast env)    #:transparent)
;; LBind2 is used only in full
(struct LBind2 (scps_p scps_u) #:transparent)

;; Literal values
(struct Sym (nam) #:transparent)

;; Defs is used only in full
(struct Defs (scp 𝓁) #:transparent)

;; Syntax objects (a subset of values)
(struct GenStx (e ctx) #:transparent)
(define (Id nam ctx) (GenStx (Sym nam) ctx))

;; Eval-time continuation, environment, and store
(struct AstEnv (ast env) #:transparent)
(struct Store (size tbl) #:transparent)
(struct KApp (vals tms loc) #:transparent)
(struct KIf (thn els loc) #:transparent)
(struct SApp (vals tms) #:transparent)
(struct SIf (tst thn els) #:transparent)
;; SSeq is used only in full
(struct SSeq (tms) #:transparent)

;; Expand-time environment
(struct TVar (id) #:transparent)
(struct TStop (all-transform) #:transparent)

;; Expand-time store
(struct Σ (size tbl) #:transparent)
(struct StoBind (scps nam) #:transparent)
(struct Θ (size tbl) #:transparent)
(struct 𝓁 (nam) #:transparent)

;; Expand-time continuation
(struct Stxξ (stx ξ) #:transparent)
(struct Hole () #:transparent)
(struct κ (stx ex? 𝓁) #:transparent)

;; Expand-time state (configuration)
(struct InEval (state ξ) #:transparent)
(struct ζ (stx ex? κ Θ Σ) #:transparent)

;; Additional predicates
(define (Stx? x)
  (or (and (GenStx? x) (Atom? (GenStx-e x)))
      (and (GenStx? x) (pair? (GenStx-e x))
           (Stx? (car (GenStx-e x)))
           (Stl? (cdr (GenStx-e x))))
      (and (GenStx? x) (ProperStl? (GenStx-e x)))
      (Stxξ? x)
      (Hole? x)
      (and (GenStx? x) (Hole? (GenStx-e x)))))

(define (Stl? x)
  (or (null? x)
      (Stx? x)
      (and (pair? x) (Stx? (car x)) (Stl? (cdr x)))
      (Hole? x)))

(define (ProperStl? x)
  (or (null? x) (and (pair? x) (Stx? (car x)) (ProperStl? (cdr x)))))

(define (Id? x) (and (GenStx? x) (Sym? (GenStx-e x))))

(define (Atom? x)
  (or (null? x) (boolean? x) (real? x) (Sym? x) (Prim? x)
      (𝓁? x) (Defs? x) ;; used only in full
      ))

(define (Prim? x)
  (or (member x '(syntax-e datum->syntax + - * / < = eq?
                           cons car cdr list second third fourth
                           printe ;; for debug
                           ))
      (StxPrim? x)))

(define (StxPrim? x)
  (member x '(syntax-local-value local-expand
                                 syntax-local-identifier-as-binding
                                 box unbox set-box!
                                 syntax-local-make-definition-context
                                 syntax-local-bind-syntaxes)))

(define (Val? x)
  (or (VFun? x)
      (Atom? x)
      (and (pair? x) (Val? (car x)) (Val? (cdr x)))
      (Stx? x)
      (LBind2? x)))

(define (Nam? x) (symbol? x))

(define (State? x)
  (and (list? x)
       (= (length x) 3)
       (Tm? (first x))
       (Cont? (second x))
       (Store? (third x))))

(define (Tm? x) (or (Val? x) (Ser? x)))

(define (Cont? x) (or (eq? x '•) (KApp? x) (KIf? x)))

(define (Ser? x) (or (AstEnv? x) (SApp? x) (SIf? x) (SSeq? x)))
