#lang racket/signature

;;;; Language

(struct Var (nam)         #:constructor-name var)
(struct Fun (vars ast)    #:constructor-name fun)
(struct App (rator rands) #:constructor-name app)
(struct If  (tst thn els) #:constructor-name iif)

;; Value
(struct VFun (vars ast env) #:constructor-name vfun)
;; LBind2 is used only in full
(struct LBind2 (scps_p scps_u) #:constructor-name lbind2)

;; Literal values
(struct Sym (nam) #:constructor-name sym)

;; Defs is used only in full
(struct Defs (scp 𝓁) #:constructor-name defs)

;; Syntax objects (a subset of values)
(struct Stx (e ctx) #:constructor-name stx)

id

;; Eval-time continuation, environment, and store
(struct AstEnv (ast env)    #:constructor-name ast&env)
(struct Store (size tbl)    #:constructor-name store)
(struct KApp (vals tms loc) #:constructor-name kapp)
(struct KIf (thn els loc)   #:constructor-name kif)
(struct SApp (vals tms)     #:constructor-name sapp)
(struct SIf (tst thn els)   #:constructor-name sif)
;; SSeq is used only in full
(struct SSeq (tms)          #:constructor-name sseq)

;; Expand-time environment
(struct TVar (id)             #:constructor-name tvar)
(struct TStop (all-transform) #:constructor-name tstop)

;; Expand-time store
(struct Σ (size tbl)       #:constructor-name mk-Σ)
(struct StoBind (scps nam) #:constructor-name stobind)
(struct Θ (size tbl)       #:constructor-name mk-Θ)
(struct 𝓁 (nam)            #:constructor-name mk-𝓁)

;; Expand-time continuation
;;(struct Stxξ (stx ξ)  #:constructor-name stx&ξ)
(struct Hole ()       #:constructor-name hole)
(struct κ (stx ex? 𝓁) #:constructor-name mk-κ)

;; Expand-time state (configuration)
(struct InEval (state ξ)  #:constructor-name in-eval)
(struct ζ (stx ex? κ Θ Σ) #:constructor-name mk-ζ)

stx?
stl?
proper-stl?
id?
atom?
prim?
stx-prim?
val?
nam?
state?
tm?
cont?
ser?
