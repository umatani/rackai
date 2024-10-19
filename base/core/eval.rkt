#lang racket/base
(require
 racket/unit
 (only-in racket/match match-let)
 "../../reduction.rkt"
 (only-in "../../set.rkt" set)
 (only-in "../../mix.rkt" define-mixed-unit inherit)
 "../../signatures.rkt"
 "terms.rkt")
(provide --> eval@)

;; ----------------------------------------
;; Evaluating AST:
;;   State ::= (List (U AstEnv Val) Cont Store)

;; (--> δ :=<1>) : State → (Setof State)
(define-reduction (--> δ :=<1>)
  #:within-signatures [(only domain^    val?)
                       (only    env^    lookup-env extend-env*)
                       (only  store^    lookup-store alloc-loc* update-store*)
                       (only   cont^    push-cont)]
  ;; propagate env into subterms
  [`(,(AstEnv (If lbl ast₀ ast₁ ast₂) env) ,cnt ,sto)
   `(,(SIf lbl (AstEnv ast₀ env)
               (AstEnv ast₁ env)
               (AstEnv ast₂ env))
     ,cnt ,sto)
   ev-env-if]

  [`(,(AstEnv (App lbl ast asts) env) ,cnt ,sto)
   `(,(SApp lbl '() (map (λ (ast) (AstEnv ast env)) (cons ast asts)))
     ,cnt ,sto)
   ev-env-app]

  ;; value
  [`(,(AstEnv (? val? val) _) ,cnt ,sto)
   `(,val ,cnt ,sto)
   ev-val]

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cnt ,sto)
   #:with loc :=<1> (lookup-env   env var)
   #:with val :=<1> (lookup-store sto loc)
   `(,(AstEnv val env) ,cnt ,sto)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun  vars ast)     env) ,cnt ,sto)
   `(,(AstEnv (VFun vars ast env) env) ,cnt ,sto)
   ev-lam]

  ;; application
  [`(,(SApp lbl vals `(,astenv ,astenvs ...)) ,cnt ,sto)
   #:with (values loc sto′) := (push-cont sto lbl cnt)
   `(,astenv ,(KApp lbl vals  astenvs loc) ,sto′)
   ev-push-app]

  [`(,(? val? val) ,(KApp lbl vals astenvs loc) ,sto)
   #:with cnt :=<1> (lookup-store sto loc)
   `(,(SApp lbl (append vals (list val)) astenvs) ,cnt ,sto)
   ev-pop-app]

  ;; β
  [`(,(SApp _lbl vals '()) ,cnt ,sto)
   #:when (and (pair? vals) (VFun? (car vals)))
   #:with `(,(VFun vars ast env) ,args ...) := vals
   #:with                `(,(Var nams) ...) := vars
   #:with                (values locs sto′) := (alloc-loc* nams sto)
   #:with                              env′ := (extend-env* env vars locs)
   #:with                              sto″ := (update-store* sto′ locs args)
   `(,(AstEnv ast env′) ,cnt ,sto″)
   ev-β]

  ;; primitive application
  [`(,(SApp _lbl vals '()) ,cnt ,sto)
   #:when (and (pair? vals) (Prim? (car vals)))
   #:with val :=<1> (δ (car vals) (cdr vals))
   `(,val ,cnt ,sto)
   ev-δ]

  ;; if
  [`(,(SIf lbl (? (λ (x) (not (val? x))) astenv)
           astenv′ astenv″) ,cnt ,sto)
   #:with (values loc sto′) := (push-cont sto lbl cnt)
   `(,astenv ,(KIf lbl astenv′ astenv″ loc) ,sto′)
   ev-push-if]

  [`(,(? val? val) ,(KIf lbl astenv astenv′ loc) ,sto)
   #:with cnt :=<1> (lookup-store sto loc)
   `(,(SIf lbl val astenv astenv′) ,cnt ,sto)
   ev-pop-if]

  [`(,(SIf _lbl (Bool #f) _ astenv) ,cnt ,sto)
   `(,astenv ,cnt ,sto)
   ev-if-#f]

  [`(,(SIf _lbl (? val? val) astenv _) ,cnt ,sto)
   #:when (not (equal? val (Bool #f)))
   `(,astenv ,cnt ,sto)
   ev-if-#t])

(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import  (only domain^    val?)
           (only    env^    init-env)
           (only  store^    init-store))
  (export  eval^)
  (inherit [red@            reducer])

  ; --> : δ → State → (Setof State)
  (define (--> δ) (reducer δ :=))

  ; evaluate : δ Ast → Val
  (define (evaluate δ ast)
    (define -->δ (--> δ))
    (match-let ([(set `(,(? val? v) ● ,_store))
                 (apply-reduction*
                  -->δ `(,(AstEnv ast (init-env)) ● ,(init-store)))])
      v)))
