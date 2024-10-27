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
;;   State ::= ⟨⟨Ast, Env⟩ ∪ Val, Cont, Store⟩

;; (--> δ :=<1>) : State → (Setof State)
(define-reduction (--> δ :=<1>)
  #:within-signatures [(only domain^    val?)
                       (only    env^    lookup-env extend-env*)
                       (only  store^    lookup-store alloc-loc* update-store*)
                       (only   cont^    push-cont)]
  ;; value
  [`(,(AstEnv (? val? val) _env) ,cnt ,sto)
   `(,val ,cnt ,sto)
   ev-val]

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cnt ,sto)
   #:with loc :=<1> (lookup-env   env var)
   #:with val :=<1> (lookup-store sto loc)
   `(,val ,cnt ,sto)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun vars ast) env) ,cnt ,sto)
   `(,(VFun vars ast env) ,cnt ,sto)
   ev-lam]

  ;; application
  [`(,(AstEnv (App lbl ast asts) env) ,cnt ,sto)
   #:with (values loc sto′) := (push-cont sto lbl cnt)
   `(,(AstEnv ast env) ,(KApp '() asts env loc) ,sto′)
   ev-push-app]

  [`(,(? val? val) ,(KApp vals (cons ast asts) env loc) ,sto)
   `(,(AstEnv ast env) ,(KApp (append vals (list val)) asts env loc) ,sto)
   ev-pop-app₁]

  [`(,(? val? val) ,(KApp '() '() env loc) ,sto)
   `(,val ,(KApp′ '() env loc) ,sto)
   ev-pop-app₂]

  [`(,(? val? val) ,(KApp (cons val′ vals) '() env loc) ,sto)
   `(,val′ ,(KApp′ (append vals (list val)) env loc) ,sto)
   ev-pop-app₃]

  ;; β
  [`(,(VFun vars ast env) ,(KApp′ args _env loc) ,sto)
   #:with `(,(Var nams) ...) := vars
   #:with (values locs sto′) := (alloc-loc* nams sto)
   #:with env′ :=    (extend-env* env vars locs)
   #:with sto″ :=    (update-store* sto′ locs args)
   #:with cnt  :=<1> (lookup-store sto″ loc)
   `(,(AstEnv ast env′) ,cnt ,sto″)
   ev-β]

  ;; primitive application
  [`(,(? Prim? prim) ,(KApp′ args _env loc) ,sto)
   #:with val :=<1> (δ prim args)
   #:with cnt :=<1> (lookup-store sto loc)
   `(,val ,cnt ,sto)
   ev-δ]

  ;; if
  [`(,(AstEnv (If lbl ast₀ ast₁ ast₂) env) ,cnt ,sto)
   #:with (values loc sto′) := (push-cont sto lbl cnt)
   `(,(AstEnv ast₀ env) ,(KIf ast₁ ast₂ env loc) ,sto′)
   ev-push-if]

  [`(,(Bool #f) ,(KIf _ast₁ ast₂ env loc) ,sto)
   #:with cnt :=<1> (lookup-store sto loc)
   `(,(AstEnv ast₂ env) ,cnt ,sto)
   ev-if-#f]

  [`(,(? val? val) ,(KIf ast₁ _ast₂ env loc) ,sto)
   #:when (not (equal? val (Bool #f)))
   #:with cnt :=<1> (lookup-store sto loc)
   `(,(AstEnv ast₁ env) ,cnt ,sto)
   ev-if-#t])


(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import)
  (export  eval^)
  (inherit [red@    reducer])

  ; --> : δ → State → (Setof State)
  (define (--> δ) (reducer δ :=)))
