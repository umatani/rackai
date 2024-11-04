#lang racket/base
(require
 racket/unit
 (only-in racket/match     match-let)
 "../../reduction.rkt"
 (only-in "../../set.rkt"  set)
 (only-in "../../mix.rkt"  define-mixed-unit inherit)
 (only-in "../../misc.rkt" update-store* alloc-loc*)
 "../../signatures.rkt"
 "terms.rkt")
(provide --> eval@)

;; ----------------------------------------
;; Evaluating AST:
;;   State ::= ⟨⟨Ast, Env⟩ ∪ Val, Cont, Store⟩

;; (--> δ :=<1>) : State → (Setof State)
(define-reduction (--> δ :=<1>)
  #:import [(only common^    push-cont)
            (only   misc^    lookup-cont lookup-val)
            (only domain^    val?)
            (only    env^    lookup-env extend-env*)
            (only  store^    lookup-store update-store alloc-loc)]

  #:default [`(,(AstEnv ast env) ,cnt ,sto) ;; for debug
             (printf "eval: unknown form ~a\n" ast)]

  ;; value
  [`(,(AstEnv (? val? val) _env) ,cnt ,sto)
   #:checkpoint (printf "ev-val\n")
   `(,val ,cnt ,sto)
   ev-val]

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cnt ,sto)
   #:checkpoint (printf "ev-x\n")
   (:=<1> loc (lookup-env   env var))
   (:=<1> val (lookup-val sto loc))
   `(,val ,cnt ,sto)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun vars ast) env) ,cnt ,sto)
   #:checkpoint (printf "ev-lam\n")
   `(,(VFun vars ast env) ,cnt ,sto)
   ev-lam]

  ;; application
  [`(,(AstEnv (App lbl ast asts) env) ,cnt ,sto)
   #:checkpoint (printf "ev-push-app\n")
   (:= (values loc sto′) (push-cont sto lbl cnt))
   `(,(AstEnv ast env) ,(KApp '() asts env loc) ,sto′)
   ev-push-app]

  [`(,(? val? val) ,(KApp vals (cons ast asts) env loc) ,sto)
   ;#:checkpoint (printf "ev-push-app₁\n")
   `(,(AstEnv ast env) ,(KApp (append vals (list val)) asts env loc) ,sto)
   ev-pop-app₁]

  [`(,(? val? val) ,(KApp '() '() env loc) ,sto)
   ;#:checkpoint (printf "ev-push-app₂\n")
   `(,val ,(KApp′ '() env loc) ,sto)
   ev-pop-app₂]

  [`(,(? val? val) ,(KApp (cons val′ vals) '() env loc) ,sto)
   ;#:checkpoint (printf "ev-push-app₃\n")
   `(,val′ ,(KApp′ (append vals (list val)) env loc) ,sto)
   ev-pop-app₃]

  ;; β
  [`(,(VFun vars ast env) ,(KApp′ args _env loc) ,sto)
   #:checkpoint (printf "ev-β\n")
   (:=    `(,(Var nams) ...) vars)
   (:=    (values locs sto′) (alloc-loc* alloc-loc nams sto))
   (:=    env′               (extend-env* env vars locs))
   (:=    sto″               (update-store* update-store sto′ locs args))
   (:=<1> cnt                (lookup-cont sto″ loc))
   `(,(AstEnv ast env′) ,cnt ,sto″)
   ev-β]

  ;; primitive application
  [`(,(? Prim? prim) ,(KApp′ args _env loc) ,sto)
   #:checkpoint (printf "ev-δ\n")
   (:=<1> val (δ prim args))
   (:=<1> cnt (lookup-cont sto loc))
   `(,val ,cnt ,sto)
   ev-δ]

  ;; if
  [`(,(AstEnv (If lbl ast₀ ast₁ ast₂) env) ,cnt ,sto)
   #:checkpoint (printf "ev-push-if\n")
   (:= (values loc sto′) (push-cont sto lbl cnt))
   `(,(AstEnv ast₀ env) ,(KIf ast₁ ast₂ env loc) ,sto′)
   ev-push-if]

  [`(,(Bool #f) ,(KIf _ast₁ ast₂ env loc) ,sto)
   #:checkpoint (printf "ev-if-#f\n")
   (:=<1> cnt (lookup-cont sto loc))
   `(,(AstEnv ast₂ env) ,cnt ,sto)
   ev-if-#f]

  [`(,(? val? val) ,(KIf ast₁ _ast₂ env loc) ,sto)
   #:when (not (equal? val (Bool #f)))
   #:checkpoint (printf "ev-if-#t\n")
   (:=<1> cnt (lookup-cont sto loc))
   `(,(AstEnv ast₁ env) ,cnt ,sto)
   ev-if-#t])


(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import)
  (export  eval^)
  (inherit [red@    reducer])

  ; --> : δ → State → (Setof State)
  (define (--> δ) (reducer δ :=)))
