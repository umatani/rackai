#lang racket/base
(require
 racket/unit
 (only-in racket/pretty      pretty-display)
 (only-in racket/match       match match-let match-λ match-λ**)
 (only-in "../../set.rkt"    set ∅ set-add)
 (only-in "../../mix.rkt"    define-mixed-unit inherit)
 (only-in "../../misc.rkt"   update-store* alloc-loc*)
 (only-in "../../syntax.rkt" prune)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt")
(provide --> eval@)

;; --> : State → (SetM State)
(define-reduction (--> δ ==> :=<1>)
  #:import [(only common^    push-cont)
            (only   misc^    lookup-cont lookup-val)
            (only domain^    val? stx?)
            (only syntax^    add flip)
            (only    env^    init-env lookup-env extend-env*)
            (only  store^    lookup-store update-store
                             alloc-loc)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    alloc-name alloc-scope alloc-𝓁 lookup-Σ update-Σ)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:default [`(,(AstEnv ph ast env maybe-scpᵢ ξ) ,cnt ,sto ,Σ̂)
             #:abort (format "eval: unknown form ~a\n" ast)]

  #:do [;; resolve* : Ph (Listof Id) Σ → (Listof Nam))
        (define (resolve* ph ids Σ)
          (map (λ (id) (resolve ph id Σ)) ids))

        ;; lookup-ξ* : ξ (Listof Nam) → (Listof AllTransform)
        (define (lookup-ξ* ξ nams)
          (map (λ (nam) (lookup-ξ ξ nam)) nams))

        ;; extend-ξ* : ξ (Listof (Pairof Nam AllTransform)) → ξ
        (define (extend-ξ* ξ nas)
          (foldr (match-λ** [((cons n a) ξ) (extend-ξ ξ n a)]) ξ nas))

        ;; unstop : AllTransform → AllTransform
        (define (unstop at)
          (match at
            [(TStop at) at]
            [_ at]))

        ;; unstop-ξ : ξ → ξ
        (define (unstop-ξ ξ)
          (make-immutable-hash
           (hash-map ξ (λ (nam at) (cons nam (unstop at))))))

        ;; ----------------------------------------
        ;; Definition-context environment allocations and updates:

        ;; alloc-def-ξ : Stx Σ → (Values 𝓁 Σ)
        (define (alloc-def-ξ stx Σ) (alloc-𝓁 stx Σ))
        ;; lookup-def-ξ : Σ 𝓁 → ξ
        (define (lookup-def-ξ Σ 𝓁) (lookup-Σ Σ 𝓁))
        ;; update-def-ξ : Σ 𝓁 ξ → Σ
        (define (update-def-ξ Σ 𝓁 ξ) (update-Σ Σ 𝓁 ξ))
        ;; extend-def-ξ : Σ 𝓁 nam at → Σ
        (define (extend-def-ξ Σ 𝓁 nam at)
          (update-def-ξ Σ 𝓁 (extend-ξ (lookup-def-ξ Σ 𝓁) nam at)))


        ;; ----------------------------------------
        ;; Box allocations and updates:

        ;; alloc-box : Stx Σ → (Values 𝓁 Σ)
        (define (alloc-box stx Σ) (alloc-𝓁 stx Σ))
        ;; lookup-box : Σ 𝓁 → Val
        (define (lookup-box Σ 𝓁) (lookup-Σ Σ 𝓁))
        ;; update-box : Σ 𝓁 Val → Σ
        (define (update-box Σ 𝓁 v) (update-Σ Σ 𝓁 v))]

  ;; value
  [`(,(AstEnv _ph (? val? val) _env _maybe-scpᵢ _ξ) ,cnt ,sto ,Σ̂)
   #:checkpoint (printf "ev-val\n")
   `(,val ,cnt ,sto ,Σ̂)
   ev-val]

  ;; reference
  [`(,(AstEnv _ph (? Var? var) env _maybe-scpᵢ _ξ) ,cnt ,sto ,Σ̂)
   #:checkpoint (printf "ev-x\n")
   loc :=<1> (lookup-env env var)
   val :=<1> (lookup-val sto loc)
   `(,val ,cnt ,sto ,Σ̂)
   ev-x]

  ;; lambda
  [`(,(AstEnv _ph (Fun vars ast) env _maybe-scpᵢ _ξ) ,cnt ,sto ,Σ̂)
   #:checkpoint (printf "ev-lam\n")
   `(,(VFun vars ast env) ,cnt ,sto ,Σ̂)
   ev-lam]

  ;; application
  [`(,(AstEnv ph (App lbl ast asts) env maybe-scpᵢ ξ) ,cnt ,sto ,Σ̂)
   ;#:checkpoint (printf "ev-push-app\n")
   (values loc sto′) := (push-cont sto lbl cnt)
   `(,(AstEnv ph ast env maybe-scpᵢ ξ)
     ,(KApp '() asts `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto′ ,Σ̂)
   ev-push-app]

  [`(,(? val? val)
     ,(KApp vals (cons ast asts) `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   ;#:checkpoint (printf "ev-pop-app₁\n")
   `(,(AstEnv ph ast env maybe-scpᵢ ξ)
     ,(KApp (append vals (list val)) asts `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   ev-pop-app₁]

  [`(,(? val? val)
     ,(KApp '() '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   ;#:checkpoint (printf "ev-pop-app₂\n")
   `(,val ,(KApp′ '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc) ,sto ,Σ̂)
   ev-pop-app₂]

  [`(,(? val? val)
     ,(KApp (cons val′ vals) '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   ;#:checkpoint (printf "ev-pop-app₃\n")
   `(,val′ ,(KApp′ (append vals (list val))
                   `(,ph ,env ,maybe-scpᵢ ,ξ) loc) ,sto ,Σ̂)
   ev-pop-app₃]

  ;; local value
  [`(,(Prim 'syntax-local-value _stx)
     ,(KApp′ `(,(? id? id)) `(,ph ,_env ,_maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   nam :=<1> (resolve ph id Σ)
   val :=<1> (lookup-ξ ξ nam)
   #:when (val? val)
   #:checkpoint (printf "ev-lval\n")
   cnt :=<1> (lookup-cont sto loc)
   `(,val ,cnt ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ev-lval]

  ;; local value with definition context
  ;;   - similar to the basic local value case, but using definition
  ;;     context's environment
  ;;   - Unlike the fourth argument to local-expand, the scopes associated with
  ;;     the provided definition contexts are not used to enrich id's
  ;;     lexical information.
  [`(,(Prim 'syntax-local-value _stx)
     ,(KApp′ `(,(? id? id) ,(Bool #f) ,(Defs _scp 𝓁))
             `(,ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ξ_defs :=<1> (lookup-def-ξ Σ 𝓁)
      nam :=<1> (resolve ph id Σ)
      val :=<1> (lookup-ξ ξ_defs nam)
   #:when (val? val)
   #:checkpoint (printf "ev-lval-defs\n")
   cnt :=<1> (lookup-cont sto loc)
   `(,val ,cnt ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ev-lval-defs]

  ;; local binder
  [`(,(Prim 'syntax-local-identifier-as-binding _stx)
     ,(KApp′ `(,(? id? id)) `(,ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-lbinder\n")
   cnt :=<1> (lookup-cont sto loc)
   `(,(prune ph id scpsᵤ) ,cnt ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ev-lbinder]

  ;; create definition context
  [`(,(Prim 'syntax-local-make-definition-context stx)
     ,(KApp′ `() `(,_ph ,_env ,_maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-slmdc\n")
   (values scp Σ₁) :=    (alloc-scope 'defs Σ₀)
     (values 𝓁 Σ₂) :=    (alloc-def-ξ stx Σ₁)
               cnt :=<1> (lookup-cont sto loc)
   `(,(Defs scp 𝓁) ,cnt ,sto
                   ,(Σ̂ (update-def-ξ Σ₂ 𝓁 ξ) (set-add scpsₚ scp) scpsᵤ))
   ev-slmdc]

  ;; create definition binding (for a variable)
  [`(,(Prim 'syntax-local-bind-syntaxes _stx)
     ,(KApp′ `(,(Lst (? id? id)) ,(Bool #f) ,(Defs scp 𝓁))
             `(,ph ,_env ,maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-slbsv\n")
               id′ :=    (add ph (prune ph (flip ph id maybe-scpᵢ) scpsᵤ) scp)
   (values nam Σ₁) :=    (alloc-name id′ Σ₀)
                Σ₂ :=    (bind ph Σ₁ id′ nam)
                Σ₃ :=    (extend-def-ξ Σ₂ 𝓁 nam (TVar id′))
               cnt :=<1> (lookup-cont sto loc)
   `(,(Lst id′) ,cnt ,sto ,(Σ̂ Σ₃ scpsₚ scpsᵤ))
   ev-slbsv]

  ;; create macro definition binding
  [`(,(Prim 'syntax-local-bind-syntaxes stx)
     ,(KApp′ `(,(Lst (? id? id)) ,(? stx? stx_arg) ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-slbsm\n")
   (InExpand
    (ζ (Stxξ (add1 ph)
             (add ph (flip ph stx_arg maybe-scpᵢ) scp) (init-ξ))
       '● (Σ̂ Σ ∅ ∅))
    `(,(Prim 'syntax-local-bind-syntaxes2 stx)
      ,(KApp′ `(,id ,(Defs scp 𝓁)) `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
      ,sto ,(Σ̂ Σ ;; not used
               scpsₚ scpsᵤ)))
   ev-slbsm]

  [(InExpand (ζ (? Stx? stx_arg′) '● (Σ̂ Σ _scpsₚ _scpsᵤ))
             `(,(Prim 'syntax-local-bind-syntaxes2 _stx)
               ,(KApp′ `(,id ,(Defs scp 𝓁)) `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
               ,sto ,(Σ̂ _Σ scpsₚ scpsᵤ)))
   #:checkpoint (printf "ev-slbsm′\n")
   ast <- (parse (add1 ph) stx_arg′ Σ)
   `(,(AstEnv ph ast (init-env) 'no-scope ξ)
     ,(KApp `(,(Prim 'syntax-local-bind-syntaxes2
                     (Stx (Bool #f) `((0 . ,scpsₚ) (1 . ,scpsᵤ))))
              ,id ,(Defs scp 𝓁)) '()
            `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ ∅))
   ev-slbsm′]

  [`(,(Prim 'syntax-local-bind-syntaxes2
            (Stx (Bool #f) `((0 . ,scpsₚ) (1 . ,scpsᵤ))))
     ,(KApp′ `(,(? id? id) ,(Defs scp 𝓁) ,val)
             `(,ph ,_env ,maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ₀ _scpsₚ _scpsᵤ))
   #:checkpoint (printf "ev-slbsm″\n")
               id′ :=    (add ph (prune ph (flip ph id maybe-scpᵢ) scpsᵤ) scp)
   (values nam Σ₁) :=    (alloc-name id′ Σ₀)
                Σ₂ :=    (bind ph Σ₁ id′ nam)
               cnt :=<1> (lookup-cont sto loc)
   `(,(Lst id′) ,cnt ,sto ,(Σ̂ (extend-def-ξ Σ₂ 𝓁 nam val) scpsₚ scpsᵤ))
   ev-slbsm″]

  ;; local expand
  [`(,(Prim 'local-expand stx)
     ,(KApp′ `(,(? stx? stx_arg) ,_val_context ,ids_stop)
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-lexpand\n")
     ξ′ :=    (unstop-ξ ξ)
   nams :=<1> (resolve* ph (lst→list ids_stop) Σ)
    ats :=<1> (lookup-ξ* ξ′ nams)
     ξ″ :=    (extend-ξ* ξ′ (map (λ (nam at) (cons nam (TStop at))) nams ats))
   (InExpand
    (ζ (Stxξ ph (flip ph stx_arg maybe-scpᵢ) ξ″) '● (Σ̂ Σ scpsₚ scpsᵤ))
    `(,(Prim 'local-expand stx)
      ,(KApp′ '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
      ,sto ,(Σ̂ Σ ;; not used
               ∅ ∅)))
   ev-lexpand]

  [(InExpand (ζ (? Stx? stx_arg′) '● Σ̂)
             `(,(Prim 'local-expand _stx)
               ,(KApp′ '() `(,ph ,_env ,maybe-scpᵢ ,_ξ) loc)
               ,sto ,_Σ̂))
   #:checkpoint (printf "ev-lexpand′\n")
   cnt :=<1> (lookup-cont sto loc)
   `(,(flip ph stx_arg′ maybe-scpᵢ) ,cnt ,sto ,Σ̂)
   ev-lexpand′]

  ;; local expand with definition context
  ;;   similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [`(,(Prim 'local-expand stx)
     ,(KApp′ `(,(? stx? stx_arg) ,_val_context ,ids_stop ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-lexpand-defs\n")
   ξ_defs :=<1> (lookup-def-ξ Σ 𝓁)
       ξ′ :=    (unstop-ξ ξ_defs)
     nams :=<1> (resolve* ph (lst→list ids_stop) Σ)
      ats :=<1> (lookup-ξ* ξ′ nams)
       ξ″ :=    (extend-ξ* ξ′ (map (λ (nam at) (cons nam (TStop at))) nams ats))
   ; TODO?: (flip ph stx_arg maybe-scpᵢ)は間違い？？しかしdefsを使わない場合にも
   ; これはある．．．これがあると，少なくともunit-4が通らない．
   ; しかし，flipないとdefs-begin-with-defnの挙動が実際の処理系と異なってしまう．
   (InExpand
    (ζ (Stxξ ph (add ph (flip ph stx_arg maybe-scpᵢ) scp) ξ″)
       '● (Σ̂ Σ scpsₚ scpsᵤ))
    `(,(Prim 'local-expand stx)
      ,(KApp′ '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
      ,sto ,(Σ̂ Σ ;; not used
               ∅ ∅)))
   ev-lexpand-defs]

  ;; ----------------------------------------
  ;; Including boxes lets us implement recursive definitions as a
  ;; macro, including variable definitions that are in a recursive
  ;; binding scope with macros.

  ;; box
  [`(,(Prim 'box stx)
     ,(KApp′ `(,val) `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-box\n")
   (values 𝓁 Σ′) :=    (alloc-box stx Σ)
             cnt :=<1> (lookup-cont sto loc)
   `(,𝓁 ,cnt ,sto ,(Σ̂ (update-box Σ′ 𝓁 val) scpsₚ scpsᵤ))
   ev-box]

  ;; unbox
  [`(,(Prim 'unbox _stx)
     ,(KApp′ `(,(? 𝓁? 𝓁)) `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-unbox\n")
   val :=<1> (lookup-box Σ 𝓁)
   cnt :=<1> (lookup-cont sto loc)
   `(,val ,cnt ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ev-unbox]

  ;; set-box!
  [`(,(Prim 'set-box! _stx)
     ,(KApp′ `(,(? 𝓁? 𝓁) ,val) `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:checkpoint (printf "ev-set-box!\n")
   cnt :=<1> (lookup-cont sto loc)
   `(,val ,cnt ,sto ,(Σ̂ (update-box Σ 𝓁 val) scpsₚ scpsᵤ))
   ev-set-box!]

  ;; β
  [`(,(VFun vars ast env)
     ,(KApp′ args `(,ph ,_env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   #:checkpoint (printf "ev-β\n")
   `(,(Var nams) ...) :=    vars
   (values locs sto′) :=    (alloc-loc* alloc-loc nams sto)
                 env′ :=    (extend-env* env vars locs)
                 sto″ :=    (update-store* update-store sto′ locs args)
                  cnt :=<1> (lookup-cont sto″ loc)
   `(,(AstEnv ph ast env′ maybe-scpᵢ ξ) ,cnt ,sto″ ,Σ̂)
   ev-β]

  ;; primitive application (except StxPrim)
  [`(,(and (Prim nam _stx) prim)
     ,(KApp′ args `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (not (stx-prim? nam))
   #:checkpoint (printf "ev-δ\n")
   val :=<1> (δ prim args)
   cnt :=<1> (lookup-cont sto loc)
   `(,val ,cnt ,sto ,Σ̂)
   ev-δ]

  ;; if
  [`(,(AstEnv ph (If lbl ast₀ ast₁ ast₂) env maybe-scpᵢ ξ)
     ,cnt
     ,sto ,Σ̂)
   #:checkpoint (printf "ev-push-if\n")
   (values loc sto′) := (push-cont sto lbl cnt)
   `(,(AstEnv ph ast₀ env maybe-scpᵢ ξ)
     ,(KIf ast₁ ast₂ `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto′ ,Σ̂)
   ev-push-if]

  [`(,(Bool #f)
     ,(KIf _ast₁ ast₂ `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)   
   #:checkpoint (printf "ev-if-#f\n")
   cnt :=<1> (lookup-cont sto loc)
   `(,(AstEnv ph ast₂ env maybe-scpᵢ ξ)
     ,cnt ,sto ,Σ̂)
   ev-if-#f]

  [`(,(? val? val)
     ,(KIf ast₁ _ast₂ (list ph env maybe-scpᵢ ξ) loc)
     ,sto ,Σ̂)   
   #:when (not (equal? val (Bool #f)))
   #:checkpoint (printf "ev-if-#t\n")
   cnt :=<1> (lookup-cont sto loc)
   `(,(AstEnv ph ast₁ env maybe-scpᵢ ξ)
     ,cnt
     ,sto ,Σ̂)
   ev-if-#t]

  ;; in-expand
  [(InExpand ζ s)
   ζ′ <- ((==>) ζ)
   (InExpand ζ′ s)
   ev-in-expand])


(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import
   (only  expand^    ==>))
  (export eval^)
  (inherit [red@    reducer])

  ;; --> : δ → → State → (SetM State)
  (define (--> δ) (λ () (reducer δ (==> δ) :=))))
