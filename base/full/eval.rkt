#lang racket/base
(require
 racket/unit
 (only-in racket/pretty    pretty-display)
 (only-in racket/match     match match-let match-λ match-λ**)
 (only-in "../../set.rkt"  set ∅ set-add)
 (only-in "../../mix.rkt"  define-mixed-unit inherit)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt")
(provide --> eval@)

;; --> : State -> (Setof State)
(define-reduction (--> δ ==> :=<1>)
  #:within-signatures [(only domain^    val? stx?)
                       (only syntax^    add flip prune)
                       (only    env^    init-env lookup-env extend-env*)
                       (only  store^    lookup-store update-store* alloc-loc*)
                       (only   cont^    push-cont)
                       (only   menv^    init-ξ lookup-ξ extend-ξ)
                       (only mstore^    alloc-name alloc-scope alloc-𝓁
                                        lookup-Σ update-Σ)
                       (only   bind^    bind resolve)
                       (only  parse^    parse)]
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

        ;; ----------------------------------------
        ;; Definition-context environment allocations and updates:

        ;; alloc-def-ξ : Stx Σ → (Values 𝓁 Σ)
        (define (alloc-def-ξ stx Σ) (alloc-𝓁 stx Σ))
        ;; def-ξ-lookup : Σ 𝓁 → ξ
        (define (def-ξ-lookup Σ 𝓁) (lookup-Σ Σ 𝓁))
        ;; def-ξ-update : Σ 𝓁 ξ → Σ
        (define (def-ξ-update Σ 𝓁 ξ) (update-Σ Σ 𝓁 ξ))

        ;; ----------------------------------------
        ;; Box allocations and updates:

        ;; alloc-box : Stx Σ → (Values 𝓁 Σ)
        (define (alloc-box stx Σ) (alloc-𝓁 stx Σ))
        ;; box-lookup : Σ 𝓁 → Val
        (define (box-lookup Σ 𝓁) (lookup-Σ Σ 𝓁))
        ;; box-update : Σ 𝓁 Val → Σ
        (define (box-update Σ 𝓁 v) (update-Σ Σ 𝓁 v))]
  ;; value
  [`(,(AstEnv _ph (? val? val) _env _maybe-scpᵢ _ξ) ,cnt ,sto ,Σ̂)
   `(,val ,cnt ,sto ,Σ̂)
   ev-val]

  ;; reference
  [`(,(AstEnv _ph (? Var? var) env _maybe-scpᵢ _ξ) ,cnt ,sto ,Σ̂)
   #:with loc :=<1> (lookup-env env var)
   #:with val :=<1> (lookup-store sto loc)
   `(,val ,cnt ,sto ,Σ̂)
   ev-x]

  ;; lambda
  [`(,(AstEnv _ph (Fun vars ast) env _maybe-scpᵢ _ξ) ,cnt ,sto ,Σ̂)
   `(,(VFun vars ast env) ,cnt ,sto ,Σ̂)
   ev-lam]

  ;; application
  [`(,(AstEnv ph (App lbl ast asts) env maybe-scpᵢ ξ) ,cnt ,sto ,Σ̂)
   #:with (values loc sto′) := (push-cont sto lbl cnt)
   `(,(AstEnv ph ast env maybe-scpᵢ ξ)
     ,(KApp '() asts `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto′ ,Σ̂)
   ev-push-app]

  [`(,(? val? val)
     ,(KApp vals (cons ast asts) `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   `(,(AstEnv ph ast env maybe-scpᵢ ξ)
     ,(KApp (append vals (list val)) asts `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   ev-pop-app₁]

  [`(,(? val? val)
     ,(KApp '() '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   `(,val ,(KApp′ '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc) ,sto ,Σ̂)
   ev-pop-app₂]

  [`(,(? val? val)
     ,(KApp (cons val′ vals) '() `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   `(,val′ ,(KApp′ (append vals (list val))
                   `(,ph ,env ,maybe-scpᵢ ,ξ) loc) ,sto ,Σ̂)
   ev-pop-app₃]

  ;; local value
  [`(,(Prim 'syntax-local-value _stx)
     ,(KApp′ `(,(? id? id)) `(,ph ,_env ,_maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:with nam :=<1> (resolve ph id Σ)
   #:with val :=<1> (lookup-ξ ξ nam)
   #:with cnt :=<1> (lookup-store sto loc)
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
   #:with ξ_defs :=<1> (def-ξ-lookup Σ 𝓁)
   #:with    nam :=<1> (resolve ph id Σ)
   #:with    val :=<1> (lookup-ξ ξ_defs nam)
   #:with    cnt :=<1> (lookup-store sto loc)
   `(,val ,cnt ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ev-lval-defs]

  ;; local binder
  [`(,(Prim 'syntax-local-identifier-as-binding _stx)
     ,(KApp′ `(,(? id? id)) `(,ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:with    cnt :=<1> (lookup-store sto loc)
   `(,(prune ph id scpsᵤ) ,cnt ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ev-lbinder]

  ;; create definition context
  [`(,(Prim 'syntax-local-make-definition-context stx)
     ,(KApp′ `() `(,_ph ,_env ,_maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:with (values scp Σ₁) :=    (alloc-scope 'defs Σ₀)
   #:with   (values 𝓁 Σ₂) :=    (alloc-def-ξ stx Σ₁)
   #:with             cnt :=<1> (lookup-store sto loc)
   `(,(Defs scp 𝓁) ,cnt ,sto
                   ,(Σ̂ (def-ξ-update Σ₂ 𝓁 ξ) (set-add scpsₚ scp) scpsᵤ))
   ev-slmdc]

  ;; create definition binding (for a variable)
  [`(,(Prim 'syntax-local-bind-syntaxes _stx)
     ,(KApp′ `(,(Lst (? id? id)) ,(Bool #f) ,(Defs scp 𝓁))
             `(,ph ,_env ,maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ₀ scpsₚ scpsᵤ))
   #:with              id′ := (add ph (prune ph (flip ph id maybe-scpᵢ) scpsᵤ)
                                   scp)
   #:with (values nam Σ₁) :=    (alloc-name id′ Σ₀)
   #:with              Σ₂ :=    (bind ph Σ₁ id′ nam)
   #:with          ξ_defs :=<1> (def-ξ-lookup Σ₂ 𝓁)
   #:with              Σ₃ :=    (def-ξ-update Σ₂ 𝓁
                                  (extend-ξ ξ_defs nam (TVar id′)))
   #:with             cnt :=<1> (lookup-store sto loc)
   `(,(Lst id′) ,cnt ,sto ,(Σ̂ Σ₃ scpsₚ scpsᵤ))
   ev-slbsv]

  ;; create macro definition binding
  [`(,(Prim 'syntax-local-bind-syntaxes stx)
     ,(KApp′ `(,(Lst (? id? id)) ,(? stx? stx_arg) ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
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
   #:with ast :=<1> (parse (add1 ph) stx_arg′ Σ)
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
   #:with          ξ_defs :=<1> (def-ξ-lookup Σ₀ 𝓁)
   #:with             id′ :=    (add ph (prune ph (flip ph id maybe-scpᵢ) scpsᵤ)
                                     scp)
   #:with (values nam Σ₁) :=    (alloc-name id′ Σ₀)
   #:with              Σ₂ :=    (bind ph Σ₁ id′ nam)
   #:with             cnt :=<1> (lookup-store sto loc)
   `(,(Lst id′) ,cnt ,sto ,(Σ̂ (def-ξ-update Σ₂ 𝓁 (extend-ξ ξ_defs nam val))
                               scpsₚ scpsᵤ))
   ev-slbsm″]

  ;; local expand
  [`(,(Prim 'local-expand stx)
     ,(KApp′ `(,(? stx? stx_arg) ,_val_context ,ids_stop)
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:with   ξ′ :=    (make-immutable-hash
                      (hash-map ξ (λ (nam at) (cons nam (unstop at)))))
   #:with nams :=<1> (resolve* ph (lst->list ids_stop) Σ)
   #:with  ats :=<1> (lookup-ξ* ξ′ nams)
   #:with   ξ″ :=    (extend-ξ* ξ′ (map (λ (nam at) (cons nam (TStop at)))
                                        nams ats))
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
   #:with cnt :=<1> (lookup-store sto loc)
   `(,(flip ph stx_arg′ maybe-scpᵢ) ,cnt ,sto ,Σ̂)
   ev-lexpand′]

  ;; local expand with definition context
  ;;   similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [`(,(Prim 'local-expand stx)
     ,(KApp′ `(,(? stx? stx_arg) ,_val_context ,ids_stop ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))

   #:with ξ_defs :=<1> (def-ξ-lookup Σ 𝓁)
   #:with     ξ′ :=    (make-immutable-hash
                        (hash-map ξ_defs (λ (nam at) (cons nam (unstop at)))))
   #:with   nams :=<1> (resolve* ph (lst->list ids_stop) Σ)
   #:with    ats :=<1> (lookup-ξ* ξ′ nams)
   #:with     ξ″ :=    (extend-ξ* ξ′ (map (λ (nam at) (cons nam (TStop at)))
                                          nams ats))
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
   #:with (values 𝓁 Σ′) :=    (alloc-box stx Σ)
   #:with           cnt :=<1> (lookup-store sto loc)
   `(,𝓁 ,cnt ,sto ,(Σ̂ (box-update Σ′ 𝓁 val) scpsₚ scpsᵤ))
   ev-box]

  ;; unbox
  [`(,(Prim 'unbox _stx)
     ,(KApp′ `(,(? 𝓁? 𝓁)) `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:with val :=<1> (box-lookup Σ 𝓁)
   #:with cnt :=<1> (lookup-store sto loc)
   `(,val ,cnt ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   ev-unbox]

  ;; set-box!
  [`(,(Prim 'set-box! _stx)
     ,(KApp′ `(,(? 𝓁? 𝓁) ,val) `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,(Σ̂ Σ scpsₚ scpsᵤ))
   #:with cnt :=<1> (lookup-store sto loc)
   `(,val ,cnt ,sto ,(Σ̂ (box-update Σ 𝓁 val) scpsₚ scpsᵤ))
   ev-set-box!]

  ;; β
  [`(,(VFun vars ast env)
     ,(KApp′ args `(,ph ,_env ,maybe-scpᵢ ,ξ) loc) ,sto ,Σ̂)
   #:with `(,(Var nams) ...) :=    vars
   #:with (values locs sto′) :=    (alloc-loc* nams sto)
   #:with               env′ :=    (extend-env* env vars locs)
   #:with               sto″ :=    (update-store* sto′ locs args)
   #:with                cnt :=<1> (lookup-store sto″ loc)
   `(,(AstEnv ph ast env′ maybe-scpᵢ ξ) ,cnt ,sto″ ,Σ̂)
   ev-β]

  ;; primitive application (except StxPrim)
  [`(,(and (Prim nam _stx) prim)
     ,(KApp′ args `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc) ,sto ,Σ̂)
   #:when (not (stx-prim? nam))
   #:with val :=<1> (δ prim args)
   #:with cnt :=<1> (lookup-store sto loc)
   `(,val ,cnt ,sto ,Σ̂)
   ev-δ]

  ;; if
  [`(,(AstEnv ph (If lbl ast₀ ast₁ ast₂) env maybe-scpᵢ ξ)
     ,cnt ,sto ,Σ̂)
   #:with (values loc sto′) := (push-cont sto lbl cnt)
   `(,(AstEnv ph ast₀ env maybe-scpᵢ ξ)
     ,(KIf ast₁ ast₂ `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto′ ,Σ̂)
   ev-push-if]

  [`(,(Bool #f)
     ,(KIf _ast₁ ast₂ `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)   
   #:with cnt :=<1> (lookup-store sto loc)
   `(,(AstEnv ph ast₂ env maybe-scpᵢ ξ)
     ,cnt ,sto ,Σ̂)
   ev-if-#f]

  [`(,(? val? val)
     ,(KIf ast₁ _ast₂ (list ph env maybe-scpᵢ ξ) loc)
     ,sto ,Σ̂)   
   #:when (not (equal? val (Bool #f)))
   #:with cnt :=<1> (lookup-store sto loc)
   `(,(AstEnv ph ast₁ env maybe-scpᵢ ξ)
     ,cnt ,sto ,Σ̂)
   ev-if-#t]

  ;; in-expand
  [(InExpand ζ s)
   #:with ζ′ <- (lift ((==>) ζ))
   (InExpand ζ′ s)
   ev-in-expand])


(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import (only domain^    val?)
          (only    env^    init-env)
          (only  store^    init-store)
          (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only expand^    ==>))
  (export eval^)
  (inherit [red@ reducer])

  ;; δ → → State → (Setof State)
  (define (--> δ) (λ () (reducer δ (==> δ) :=)))

  ;; eval : Ph Ast MaybeScp ξ Σ̂ → (Values Val Σ̂)
  (define (eval δ ph ast maybe-scpᵢ ξ Σ̂)
    (define -->δ (--> δ))
    (match-let ([(set `(,(? val? val) ● ,_sto ,Σ̂′))
                 (apply-reduction*
                  (-->δ) `(,(AstEnv ph ast (init-env) maybe-scpᵢ ξ)
                           ● ,(init-store) ,Σ̂))])
      (values val Σ̂′)))

  ;; evaluate : Ast → Val
  (define (evaluate δ ast)
    (call-with-values
     (λ () (eval δ 0 ast 'no-scope (init-ξ) (Σ̂ (init-Σ) ∅ ∅)))
     (λ (val Σ̂) val))))
