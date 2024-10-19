#lang racket/base
(require
 racket/unit
 (only-in racket/match     match match-let)
 (only-in "../../set.rkt"  set ∅)
 (only-in "../../mix.rkt"  define-mixed-unit inherit)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "terms.rkt"
 (only-in "../../misc.rkt" union))
(provide --> eval@)

;; --> : State -> (Setof State)
(define-reduction (--> δ ==> :=<1>)
  #:within-signatures [(only domain^    val? stx?)
                       (only syntax^    add flip prune)
                       (only    env^    init-env lookup-env extend-env*)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-scope alloc-𝓁 lookup-Σ update-Σ)
                       (only  bind^    bind resolve)
                       (only parse^    parse)]
  #:do [;; resolve* : Ph (Listof Id) Σ -> (Listof Nam))
        (define (resolve* ph ids Σ)
          (match ids
            ['() '()]
            [(cons id ids*) (cons (resolve ph id Σ)
                                  (resolve* ph ids* Σ))]))

        ;; lookup-ξ* : ξ (Listof Nam) -> (Listof AllTransform)
        (define (lookup-ξ* ξ ns)
          (match ns
            ['() '()]
            [(cons n ns*) (cons (lookup-ξ ξ n) (lookup-ξ* ξ ns*))]))

        ;; extend-ξ* : ξ (Listof (Pairof Nam AllTransform)) -> ξ
        (define (extend-ξ* ξ nas)
          (match nas
            ['() ξ]
            [(cons (cons n a) nas*) (extend-ξ (extend-ξ* ξ nas*) n a)]))

        ;; unstop : AllTransform -> AllTransform
        (define (unstop all-transform)
          (match all-transform
            [(TStop all-transform2) all-transform2]
            [_ all-transform]))

        ;; ----------------------------------------
        ;; Definition-context environment allocations and updates:

        ;; alloc-def-ξ : Stx Σ -> (Values 𝓁 Σ)
        (define (alloc-def-ξ stx Σ) (alloc-𝓁 stx Σ))
        ;; def-ξ-lookup : Σ 𝓁 -> ξ
        (define (def-ξ-lookup Σ 𝓁) (lookup-Σ Σ 𝓁))
        ;; def-ξ-update : Σ 𝓁 ξ -> Σ
        (define (def-ξ-update Σ 𝓁 ξ) (update-Σ Σ 𝓁 ξ))

        ;; ----------------------------------------
        ;; Box allocations and updates:

        ;; alloc-box : Stx Σ -> (Values 𝓁 Σ)
        (define (alloc-box stx Σ) (alloc-𝓁 stx Σ))
        ;; box-lookup : Σ 𝓁 -> Val
        (define (box-lookup Σ 𝓁) (lookup-Σ Σ 𝓁))
        ;; box-update : Σ 𝓁 Val -> Σ
        (define (box-update Σ 𝓁 val) (update-Σ Σ 𝓁 val))]

  ;; propagate env into subterms
  [`(,(AstEnv ph (If lbl ast_test ast_then ast_else) env maybe-scp_i ξ)
     ,cont ,store ,Σ*)
   `(,(SIf lbl
           (AstEnv ph ast_test env maybe-scp_i ξ)
           (AstEnv ph ast_then env maybe-scp_i ξ)
           (AstEnv ph ast_else env maybe-scp_i ξ)) ,cont ,store ,Σ*)
   ev-env-if]

  [`(,(AstEnv ph (App lbl ast_fun ast_args) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   `(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            '()
            (cons (AstEnv ph ast_fun env maybe-scp_i ξ)
                  (map (λ (arg) (AstEnv ph arg env maybe-scp_i ξ))
                       ast_args)))
     ,cont ,store ,Σ*)
   ev-env-app]

  ;; value
  [`(,(AstEnv ph (? val? val) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   `(,val ,cont ,store ,Σ*)
   ev-val]

  ;; reference
  [`(,(AstEnv ph (? Var? var) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   #:with loc :=<1> (lookup-env env var)
   #:with val :=<1> (lookup-store store loc)
   `(,(AstEnv ph val env maybe-scp_i ξ) ,cont ,store ,Σ*)
   ev-x]

  ;; lambda
  [`(,(AstEnv ph (Fun vars ast) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   `(,(AstEnv ph (VFun vars ast env) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   ev-lam]

  ;; application
  [`(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,vals ...) `(,tm ,tms ...)) ,cont ,store ,Σ*)
   #:with (values loc_new store_1) := (push-cont store lbl cont)
   `(,tm ,(KApp lbl `(,ph ,maybe-scp_i ,ξ) vals  tms loc_new) ,store_1 ,Σ*)
   ev-push-app]

  [`(,(? val? val) ,(KApp lbl `(,ph ,maybe-scp_i ,ξ)
                          vals clos loc_cont) ,store ,Σ*)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SApp lbl `(,ph ,maybe-scp_i ,ξ) (append vals (list val)) clos)
     ,cont ,store ,Σ*)
   ev-pop-app]

  ;; local value
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-value _) ,(? id? id)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with nam :=<1> (resolve ph id Σ)
   #:with val :=<1> (lookup-ξ ξ nam)
   `(,val ,cont ,store ,Σ*_0)
   ev-lval]

  ;; local value with definition context
  ;; - similar to the basic local value case, but using definition
  ;;   context's environment
  ;; - Unlike the fourth argument to local-expand, the scopes associated with
  ;;   the provided definition contexts are not used to enrich id's
  ;;   lexical information.
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-value _)
              ,(? id? id) ,(Bool #f) ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_defs :=<1> (def-ξ-lookup Σ 𝓁)
   #:with    nam :=<1> (resolve ph id Σ)
   #:with    val :=<1> (lookup-ξ ξ_defs nam)
   `(,val ,cont ,store ,Σ*_0)
   ev-lval-defs]

  ;; local binder
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-identifier-as-binding _) ,(? id? id)) '())
     ,cont ,store ,(and Σ*_0 (Σ* _ _ scps_u)))
   `(,(prune ph id scps_u) ,cont ,store ,Σ*_0)
   ev-lbinder]

  ;; create definition context
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-make-definition-context stx)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with (values scp_defs Σ_1) := (alloc-scope 'defs Σ)
   #:with        (values 𝓁 Σ_2) := (alloc-def-ξ stx Σ_1)
   #:with                  Σ*_1 := (Σ* (def-ξ-update Σ_2 𝓁 ξ)
                                       (union (set scp_defs) scps_p)
                                       scps_u)
   `(,(Defs scp_defs 𝓁) ,cont ,store ,Σ*_1)
   ev-slmdc]

  ;; create definition binding (for a variable)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-bind-syntaxes _)
              ,(Pair (? id? id_arg) (Null))
              ,(Bool #f) ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with              id_defs := (add ph
                                       (prune ph (flip ph id_arg maybe-scp_i)
                                              scps_u)
                                       scp_defs)
   #:with (values nam_new Σ_1) :=    (alloc-name id_defs Σ)
   #:with                  Σ_2 :=    (bind ph Σ_1 id_defs nam_new)
   #:with               ξ_defs :=<1> (def-ξ-lookup Σ_2 𝓁)
   #:with                  Σ_3 :=    (def-ξ-update Σ_2 𝓁
                                        (extend-ξ ξ_defs nam_new
                                                   (TVar id_defs)))
   `(,(Pair id_defs (Null)) ,cont ,store ,(Σ* Σ_3 scps_p scps_u))
   ev-slbsv]

  ;; create macro definition binding
  [`(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-bind-syntaxes _)
              ,(Pair (? id? id_arg) (Null))
              ,(? stx? stx_arg) ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with (values stx_arg2) := (add ph (flip ph stx_arg maybe-scp_i) scp_defs)
   (InExpand (ζ (Stxξ (add1 ph) stx_arg2 (init-ξ))
                 '◯ '● (Σ* Σ ∅ ∅))
             `(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
                      `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                              `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,Σ*_0))
   ev-slbsm]

  [(InExpand (ζ stx_exp '● '● (Σ* Σ_2 _ _))
             `(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
                      `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                              `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,_))
   #:with                  ast_exp :=<1> (parse (add1 ph) stx_exp Σ_2)
   #:with (values loc_new store_1) :=    (push-cont store lbl cont)
   `(,(AstEnv ph ast_exp (init-env) 'no-scope ξ)
     ,(KApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                    `((0 . ,scps_p) (1 . ,scps_u)))
              (,id_arg) ,(Defs scp_defs 𝓁)) '() loc_new)
     ,store_1 ,(Σ* Σ_2 scps_p ∅))
   ev-slbsm2]

  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                    `((0 . ,scps_p) (1 . ,scps_u)))
              (,(? id? id_arg)) ,(Defs scp_defs 𝓁) ,val_exp) '())
     ,cont ,store ,(Σ* Σ _ _))
   #:with               ξ_defs :=<1> (def-ξ-lookup Σ 𝓁)
   #:with              id_defs :=    (add ph
                                          (prune ph (flip ph id_arg maybe-scp_i)
                                                 scps_u)
                                          scp_defs)
   #:with (values nam_new Σ_2) :=    (alloc-name id_defs Σ)
   #:with                  Σ_3 :=    (bind ph Σ_2 id_defs nam_new)
   #:with                 Σ*_4 :=    (Σ* (def-ξ-update Σ_3 𝓁
                                             (extend-ξ ξ_defs nam_new val_exp))
                                           scps_p scps_u)
   `(,(Pair id_defs (Null)) ,cont ,store ,Σ*_4)
   ev-slbsm3]

  ;; local expand
  [`(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'local-expand _)
              ,(? stx? stx) ,val_contextv ,val_idstops) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_unstops :=    (make-immutable-hash
                            (map (λ (p) (cons (car p) (unstop (cdr p))))
                                 (hash->list ξ)))
   #:with nams_stop :=<1> (resolve* ph (lst->list val_idstops) Σ)
   #:with  ats_stop :=<1> (lookup-ξ* ξ_unstops nams_stop)
   #:with   ξ_stops :=    (extend-ξ*
                            ξ_unstops
                            (map (λ (n at) (cons n (TStop at)))
                                 nams_stop
                                 ats_stop))
   (InExpand
    (ζ (Stxξ ph (flip ph stx maybe-scp_i) ξ_stops) '◯ '● Σ*_0)
    `(,(SApp lbl `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) '())
      ,cont ,store ,Σ*_0))
   ev-lexpand]  

  [(InExpand (ζ stx_exp '● '● Σ*)
             `(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) '())
               ,cont ,store ,_))
   `(,(flip ph stx_exp maybe-scp_i) ,cont ,store ,Σ*)
   ev-lexpand2]

  ;; local expand with definition context
  ;; - similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [`(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'local-expand _)
              ,(? stx? stx) ,val_contextv ,val_idstops ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with    ξ_defs :=<1> (def-ξ-lookup Σ 𝓁)
   #:with ξ_unstops :=    (make-immutable-hash
                            (map (λ (p) (cons (car p) (unstop (cdr p))))
                                 (hash->list ξ_defs)))
   #:with nams_stop :=<1> (resolve* ph (lst->list val_idstops) Σ)
   #:with  ats_stop :=<1> (lookup-ξ* ξ_unstops nams_stop)
   #:with   ξ_stops :=    (extend-ξ*
                            ξ_unstops
                            (map (λ (n at) (cons n (TStop at)))
                                 nams_stop
                                 ats_stop))
   ; TODO?: (flip ph stx scp_i)は間違い？？しかしdefsを使わない場合にも
   ; これはある．．．これがあると，少なくともunit-4が通らない．
   ; しかし，flipないとdefs-begin-with-defnの挙動が実際の処理系と異なってしまう．
   (InExpand
    (ζ (Stxξ ph (add ph (flip ph stx maybe-scp_i) scp_defs)
               ξ_stops) '◯ '● Σ*_0)
    `(,(SApp lbl `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) `())
      ,cont ,store ,Σ*_0))
   ev-lexpand-defs]

  ;; ----------------------------------------
  ;; Including boxes lets us implement recursive definitions as a
  ;; macro, including variable definitions that are in a recursive
  ;; binding scope with macros.

  ;; box
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'box stx) ,val) '()) ,cont ,store ,(Σ* Σ scps_p scps_u))
   #:with (values 𝓁 Σ_1) := (alloc-box stx Σ)
   `(,𝓁 ,cont ,store ,(Σ* (box-update Σ_1 𝓁 val) scps_p scps_u))
   ev-box]

  ;; unbox
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'unbox _)
              ,(? 𝓁? 𝓁)) '()) ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with val :=<1> (box-lookup Σ 𝓁)
   `(,val ,cont ,store ,Σ*_0)
   ev-unbox]

  ;; set-box!
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'set-box! _)
              ,(? 𝓁? 𝓁) ,val) '()) ,cont ,store ,(Σ* Σ scps_p scps_u))
   `(,val ,cont ,store ,(Σ* (box-update Σ 𝓁 val) scps_p scps_u))
   ev-set-box!]

  ;; β
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ) vals '()) ,cont ,store ,Σ*)
   #:when (and (pair? vals) (VFun? (car vals)))
   #:with (cons (VFun vars ast env) vals) := vals
   #:with                            nams := (map Var-nam vars)
   #:with           (values locs store_1) := (alloc-loc* nams store)
   #:with                         env_new := (extend-env* env vars locs)
   #:with                         store_2 := (update-store* store_1 locs vals)
   `(,(AstEnv ph ast env_new maybe-scp_i ξ) ,cont ,store_2 ,Σ*)
   ev-β]

  ;; primitive application (except StxPrim)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ) vals '()) ,cont ,store ,Σ*)
   #:when (and (pair? vals) (Prim? (car vals))
               (not (stx-prim? (Prim-nam (car vals)))))
   #:with val :=<1> (δ (car vals) (cdr vals))
   `(,val ,cont ,store ,Σ*)
   ev-δ]

  ;; if
  [`(,(SIf lbl (? (λ (x) (not (val? x))) ser_test) tm_then tm_else)
     ,cont ,store ,Σ*)
   #:with (values loc_new store_1) := (push-cont store lbl cont)
   `(,ser_test ,(KIf lbl tm_then tm_else loc_new) ,store_1 ,Σ*)
   ev-push-if]

  [`(,(? val? val) ,(KIf lbl tm_then tm_else loc_cont) ,store ,Σ*)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SIf lbl val tm_then tm_else) ,cont ,store ,Σ*)
   ev-pop-if]

  [`(,(SIf _lbl (Bool #f) _ tm_else) ,cont ,store ,Σ*)
   `(,tm_else ,cont ,store ,Σ*)
   ev-if-#f]

  [`(,(SIf _lbl (? val? val) tm_then _) ,cont ,store ,Σ*)
   #:when (not (equal? val (Bool #f)))
   `(,tm_then ,cont ,store ,Σ*)
   ev-if-#t]

  ;; in-expand
  [(InExpand ζ1 s0)
   #:with ζ2 <- (lift ((==>) ζ1)) ;; extra call due to mut. rec. defs
   (InExpand ζ2 s0)
   ex-in-expand])

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

  ;; eval : Ph Ast MaybeScp ξ Σ* → (Values Val Σ*)
  (define (eval δ ph ast maybe-scp_i ξ Σ*)
    (define -->δ (--> δ))
    (match-let ([(set `(,(? val? val) ● ,_store ,Σ*_2))
                 (apply-reduction*
                  (-->δ) `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                           ● ,(init-store) ,Σ*))])
      (values val Σ*_2)))

  ;; evaluate : Ast → Val
  (define (evaluate δ ast)
    (call-with-values
     (λ () (eval δ 0 ast 'no-scope (init-ξ) (Σ* (init-Σ) ∅ ∅)))
     (λ (val Σ*) val))))
