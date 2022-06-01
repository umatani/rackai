#lang racket
(require "../set.rkt" "../dprint.rkt" "../reduction.rkt"
         (only-in "../core/delta.rkt" delta)
         (only-in "../core/syntax.rkt" zip unzip snoc union)
         (only-in "../core/expand.rkt"
                  init-ξ lookup-ξ extend-ξ alloc-name alloc-scope
                  push-κ lookup-κ init-Θ)
         (only-in "../core/eval.rkt"
                  init-env lookup-env update-env
                  init-store lookup-store update-store*
                  alloc-loc* push-cont)

         (only-in "../phases/syntax.rkt"
                  empty-ctx add flip prune bind at-phase resolve)
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/expand.rkt"
                  regist-vars id-seq id-kont id-snoc stx-nil)

         "struct.rkt"
         (only-in "syntax.rkt" in-hole resolve*/resolve  id=?))
(provide (all-defined-out))

;(: extend-ξ* : ξ (Listof (Pairof Nam AllTransform)) -> ξ)
(define (extend-ξ* ξ nas)
  (if (null? nas)
      ξ
      (hash-set (extend-ξ* ξ (cdr nas)) (caar nas) (cdar nas))))

;(: unstop : AllTransform -> AllTransform)
(define (unstop all-transform)
  (match all-transform
    [(TStop all-transform2) all-transform2]
    [_ all-transform]))

;; ----------------------------------------
;; Box allocations and updates:

;(: alloc-box : Σ -> (Values 𝓁 Σ))
(define (alloc-box Σ0)
  (dprint 'full 'alloc-box "")
  (match-let ([(Σ size tbl) Σ0])
    (values (𝓁 (string->symbol (format "b:~a" size)))
            (Σ (add1 size) tbl))))

;(: box-lookup : Σ 𝓁 -> Val)
(define (box-lookup Σ 𝓁)
  (dprint 'full 'box-lookup "")
  (hash-ref (Σ-tbl Σ) 𝓁))

;(: box-update : Σ 𝓁 Val -> Σ)
(define (box-update Σ0 𝓁0 val)
  (dprint 'full 'box-update "")
  (match-let ([(Σ size binds) Σ0])
    (Σ size (hash-set binds 𝓁0 val))))

;; ----------------------------------------
;; Definition-context environment allocations and updates:

;(: alloc-def-ξ : Σ -> (Values 𝓁 Σ))
(define (alloc-def-ξ Σ0)
  (dprint 'full 'alloc-def-ξ "")
  (match-let ([(Σ size tbl) Σ0])
    (values (𝓁 (string->symbol (format "ξ:~a" size)))
            (Σ (add1 size) tbl))))

;(: def-ξ-lookup : Σ 𝓁 -> ξ)
(define (def-ξ-lookup Σ0 𝓁)
  (dprint 'full 'def-ξ-lookup "")
  (hash-ref (Σ-tbl Σ0) 𝓁))

;(: def-ξ-update : Σ 𝓁 ξ -> Σ)
(define (def-ξ-update Σ0 𝓁 ξ)
  (dprint 'full 'def-ξ-update "")
  (match-let ([(Σ size tbl) Σ0])
    (Σ size (hash-set tbl 𝓁 ξ))))


;; (: -->f : State -> (Setof State))
(define-parameterized-reduction-relation (-->f/store delta ==>f :=<1>)

  ;; propagate env into subterms
  [`(,(AstEnv ph (If ast_test ast_then ast_else) env maybe-scp_i ξ)
     ,cont ,store ,Σ*)
   `(,(SIf (AstEnv ph ast_test env maybe-scp_i ξ)
           (AstEnv ph ast_then env maybe-scp_i ξ)
           (AstEnv ph ast_else env maybe-scp_i ξ)) ,cont ,store ,Σ*)
   ev-env-if]

  [`(,(AstEnv ph (App ast_fun ast_args) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   `(,(SApp `(,ph ,maybe-scp_i ,ξ)
            '()
            (cons (AstEnv ph ast_fun env maybe-scp_i ξ)
                  (map (λ (arg) (AstEnv ph arg env maybe-scp_i ξ))
                       ast_args)))
     ,cont ,store ,Σ*)
   ev-env-app]

  ;; value
  [`(,(AstEnv ph (? Val? val) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   `(,val ,cont ,store ,Σ*)
   ev-val]

  ;; reference
  [`(,(AstEnv ph (? Var? var) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   #:with val :=<1> (lookup-store store (lookup-env env var))
   `(,(AstEnv ph val env maybe-scp_i ξ) ,cont ,store ,Σ*)
   ev-x]

  ;; lambda
  [`(,(AstEnv ph (Fun vars ast) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   `(,(AstEnv ph (VFun vars ast env) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   ev-lam]

  ;; application
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(,vals ...) `(,tm ,tms ...)) ,cont ,store ,Σ*)
   #:with (values loc_new store_1) := (push-cont store cont)
   `(,tm ,(KApp `(,ph ,maybe-scp_i ,ξ) vals  tms loc_new) ,store_1 ,Σ*)
   ev-push-app]

  [`(,(? Val? val) ,(KApp `(,ph ,maybe-scp_i ,ξ) vals clos loc_cont) ,store ,Σ*)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SApp `(,ph ,maybe-scp_i ,ξ) (append vals (list val)) clos)
     ,cont ,store ,Σ*)
   ev-pop-app]

  ;; local value
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-value ,(? Id? id)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with nam :=<1> (resolve ph id Σ)
   `(,(lookup-ξ ξ nam) ,cont ,store ,Σ*_0)
   ev-lval]

  ;; local value with definition context
  ;; - similar to the basic local value case, but using definition
  ;;   context's environment
  ;; - Unlike the fourth argument to local-expand, the scopes associated with
  ;;   the provided definition contexts are not used to enrich id's
  ;;   lexical information.
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-value ,(? Id? id) #f ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_defs :=    (def-ξ-lookup Σ 𝓁)
   #:with    nam :=<1> (resolve ph id Σ)
   `(,(lookup-ξ ξ_defs nam) ,cont ,store ,Σ*_0)
   ev-lval-defs]

  ;; local binder
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-identifier-as-binding ,(? Id? id)) '())
     ,cont ,store ,(and Σ*_0 (Σ* _ _ scps_u)))
   `(,(prune ph id scps_u) ,cont ,store ,Σ*_0)
   ev-lbinder]

  ;; create definition context
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-make-definition-context) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with (values scp_defs Σ_2) := (alloc-scope 'defs Σ)
   #:with        (values 𝓁 Σ_3) := (alloc-def-ξ Σ_2)
   #:with                  Σ*_3 := (Σ* (def-ξ-update Σ_3 𝓁 ξ)
                                       (union (set scp_defs) scps_p)
                                       scps_u)
   `(,(Defs scp_defs 𝓁) ,cont ,store ,Σ*_3)
   ev-slmdc]

  ;; create definition binding (for a variable)
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-bind-syntaxes
              (,(? Id? id_arg)) #f ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with              id_defs := (add ph
                                       (prune ph (flip ph id_arg maybe-scp_i)
                                              scps_u)
                                       scp_defs)
   #:with (values nam_new Σ_1) := (alloc-name id_defs Σ)
   #:with                  Σ_2 := (bind ph Σ_1 id_defs nam_new)
   #:with               ξ_defs := (def-ξ-lookup Σ_2 𝓁)
   #:with                  Σ_3 := (def-ξ-update Σ_2 𝓁
                                    (extend-ξ ξ_defs nam_new (TVar id_defs)))
   `((,id_defs) ,cont ,store ,(Σ* Σ_3 scps_p scps_u))
   ev-slbcv]

  ;; create macro definition binding
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-bind-syntaxes
              (,(? Id? id_arg)) ,(? Stx? stx_arg) ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with (values stx_arg2) := (add ph (flip ph stx_arg maybe-scp_i) scp_defs)
   (InExpand (ζ (Stxξ (add1 ph) stx_arg2 (init-ξ))
                '∘ '• (init-Θ) (Σ* Σ (set) (set)))
             `(,(SApp `(,ph ,maybe-scp_i ,ξ)
                      `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                                 `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,Σ*_0))
   ev-slbcm]

  [(InExpand (ζ stx_exp '• '• Θ_new (Σ* Σ_2 _ _))
             `(,(SApp `(,ph ,maybe-scp_i ,ξ)
                      `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                                 `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,_))
   #:with                  ast_exp :=<1> (parse (add1 ph) stx_exp Σ_2)
   #:with (values loc_new store_1) :=    (push-cont store cont)
   `(,(AstEnv ph ast_exp (init-env) 'no-scope ξ)
     ,(KApp `(,ph ,maybe-scp_i ,ξ)
            `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                       `((0 . ,scps_p) (1 . ,scps_u)))
              (,id_arg) ,(Defs scp_defs 𝓁)) '() loc_new)
     ,store_1 ,(Σ* Σ_2 scps_p (set)))
   ev-slbcm2]

  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                       `((0 . ,scps_p) (1 . ,scps_u)))
              (,(? Id? id_arg)) ,(Defs scp_defs 𝓁) ,val_exp) '())
     ,cont ,store ,(Σ* Σ _ _))
   #:with               ξ_defs := (def-ξ-lookup Σ 𝓁)
   #:with              id_defs := (add ph (prune ph (flip ph id_arg maybe-scp_i)
                                                 scps_u)
                                       scp_defs)
   #:with (values nam_new Σ_2) := (alloc-name id_defs Σ)
   #:with                  Σ_3 := (bind ph Σ_2 id_defs nam_new)
   #:with                 Σ*_4 := (Σ* (def-ξ-update Σ_3 𝓁
                                          (extend-ξ ξ_defs nam_new val_exp))
                                        scps_p scps_u)
   `((,id_defs) ,cont ,store ,Σ*_4)
   ev-slbcm3]

  ;; local expand
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(local-expand ,(? Stx? stx) ,val_contextv ,val_idstops) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_unstops :=    (make-immutable-hash
                            (map (λ (p) (cons (car p) (unstop (cdr p))))
                                 (hash->list ξ)))
   #:with nams_stop :=<1> ((resolve*/resolve resolve) ph val_idstops Σ)
   #:with   ξ_stops :=    (extend-ξ*
                            ξ_unstops
                            (map (λ (n) (cons n (TStop (lookup-ξ ξ_unstops n))))
                                 nams_stop))
   (InExpand
    (ζ (Stxξ ph (flip ph stx maybe-scp_i) ξ_stops) '∘ '• (init-Θ) Σ*_0)
    `(,(SApp `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) `())
      ,cont ,store ,Σ*_0))
   ev-lexpand]  

  [(InExpand (ζ stx_exp '• '• Θ_new Σ*)
             `(,(SApp `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) `())
               ,cont ,store ,_))
   `(,(flip ph stx_exp maybe-scp_i) ,cont ,store ,Σ*)
   ev-lexpand2]

  ;; local expand with definition context
  ;; - similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(local-expand ,(? Stx? stx) ,val_contextv ,val_idstops
                           ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with    ξ_defs :=    (def-ξ-lookup Σ 𝓁)
   #:with ξ_unstops :=    (make-immutable-hash
                            (map (λ (p) (cons (car p) (unstop (cdr p))))
                                 (hash->list ξ_defs)))
   #:with nams_stop :=<1> ((resolve*/resolve resolve) ph val_idstops Σ)
   #:with   ξ_stops :=    (extend-ξ*
                            ξ_unstops
                            (map (λ (n) (cons n (TStop (lookup-ξ ξ_unstops n))))
                                 nams_stop))
   ; TODO?: (flip ph stx scp_i)は間違い？？しかしdefsを使わない場合にも
   ; これはある．．．これがあると，少なくともunit-4が通らない．
   ; しかし，flipないとdefs-begin-with-defnの挙動が実際の処理系と異なってしまう．
   (InExpand
    (ζ (Stxξ ph (add ph (flip ph stx maybe-scp_i) scp_defs)
               ξ_stops) '∘ '• (init-Θ) Σ*_0)
    `(,(SApp `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) `())
      ,cont ,store ,Σ*_0))
   ev-lexpand-defs]

  ;; ----------------------------------------
  ;; Including boxes lets us implement recursive definitions as a
  ;; macro, including variable definitions that are in a recursive
  ;; binding scope with macros.

  ;; box
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(box ,val) '()) ,cont ,store ,(Σ* Σ scps_p scps_u))
   #:with (values 𝓁 Σ_1) := (alloc-box Σ)
   `(,𝓁 ,cont ,store ,(Σ* (box-update Σ_1 𝓁 val) scps_p scps_u))
   ev-box]

  ;; unbox
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(unbox ,(? 𝓁? 𝓁)) '()) ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   `(,(box-lookup Σ 𝓁) ,cont ,store ,Σ*_0)
   ev-unbox]

  ;; set-box!
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(set-box! ,(? 𝓁? 𝓁) ,val) '()) ,cont ,store ,(Σ* Σ scps_p scps_u))
   `(,val ,cont ,store ,(Σ* (box-update Σ 𝓁 val) scps_p scps_u))
   ev-set-box!]

  ;; β
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ) vals '()) ,cont ,store ,Σ*)
   #:when (and (pair? vals) (VFun? (car vals)))
   #:with (cons (VFun vars ast env) vals) := vals
   #:with                            nams := (map Var-nam vars)
   #:with           (values locs store_1) := (alloc-loc* nams store)
   #:with                         env_new := (update-env env vars locs)
   #:with                         store_2 := (update-store* store_1 locs vals)
   `(,(AstEnv ph ast env_new maybe-scp_i ξ) ,cont ,store_2 ,Σ*)
   ev-β]

  ;; primitive application (except StxPrim)
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ) vals '()) ,cont ,store ,Σ*)
   #:when (and (pair? vals) (Prim? (car vals)) (not (StxPrim? (car vals))))
   `(,(delta (car vals) (cdr vals)) ,cont ,store ,Σ*)
   ev-delta]

  ;; if
  [`(,(SIf (? (λ (x) (not (Val? x))) ser_test) tm_then tm_else)
     ,cont ,store ,Σ*)
   #:with (values loc_new store_1) := (push-cont store cont)
   `(,ser_test ,(KIf tm_then tm_else loc_new) ,store_1 ,Σ*)
   ev-push-if]

  [`(,(? Val? val) ,(KIf tm_then tm_else loc_cont) ,store ,Σ*)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SIf val tm_then tm_else) ,cont ,store ,Σ*)
   ev-pop-if]

  [`(,(SIf #f _ tm_else) ,cont ,store ,Σ*)
   `(,tm_else ,cont ,store ,Σ*)
   ev-if-#f]

  [`(,(SIf (? Val? val) tm_then _) ,cont ,store ,Σ*)
   #:when (not (equal? val #f))
   `(,tm_then ,cont ,store ,Σ*)
   ev-if-#t]

  ;; in-expand
  [(InExpand ζ1 s0)
   #:with ζ2 <- (lift ((==>f) ζ1)) ;; extra call due to mut. rec. defs
   (InExpand ζ2 s0)
   ex-in-expand])


;; (: ==>f : ζ -> (Setof ζ))
(define-parameterized-reduction-relation (==>f/Σ -->f :=<1>)

  ;; stops
  [(ζ (Stxξ ph (and stx (GenStx `(,(? Id? id_stop)
                                    ,@stl_args) ctx)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam_stop :=<1> (resolve ph id_stop Σ)
   #:when (TStop? (lookup-ξ ξ nam_stop))
   (ζ stx '• κ Θ Σ*_0)
   ex-stop]

  ;; lambda (same as phases)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_lam)
                           ,(GenStx (? ProperStl? stl_args) ctx_0)
                           ,stx_body) ctx)
              ξ) '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_lam 'lambda ξ Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars ph scp_new stl_args ξ Σ_1)
   #:with                         Σ*_2 := (Σ* Σ_2
                                                (union (set scp_new) scps_p)
                                                (set))
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '∘
       (κ (GenStx `(,id_lam
                     ,(GenStx stl_args2 ctx_0)
                     ,(Hole)) ctx) '• Σ*_0 𝓁_new) Θ_1 Σ*_2)
   ex-lam-body]

  ;; let
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_let)
                           ,(GenStx (? ProperStl? stl_binds) ctx_1)
                           ,stx_body) ctx)
              ξ) '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_let 'let ξ Σ)
   #:with (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars ph scp_new stl_vars ξ Σ_1)
   #:with Σ*_2 := (Σ* Σ_2 (union (set scp_new) scps_p) (set))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '∘
       (κ (GenStx `(,id-kont
                     ,id_let
                     ,(Stxξ ph (GenStx `(,(GenStx stl_vars2 ctx_1)
                                          ,(GenStx stl_rhs ctx_1))
                                        ctx_1) ξ)
                     ,(Hole)) ctx) '∘ Σ*_0 𝓁_new) Θ_1 Σ*_2)
   ex-let-body]
  
  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_let)
                 ,(Stxξ ph (GenStx
                             `(,(GenStx (? ProperStl? stl_vars) _)
                               ,(GenStx (? ProperStl? stl_rhs) _)) ctx_1)
                         ξ) ,stx_body) ctx) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (and (id=? ph id_kont '#%kont ξ Σ)
               (id=? ph id_let  'let    ξ Σ))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ) '∘
       (κ (Stxξ ph (GenStx
                      `(,id_kont
                        ,id_let
                        ,(GenStx `(,(GenStx stl_vars ctx_1) ,(Hole)) ctx_1)
                        ,stx_body) ctx)
                  ξ) '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-let-rhs]  

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                           ,(? Id? id_let)
                           ,(GenStx `(,(GenStx (? ProperStl? stl_vars) _)
                                      ,(GenStx (? ProperStl? val_rhs) _)) ctx_1)
                           ,stx_body) ctx)
              ξ) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? ph id_kont '#%kont ξ Σ) (id=? ph id_let  'let    ξ Σ))
   (ζ (GenStx `(,id_let ,(GenStx (zip stl_vars val_rhs ctx_1) ctx_1)
                         ,stx_body) ctx) '• κ Θ Σ*_0)
   ex-let-rhs2]

  ;; quote (same as phases)
  [(ζ (Stxξ ph (and stx (GenStx `(,(? Id? id_quote) ,_) _)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_quote 'quote ξ Σ)
   (ζ stx '• κ Θ Σ*_0)
   ex-quote]

  ;; syntax (same as phases)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_syntax) ,stx) ctx) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_syntax 'syntax ξ Σ)
   #:with stx_pruned := (prune ph stx scps_p)
   (ζ (GenStx `(,id_syntax ,stx_pruned) ctx) '• κ Θ Σ*_0)
   ex-stx]

  ;; macro creation (eval gets more and updates store)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_ls)
                           ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                           ,stx_body) ctx) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_ls 'let-syntax ξ Σ)
   (ζ (GenStx `(,id_ls
                 ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ)) ctx) '∘ κ Θ Σ*_0)
   ex-ξ-ls]

  [(ζ (GenStx `(,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id) ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ)) ctx) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_ls 'let-syntax ξ Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ)
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add ph id scp_new)
   #:with                  Σ_3 := (bind ph Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ)) '∘
       (κ (GenStx `(,id-kont
                     ,id_ls
                     ,(GenStx `(,(GenStx `(,id_new ,(Hole)) ctx_0)) ctx_1)
                     ,(Stxξ ph stx_body ξ)
                     ,(GenStx #f (list (cons ph (set scp_new))))) ctx)
           '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ_3 (set) (set)))
   ex-ls-push-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ)
                 ,(GenStx #f ctx_new)) ctx) '∘ κ Θ (Σ* Σ scps_p _))
   #:when (and (id=? ph id_kont '#%kont     ξ Σ)
               (id=? ph id_ls   'let-syntax ξ Σ))
   #:with nam_new :=<1> (resolve ph id_new Σ)
   #:with ast_exp :=<1> (parse (add1 ph) stx_exp Σ)
   (InEval `(,(AstEnv ph ast_exp (init-env) 'no-scope ξ)
             • ,(init-store) ,(Σ* Σ scps_p (set)))
           (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                         ,(Stxξ ph stx_body ξ)
                         ,(GenStx #f ctx_new)) (empty-ctx)) '∘
               κ Θ (Σ* Σ scps_p (set))))
   ex-ls-eval]

  [(InEval `(,(? Val? val) • ,store_0 ,(Σ* Σ _ _))
           (ζ (GenStx `(,(GenStx (Sym nam_new) _)
                         ,(Stxξ ph stx_body ξ)
                         ,(GenStx #f ctx_new)) _) '∘ κ Θ (Σ* _ scps_p _)))
   #:with scp_new   := (car (set->list (at-phase ctx_new ph)))
   #:with ξ_new     := (extend-ξ ξ nam_new val)
   #:with stx_body2 := (add ph stx_body scp_new)
   (ζ (Stxξ ph stx_body2 ξ_new) '∘
       κ Θ (Σ* Σ (union (set scp_new) scps_p) (set)))
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with            nam_mac :=<1> (resolve ph id_mac Σ)
   #:with                val :=    (lookup-ξ ξ nam_mac)
   #:when (Val? val)
   #:with (values scp_u Σ_1) :=    (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) :=    (alloc-scope 'i Σ_1)
   #:with               Σ*_2 :=    (Σ* Σ_2
                                         (union (set scp_u) scps_p)
                                         (union (set scp_u) scps_u))
   #:with        stx_macapp2 :=    (flip ph (add ph stx_macapp scp_u) scp_i)
   (InEval
    `(,(AstEnv ph (App val (list stx_macapp2))
               (init-env) scp_i ξ)
      • ,(init-store) ,Σ*_2)
    (ζ (Stxξ ph (GenStx #f (list (cons ph (set scp_i)))) ξ)
        '∘ κ Θ Σ*_2)) ;; Σ*_2 not used
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0 ,Σ*)
           (ζ (Stxξ ph (GenStx #f ctx_i) ξ) '∘ κ Θ _))
   #:with scp_i := (car (set->list (at-phase ctx_i ph)))
   ;(printf "after expand: ~a\n" stx_exp)
   (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ) '∘ κ Θ Σ*)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_if) ,stl_exps ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_if 'if ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ) '∘
       (κ (Stxξ ph (GenStx `(,id-kont ,id_if ,(Hole)) ctx) ξ)
           '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-if]

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                           ,(? Id? id_if)
                           ,(GenStx (? ProperStl? val_exps) ctx)) _)
              ξ) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? ph id_kont '#%kont ξ Σ) (id=? ph id_if 'if ξ Σ))
   (ζ (GenStx `(,id_if ,@val_exps) ctx) '• κ Θ Σ*_0)
   ex-if-kont]

  ;; application (non-canonical #%app version, same as phases)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_app)
                           ,stx_fun ,stl_args ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_app '#%app ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-#%app]

  ;; application (canonical #%app version, same as phases)
  [(ζ (Stxξ ph (GenStx
                  (cons (? Id? id_app)
                        (GenStx `(,stx_fun ,stl_args ...) _)) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_app '#%app ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-#%app2]

  ;; application (same as phases)
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (Id? stx_fun)
   #:with name :=<1> (resolve ph stx_fun Σ)
   #:with   at :=    (lookup-ξ ξ name)
   #:when (or (TVar? at)
              (and (eq? 'not-found at)
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))
   #:with             id_app := (GenStx (Sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-app]

  ;; primitive application
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (not (Id? stx_fun))
   #:with             id_app := (GenStx (Sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-prim-app]

  ;; reference (same as phases)
  [(ζ (Stxξ ph (and id (GenStx (Sym nam) ctx)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam :=<1> (resolve ph id Σ)
   #:with val :=    (lookup-ξ ξ nam)
   #:when (TVar? val)
   #:with (TVar id_new) := val
   (ζ id_new '• κ Θ Σ*_0)
   ex-var]

  ;; literal (same as phases)
  [(ζ (Stxξ ph (GenStx (? Atom? atom) ctx) ξ) '∘ κ Θ Σ*)
   #:when (not (Id? (GenStx atom ctx)))
   (ζ (GenStx `(,(GenStx (Sym 'quote) ctx) ,(GenStx atom ctx)) ctx) '• κ Θ Σ*)
   ex-lit]

  ;; pop κ (merge Σ*)
  [(ζ stx '• (κ stx_c ex? (Σ* _ scps_p scps_u) 𝓁) Θ (Σ* Σ _ _))
   #:with κ0 := (lookup-κ Θ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 Θ (Σ* Σ scps_p scps_u))
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                           ,(GenStx (? ProperStl? val_dones) _)
                           ,stx_exp0 ,stl_exps ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? ph id_seq '#%seq ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph stx_exp0 ξ) '∘
       (κ
        (GenStx
         `(,(Stxξ ph id_seq ξ)
           ,(GenStx `(,id-snoc ,(GenStx val_dones (empty-ctx)) ,(Hole))
                    (empty-ctx))
           ,@stl_exps) ctx) '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-seq-cons]

  [(ζ (GenStx `(,(Stxξ ph (? Id? id_seq) ξ)
                 ,(GenStx `(,(? Id? id_snoc)
                            ,(GenStx (? ProperStl? val_dones) ctx_1)
                            ,(? Stx? stx_done)) _)
                 ,stl_exps ...) ctx) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? ph id_seq  '#%seq  ξ Σ) (id=? ph id_snoc '#%snoc ξ Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ ph (GenStx `(,id_seq ,(GenStx val_dones2 ctx_1)
                                   ,@stl_exps) ctx) ξ) '∘ κ Θ Σ*_0)
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                           ,(GenStx (? ProperStl? val_dones) _)) ctx) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? ph id_seq '#%seq ξ Σ)
   (ζ (GenStx val_dones ctx) '• κ Θ Σ*_0)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift ((-->f) s1)) ;; extra call due to mut. rec. defs
   (InEval s2 ζ0)
   ex-in-eval])

(define-values (-->f ==>f)
  (letrec ([-->f (λ () ((reducer-of -->f/store) delta ==>f :=))]
           [==>f (λ () ((reducer-of ==>f/Σ) -->f :=))])
    (values (-->f) (==>f))))


;(: eval : Ph Ast MaybeScp ξ Σ* -> (Values Val Σ*))
(define ((eval/--> -->) ph ast maybe-scp_i ξ Σ*)
  (match-let ([(set `(,(? Val? val) • ,_store ,Σ*_2))
               (apply-reduction-relation*
                --> `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                      • ,(init-store) ,Σ*))])
    (values val Σ*_2)))

(define eval (eval/--> -->f))

;(: expand : Ph Stx ξ Σ* -> (Cons Stx Σ*))
(define ((expand/==> ==>) ph stx ξ Σ*)
  (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• (init-Θ) Σ*)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ*_new))
                 (apply-reduction-relation* ==> init-ζ)])
      (cons stx_new Σ*_new))))

(define expand (expand/==> ==>f))
