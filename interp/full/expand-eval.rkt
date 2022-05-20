#lang racket
(require "../reduction.rkt"
         (only-in "../core/delta.rkt" δ)
         (only-in "../core/conf.rkt" init-env init-store init-ξ init-Θ)
         (only-in "../core/syntax.rkt" zip unzip snoc union)
         (only-in "../core/expand.rkt"
                  lookup-ξ extend-ξ alloc-name alloc-scope
                  push-κ lookup-κ)
         (only-in "../core/eval.rkt"
                  lookup-store update-store* lookup-env update-env alloc-loc*
                  push-cont)
         (only-in "../phases/conf.rkt" empty-ctx)
         (only-in "../phases/syntax.rkt"
                  add flip prune bind at-phase resolve)
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/expand.rkt"
                  regist-vars id-seq id-kont id-snoc stx-nil)
         "struct.rkt"
         "syntax.rkt")
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
  (match-let ([(Σ size tbl) Σ0])
    (values (𝓁 (string->symbol (format "b:~a" size)))
            (Σ (add1 size) tbl))))

;(: box-lookup : Σ 𝓁 -> Val)
(define (box-lookup Σ 𝓁) (hash-ref (Σ-tbl Σ) 𝓁))

;(: box-update : Σ 𝓁 Val -> Σ)
(define (box-update Σ0 𝓁0 val)
  (match-let ([(Σ size binds) Σ0])
    (Σ size (hash-set binds 𝓁0 val))))

;; ----------------------------------------
;; Definition-context environment allocations and updates:

;(: alloc-def-ξ : Σ -> (Values 𝓁 Σ))
(define (alloc-def-ξ Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (𝓁 (string->symbol (format "ξ:~a" size)))
            (Σ (add1 size) tbl))))

;(: def-ξ-lookup : Σ 𝓁 -> ξ)
(define (def-ξ-lookup Σ0 𝓁) (hash-ref (Σ-tbl Σ0) 𝓁))

;(: def-ξ-update : Σ 𝓁 ξ -> Σ)
(define (def-ξ-update Σ0 𝓁 ξ)
  (match-let ([(Σ size tbl) Σ0])
    (Σ size (hash-set tbl 𝓁 ξ))))


;; (: -->f : State -> (Setof State))
(define-reduction-relation -->f

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
   `(,(AstEnv ph (lookup-store store (lookup-env env var))
              env maybe-scp_i ξ) ,cont ,store ,Σ*)
   ev-x]

  ;; lambda
  [`(,(AstEnv ph (Fun vars ast) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   `(,(AstEnv ph (VFun vars ast env) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   ev-lam]

  ;; application
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(,vals ...) `(,tm ,tms ...)) ,cont ,store ,Σ*)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,tm ,(KApp `(,ph ,maybe-scp_i ,ξ) vals  tms loc_new) ,store_1 ,Σ*))
   ev-push-app]

  [`(,(? Val? val) ,(KApp `(,ph ,maybe-scp_i ,ξ) vals clos loc_cont) ,store ,Σ*)
   `(,(SApp `(,ph ,maybe-scp_i ,ξ) (append vals (list val)) clos)
     ,(lookup-store store loc_cont) ,store ,Σ*)
   ev-pop-app]

  ;; local value
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-value ,(? Id? id)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   `(,(lookup-ξ ξ (resolve ph id Σ)) ,cont ,store ,Σ*_0)
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
   (let ([ξ_defs (def-ξ-lookup Σ 𝓁)])
     ;(printf "lvalue-defs nam: ~a\n" (resolve ph id Σ))
     ;(printf "           keys: ~a\n" (hash-keys ξ_defs))
     `(,(lookup-ξ ξ_defs (resolve ph id Σ)) ,cont ,store ,Σ*_0))
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
   (let*-values ([(scp_defs Σ_2) (alloc-scope 'defs Σ)]
                 [(𝓁 Σ_3) (alloc-def-ξ Σ_2)]
                 [(Σ*_3) (Σ* (def-ξ-update Σ_3 𝓁 ξ)
                               (union (set scp_defs) scps_p)
                               scps_u)])
     `(,(Defs scp_defs 𝓁) ,cont ,store ,Σ*_3))
   ev-slmdc]

  ;; create definition binding (for a variable)
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-bind-syntaxes
              (,(? Id? id_arg)) #f ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   (let*-values ([(id_defs)
                  (add ph (prune ph (flip ph id_arg maybe-scp_i) scps_u)
                       scp_defs)]
                 [(nam_new Σ_1) (alloc-name id_defs Σ)]
                 [(Σ_2) (bind ph Σ_1 id_defs nam_new)]
                 [(ξ_defs) (def-ξ-lookup Σ_2 𝓁)]
                 [(Σ_3) (def-ξ-update Σ_2 𝓁
                           (extend-ξ ξ_defs nam_new (TVar id_defs)))])
     `((,id_defs) ,cont ,store ,(Σ* Σ_3 scps_p scps_u)))
   ev-slbcv]

  ;; create macro definition binding
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-bind-syntaxes
              (,(? Id? id_arg)) ,(? Stx? stx_arg) ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   (let*-values ([(stx_arg2) (add ph (flip ph stx_arg maybe-scp_i) scp_defs)])
     (InExpand (ζ (Stxξ (add1 ph) stx_arg2 (init-ξ))
                   '∘ '• (init-Θ) (Σ* Σ (set) (set)))
               `(,(SApp `(,ph ,maybe-scp_i ,ξ)
                        `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                                   `((0 . ,scps_p) (1 . ,scps_u)))
                          (,id_arg) ,(Defs scp_defs 𝓁)) '())
                 ,cont ,store ,Σ*_0)))
   ev-slbcm]

  [(InExpand (ζ stx_exp '• '• Θ_new (Σ* Σ_2 _ _))
             `(,(SApp `(,ph ,maybe-scp_i ,ξ)
                      `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                                 `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,_))
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,(AstEnv ph (parse (add1 ph) stx_exp Σ_2) (init-env) 'no-scope ξ)
       ,(KApp `(,ph ,maybe-scp_i ,ξ)
              `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                         `((0 . ,scps_p) (1 . ,scps_u)))
                (,id_arg) ,(Defs scp_defs 𝓁)) '() loc_new)
       ,store_1 ,(Σ* Σ_2 scps_p (set))))
   ev-slbcm2]

  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                       `((0 . ,scps_p) (1 . ,scps_u)))
              (,(? Id? id_arg)) ,(Defs scp_defs 𝓁) ,val_exp) '())
     ,cont ,store ,(Σ* Σ _ _))
   (let*-values ([(ξ_defs) (def-ξ-lookup Σ 𝓁)]
                 [(id_defs) (add ph (prune ph (flip ph id_arg maybe-scp_i)
                                           scps_u)
                                 scp_defs)]
                 [(nam_new Σ_2) (alloc-name id_defs Σ)]
                 [(Σ_3) (bind ph Σ_2 id_defs nam_new)]
                 [(Σ*_4) (Σ* (def-ξ-update Σ_3 𝓁
                                 (extend-ξ ξ_defs nam_new val_exp))
                               scps_p scps_u)])
     `((,id_defs) ,cont ,store ,Σ*_4))
   ev-slbcm3]

  ;; local expand
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(local-expand ,(? Stx? stx) ,val_contextv ,val_idstops) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   (let* ([ξ_unstops (make-immutable-hash
                       (map (λ (p) (cons (car p) (unstop (cdr p))))
                            (hash->list ξ)))]
          [nams_stop (resolve* ph val_idstops Σ)]
          [ξ_stops (extend-ξ*
                     ξ_unstops
                     (map (λ (n) (cons n (TStop (lookup-ξ ξ_unstops n))))
                          nams_stop))])
     (InExpand
      (ζ (Stxξ ph (flip ph stx maybe-scp_i) ξ_stops) '∘ '• (init-Θ) Σ*_0)
      `(,(SApp `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) `())
        ,cont ,store ,Σ*_0)))
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
   (let* ([ξ_defs (def-ξ-lookup Σ 𝓁)]
          [ξ_unstops (make-immutable-hash
                       (map (λ (p) (cons (car p) (unstop (cdr p))))
                            (hash->list ξ_defs)))]
          [nams_stop (resolve* ph val_idstops Σ)]
          [ξ_stops (extend-ξ*
                     ξ_unstops
                     (map (λ (n) (cons n (TStop (lookup-ξ ξ_unstops n))))
                          nams_stop))])
     ; TODO?: (flip ph stx scp_i)は間違い？？しかしdefsを使わない場合にも
     ; これはある．．．これがあると，少なくともunit-4が通らない．
     ; しかし，flipないとdefs-begin-with-defnの挙動が実際の処理系と異なってしまう．
     (InExpand
      (ζ (Stxξ ph (add ph (flip ph stx maybe-scp_i) scp_defs)
                 ξ_stops) '∘ '• (init-Θ) Σ*_0)
      `(,(SApp `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) `())
        ,cont ,store ,Σ*_0)))
   ev-lexpand-defs]

  ;; ----------------------------------------
  ;; Including boxes lets us implement recursive definitions as a
  ;; macro, including variable definitions that are in a recursive
  ;; binding scope with macros.

  ;; box
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(box ,val) '()) ,cont ,store ,(Σ* Σ scps_p scps_u))
   (let-values ([(𝓁 Σ_1) (alloc-box Σ)])
     `(,𝓁 ,cont ,store ,(Σ* (box-update Σ_1 𝓁 val) scps_p scps_u)))
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
   #:when (and (pair? vals)
               (VFun? (car vals)))
   (let*-values ([(vars ast env vals) (let ([f(car vals)])
                                        (values (VFun-vars f)
                                                (VFun-ast f)
                                                (VFun-env f)
                                                (cdr vals)))]
                 [(nams) (map Var-nam vars)]
                 [(locs store_1) (alloc-loc* nams store)]
                 [(env_new) (update-env env vars locs)]
                 [(store_2) (update-store* store_1 locs vals)])
     `(,(AstEnv ph ast env_new maybe-scp_i ξ) ,cont ,store_2 ,Σ*))
   ev-β]

  ;; primitive application (except StxPrim)
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ) vals '()) ,cont ,store ,Σ*)
   #:when (and (pair? vals) (Prim? (car vals)) (not (StxPrim? (car vals))))
   `(,(δ (car vals) (cdr vals)) ,cont ,store ,Σ*)
   ev-δ]

  ;; if
  [`(,(SIf (? (λ (x) (not (Val? x))) ser_test) tm_then tm_else)
     ,cont ,store ,Σ*)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,ser_test ,(KIf tm_then tm_else loc_new) ,store_1 ,Σ*))
   ev-push-if]

  [`(,(? Val? val) ,(KIf tm_then tm_else loc_cont) ,store ,Σ*)
   `(,(SIf val tm_then tm_else)
     ,(lookup-store store loc_cont) ,store ,Σ*)
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
   #:with ((reducer-of ==>f) ζ1)
   (λ (ζ2) (InExpand ζ2 s0))
   ex-in-expand])


;(: eval : Ph Ast MaybeScp ξ Σ* -> (Values Val Σ*))
(define (eval ph ast maybe-scp_i ξ Σ*)
  (match-let ([`((,(? Val? val) • ,_store ,Σ*_2))
               (apply-reduction-relation*
                (reducer-of -->f)
                `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                  • ,(init-store) ,Σ*))])
    (values val Σ*_2)))


;; (: ==>f : ζ -> (Setof ζ))
(define-reduction-relation ==>f

  ;; stops
  [(ζ (Stxξ ph (and stx (GenStx `(,(? Id? id_stop)
                                    ,@stl_args) ctx)) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (TStop? (lookup-ξ ξ (resolve ph id_stop Σ)))
   (ζ stx '• κ Θ Σ*_0)
   ex-stop]

  ;; lambda (same as phases)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_lam)
                           ,(GenStx (? ProperStl? stl_args) ctx_0)
                           ,stx_body) ctx)
              ξ) '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (let ([nam (resolve ph id_lam Σ)])
            (and (eq? 'lambda nam) (not (TStop? (lookup-ξ ξ nam)))))
   (let*-values ([(scp_new Σ_1) (alloc-scope 'lam Σ)]
                 [(stl_args2 ξ_new Σ_2)
                  (regist-vars ph scp_new stl_args ξ Σ_1)]
                 [(Σ*_2) (Σ* Σ_2 (union (set scp_new) scps_p) (set))]
                 [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new)
         '∘
         (κ (GenStx `(,id_lam
                       ,(GenStx stl_args2 ctx_0)
                       ,(Hole)) ctx)
             '• Σ*_0 𝓁_new)
         Θ_1 Σ*_2))
   ex-lam-body]

  ;; let
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_let)
                           ,(GenStx (? ProperStl? stl_binds) ctx_1)
                           ,stx_body) ctx)
              ξ) '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (let ([nam (resolve ph id_let Σ)])
            (and (eq? 'let nam) (not (TStop? (lookup-ξ ξ nam)))))
   (let*-values ([(stl_vars stl_rhs) (unzip stl_binds)]
                 [(scp_new Σ_1) (alloc-scope 'let Σ)]
                 [(stl_vars2 ξ_new Σ_2) (regist-vars ph scp_new stl_vars ξ Σ_1)]
                 [(Σ*_2) (Σ* Σ_2 (union (set scp_new) scps_p) (set))]
                 [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new)
         '∘
         (κ (GenStx `(,id-kont
                       ,id_let
                       ,(Stxξ ph (GenStx `(,(GenStx stl_vars2 ctx_1)
                                            ,(GenStx stl_rhs ctx_1))
                                          ctx_1) ξ)
                       ,(Hole)) ctx) '∘ Σ*_0 𝓁_new)
         Θ_1 Σ*_2))
   ex-let-body]
  
  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_let)
                 ,(Stxξ ph (GenStx
                             `(,(GenStx (? ProperStl? stl_vars) _)
                               ,(GenStx (? ProperStl? stl_rhs) _)) ctx_1)
                         ξ) ,stx_body) ctx)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (and (eq? '#%kont (resolve ph id_kont Σ))
               (eq? 'let (resolve ph id_let Σ)))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ)
         '∘
         (κ (Stxξ ph (GenStx
                        `(,id_kont
                          ,id_let
                          ,(GenStx `(,(GenStx stl_vars ctx_1) ,(Hole)) ctx_1)
                          ,stx_body) ctx)
                 ξ) '∘ Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-let-rhs]  

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                           ,(? Id? id_let)
                           ,(GenStx `(,(GenStx (? ProperStl? stl_vars) _)
                                      ,(GenStx (? ProperStl? val_rhs) _)) ctx_1)
                           ,stx_body) ctx)
              ξ) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (eq? '#%kont (resolve ph id_kont Σ))
               (eq? 'let (resolve ph id_let Σ)))
   (ζ (GenStx `(,id_let ,(GenStx (zip stl_vars val_rhs ctx_1) ctx_1)
                         ,stx_body) ctx)
       '• κ Θ Σ*_0)
   ex-let-rhs2]

  ;; quote (same as phases)
  [(ζ (Stxξ ph (and stx (GenStx `(,(? Id? id_quote) ,_) _)) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (let ([nam (resolve ph id_quote Σ)])
            (and (eq? 'quote nam) (not (TStop? (lookup-ξ ξ nam)))))

   (ζ stx '• κ Θ Σ*_0)
   ex-quote]

  ;; syntax (same as phases)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_syntax) ,stx) ctx) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (let ([nam (resolve ph id_syntax Σ)])
            (and (eq? 'syntax nam) (not (TStop? (lookup-ξ ξ nam)))))
   (let ([stx_pruned (prune ph stx scps_p)])
     (ζ (GenStx `(,id_syntax ,stx_pruned) ctx) '• κ Θ Σ*_0))
   ex-stx]

  ;; macro creation (eval gets more and updates store)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_ls)
                           ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                           ,stx_body) ctx) ξ
              ) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (let ([nam (resolve ph id_ls Σ)])
            (and (eq? 'let-syntax nam) (not (TStop? (lookup-ξ ξ nam)))))
   (ζ (GenStx `(,id_ls
                 ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ)) ctx)
       '∘ κ Θ Σ*_0)
   ex-ξ-ls]

  [(ζ (GenStx `(,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id) ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ)) ctx)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (let ([nam (resolve ph id_ls Σ)])
            (and (eq? 'let-syntax nam) (not (TStop? (lookup-ξ ξ nam)))))
   (let*-values ([(nam_new Σ_1) (alloc-name id Σ)]
                 [(scp_new Σ_2) (alloc-scope 'ls Σ_1)]
                 [(id_new) (add ph id scp_new)]
                 [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                 [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (add1 ph) stx_rhs (init-ξ))
         '∘
         (κ (GenStx `(,id-kont
                       ,id_ls
                       ,(GenStx `(,(GenStx `(,id_new ,(Hole)) ctx_0)) ctx_1)
                       ,(Stxξ ph stx_body ξ)
                       ,(GenStx #f (list (cons ph (set scp_new))))) ctx)
             '∘ Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ_3 (set) (set))))
   ex-ls-push-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ)
                 ,(GenStx #f ctx_new)) ctx)
       '∘ κ Θ (Σ* Σ scps_p _))
   #:when (and (eq? '#%kont     (resolve ph id_kont Σ))
               (eq? 'let-syntax (resolve ph id_ls Σ)))
   (let ([nam_new (resolve ph id_new Σ)])
     (InEval `(,(AstEnv ph (parse (add1 ph) stx_exp Σ) (init-env) 'no-scope ξ)
               • ,(init-store) ,(Σ* Σ scps_p (set)))
             (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                           ,(Stxξ ph stx_body ξ)
                           ,(GenStx #f ctx_new)) (empty-ctx))
                 '∘ κ Θ (Σ* Σ scps_p (set)))))
   ex-ls-eval]

  [(InEval `(,(? Val? val) • ,store_0 ,(Σ* Σ _ _))
           (ζ (GenStx `(,(GenStx (Sym nam_new) _)
                         ,(Stxξ ph stx_body ξ)
                         ,(GenStx #f ctx_new)) _)
               '∘ κ Θ (Σ* _ scps_p _)))
   (let* ([scp_new (car (set->list (at-phase ctx_new ph)))]
          [ξ_new (extend-ξ ξ nam_new val)]
          [stx_body2 (add ph stx_body scp_new)])
     (ζ (Stxξ ph stx_body2 ξ_new)
         '∘ κ Θ (Σ* Σ (union (set scp_new) scps_p) (set))))
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:when (Val? (lookup-ξ ξ (resolve ph id_mac Σ)))
   (let*-values ([(val) (lookup-ξ ξ (resolve ph id_mac Σ))]
                 [(scp_u Σ_1) (alloc-scope 'u Σ)]
                 [(scp_i Σ_2) (alloc-scope 'i Σ_1)]
                 [(Σ*_2) (Σ* Σ_2
                               (union (set scp_u) scps_p)
                               (union (set scp_u) scps_u))]
                 [(stx_macapp2) (flip ph (add ph stx_macapp scp_u) scp_i)])
     (InEval
      `(,(AstEnv ph (App val (list stx_macapp2))
                 (init-env) scp_i ξ)
        • ,(init-store) ,Σ*_2)
      (ζ (Stxξ ph (GenStx #f (list (cons ph (set scp_i)))) ξ)
          '∘ κ Θ Σ*_2))) ;; Σ*_2 not used
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0 ,Σ*)
           (ζ (Stxξ ph (GenStx #f ctx_i) ξ) '∘ κ Θ _))
   (let ([scp_i (car (set->list (at-phase ctx_i ph)))])
     ;(printf "after expand: ~a\n" stx_exp)
     (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ) '∘ κ Θ Σ*))
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_if) ,stl_exps ...) ctx) ξ)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (let ([nam (resolve ph id_if Σ)])
            (and (eq? 'if nam) (not (TStop? (lookup-ξ ξ nam)))))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ)
         '∘
         (κ (Stxξ ph (GenStx `(,id-kont ,id_if ,(Hole)) ctx) ξ)
             '∘ Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-if]

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                           ,(? Id? id_if)
                           ,(GenStx (? ProperStl? val_exps) ctx)) _)
              ξ) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (eq? '#%kont (resolve ph id_kont Σ))
               (eq? 'if     (resolve ph id_if Σ)))
   (ζ (GenStx `(,id_if ,@val_exps) ctx) '• κ Θ Σ*_0)
   ex-if-kont]

  ;; application (non-canonical #%app version, same as phases)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_app)
                           ,stx_fun ,stl_args ...) ctx) ξ)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (let ([nam (resolve ph id_app Σ)])
            (and (eq? '#%app nam) (not (TStop? (lookup-ξ ξ nam)))))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-#%app]

  ;; application (canonical #%app version, same as phases)
  [(ζ (Stxξ ph (GenStx
                  (cons (? Id? id_app)
                        (GenStx `(,stx_fun ,stl_args ...) _)) ctx) ξ)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (let ([nam (resolve ph id_app Σ)])
            (and (eq? '#%app nam) (not (TStop? (lookup-ξ ξ nam)))))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-#%app2]

  ;; application (same as phases)
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (or (not (Id? stx_fun))
              (let* ([name (resolve ph stx_fun Σ)]
                     [at (lookup-ξ ξ name)])
                (or (TVar? at)
                    (and (eq? 'not-found at)
                         (not (member name
                                      '(lambda let quote syntax let-syntax if
                                         #%app #%kont #%seq #%ls-kont
                                         #%snoc)))))))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-app]

  ;; reference (same as phases)
  [(ζ (Stxξ ph (and id (GenStx (Sym nam) ctx)) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (TVar? (lookup-ξ ξ (resolve ph id Σ)))
   (match-let ([(TVar id_new) (lookup-ξ ξ (resolve ph id Σ))])
     (ζ id_new '• κ Θ Σ*_0))
   ex-var]

  ;; literal (same as phases)
  [(ζ (Stxξ ph (GenStx (? Atom? atom) ctx) ξ) '∘ κ Θ Σ*)
   #:when (not (Id? (GenStx atom ctx)))
   (ζ (GenStx `(,(GenStx (Sym 'quote) ctx) ,(GenStx atom ctx)) ctx) '• κ Θ Σ*)
   ex-lit]

  ;; pop κ (merge Σ*)
  [(ζ stx '• (κ stx_c ex? (Σ* _ scps_p scps_u) 𝓁) Θ (Σ* Σ _ _))
   (let ([κ0 (lookup-κ Θ 𝓁)])
     (ζ (in-hole stx_c stx) ex? κ0 Θ (Σ* Σ scps_p scps_u)))
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                           ,(GenStx (? ProperStl? val_dones) _)
                           ,stx_exp0 ,stl_exps ...) ctx) ξ)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (eq? '#%seq (resolve ph id_seq Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph stx_exp0 ξ) '∘
         (κ
          (GenStx
           `(,(Stxξ ph id_seq ξ)
             ,(GenStx `(,id-snoc ,(GenStx val_dones (empty-ctx)) ,(Hole))
                      (empty-ctx))
             ,@stl_exps) ctx) '∘ Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-seq-cons]

  [(ζ (GenStx `(,(Stxξ ph (? Id? id_seq) ξ)
                 ,(GenStx `(,(? Id? id_snoc)
                            ,(GenStx (? ProperStl? val_dones) ctx_1)
                            ,(? Stx? stx_done)) _)
                 ,stl_exps ...) ctx) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (eq? '#%seq  (resolve ph id_seq Σ))
               (eq? '#%snoc (resolve ph id_snoc Σ)))
   (let ([val_dones2 (snoc val_dones stx_done)])
     (ζ (Stxξ ph (GenStx `(,id_seq ,(GenStx val_dones2 ctx_1)
                                     ,@stl_exps) ctx) ξ)
         '∘ κ Θ Σ*_0))
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                           ,(GenStx (? ProperStl? val_dones) _)) ctx) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (eq? '#%seq (resolve ph id_seq Σ))
   (ζ (GenStx val_dones ctx) '• κ Θ Σ*_0)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with ((reducer-of -->f) s1)
   (λ (s2) (InEval s2 ζ0))
   ex-in-eval])

;(: expand : Ph Stx ξ Σ* -> (Values Stx Σ*))
(define (expand ph stx ξ Σ*)
  (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• (init-Θ) Σ*)])
    (match-let ([(list (ζ stx_new '• '• Θ_new Σ*_new))
                 (apply-reduction-relation* (reducer-of ==>f) init-ζ)])
      (values stx_new Σ*_new))))