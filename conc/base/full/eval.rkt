#lang racket
(require
 "../../../reduction.rkt"
 "../../../set.rkt"
 (only-in "../../../term.rkt" use-terms)
 (only-in "../../../dprint.rkt" dprint)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^)
 (only-in "terms.rkt" terms^ #%term-forms))
(provide --> eval@)

;; --> : State -> (Setof State)
(define-reduction (--> delta ==> :=<1>)
  #:within-signatures [(only terms^
                             Var% Fun% App% If% VFun% Sym% Stx% KApp% KIf%
                             SApp% SIf% AstEnv% TVar% TStop% Defs% Stxξ%
                             Σ% Σ*% 𝓁% ζ% InExpand%)
                       (only terms-extra^
                             val? id? prim? stx-prim?)
                       (only syntax^
                             add flip union prune)
                       (only env^
                             init-env lookup-env update-env)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-scope)
                       (only bind^
                             bind resolve)
                       (only mcont^
                             init-Θ)
                       (only parser^
                             parse)]
  #:do [(use-terms Var Fun App If VFun Sym Stx KApp KIf SApp SIf AstEnv
                   TVar TStop Defs Stxξ Σ Σ* 𝓁 ζ InExpand)
        ;; resolve* : Ph (Listof Id) Σ -> (Listof Nam))
        (define (resolve* ph val Σ)
          (match val
            ['() '()]
            [(cons id val2) (cons (resolve #:phase ph id Σ)
                                  (resolve* ph val2 Σ))]))

        ;; extend-ξ* : ξ (Listof (Pairof Nam AllTransform)) -> ξ
        (define (extend-ξ* ξ nas)
          (if (null? nas)
              ξ
              (hash-set (extend-ξ* ξ (cdr nas)) (caar nas) (cdar nas))))

        ;; unstop : AllTransform -> AllTransform
        (define (unstop all-transform)
          (match all-transform
            [(TStop all-transform2) all-transform2]
            [_ all-transform]))

        ;; ----------------------------------------
        ;; Definition-context environment allocations and updates:

        ;; alloc-def-ξ : Σ -> (Values 𝓁 Σ)
        (define (alloc-def-ξ Σ0)
          (dprint 'full 'alloc-def-ξ "")
          (match-let ([(Σ size tbl) Σ0])
            (values (𝓁 (string->symbol (format "ξ:~a" size)))
                    (Σ (add1 size) tbl))))

        ;; def-ξ-lookup : Σ 𝓁 -> ξ
        (define (def-ξ-lookup Σ0 𝓁)
          (dprint 'full 'def-ξ-lookup "")
          (hash-ref (Σ-tbl Σ0) 𝓁))

        ;; def-ξ-update : Σ 𝓁 ξ -> Σ
        (define (def-ξ-update Σ0 𝓁 ξ)
          (dprint 'full 'def-ξ-update "")
          (match-let ([(Σ size tbl) Σ0])
            (Σ size (hash-set tbl 𝓁 ξ))))

        ;; ----------------------------------------
        ;; Box allocations and updates:

        ;; alloc-box : Σ -> (Values 𝓁 Σ)
        (define (alloc-box Σ0)
          (dprint 'full 'alloc-box "")
          (match-let ([(Σ size tbl) Σ0])
            (values (𝓁 (string->symbol (format "b:~a" size)))
                    (Σ (add1 size) tbl))))

        ;; box-lookup : Σ 𝓁 -> Val
        (define (box-lookup Σ 𝓁)
          (dprint 'full 'box-lookup "")
          (hash-ref (Σ-tbl Σ) 𝓁))

        ;; box-update : Σ 𝓁 Val -> Σ
        (define (box-update Σ0 𝓁0 val)
          (dprint 'full 'box-update "")
          (match-let ([(Σ size binds) Σ0])
            (Σ size (hash-set binds 𝓁0 val))))]

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
  [`(,(AstEnv ph (? val? val) env maybe-scp_i ξ) ,cont ,store ,Σ*)
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

  [`(,(? val? val) ,(KApp `(,ph ,maybe-scp_i ,ξ) vals clos loc_cont) ,store ,Σ*)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SApp `(,ph ,maybe-scp_i ,ξ) (append vals (list val)) clos)
     ,cont ,store ,Σ*)
   ev-pop-app]

  ;; local value
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-value ,(? id? id)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with nam :=<1> (resolve #:phase ph id Σ)
   `(,(lookup-ξ ξ nam) ,cont ,store ,Σ*_0)
   ev-lval]

  ;; local value with definition context
  ;; - similar to the basic local value case, but using definition
  ;;   context's environment
  ;; - Unlike the fourth argument to local-expand, the scopes associated with
  ;;   the provided definition contexts are not used to enrich id's
  ;;   lexical information.
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-value ,(? id? id) #f ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_defs :=    (def-ξ-lookup Σ 𝓁)
   #:with    nam :=<1> (resolve #:phase ph id Σ)
   `(,(lookup-ξ ξ_defs nam) ,cont ,store ,Σ*_0)
   ev-lval-defs]

  ;; local binder
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-identifier-as-binding ,(? id? id)) '())
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
              (,(? id? id_arg)) #f ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with              id_defs := (add ph
                                       (prune ph (flip ph id_arg maybe-scp_i)
                                              scps_u)
                                       scp_defs)
   #:with (values nam_new Σ_1) := (alloc-name id_defs Σ)
   #:with                  Σ_2 := (bind #:phase ph Σ_1 id_defs nam_new)
   #:with               ξ_defs := (def-ξ-lookup Σ_2 𝓁)
   #:with                  Σ_3 := (def-ξ-update Σ_2 𝓁
                                     (extend-ξ ξ_defs nam_new (TVar id_defs)))
   `((,id_defs) ,cont ,store ,(Σ* Σ_3 scps_p scps_u))
   ev-slbcv]

  ;; create macro definition binding
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-bind-syntaxes
              (,(? id? id_arg)) ,(? Stx? stx_arg) ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with (values stx_arg2) := (add ph (flip ph stx_arg maybe-scp_i) scp_defs)
   (InExpand (ζ (Stxξ (add1 ph) stx_arg2 (init-ξ))
                 '∘ '• (init-Θ) (Σ* Σ (set) (set)))
             `(,(SApp `(,ph ,maybe-scp_i ,ξ)
                      `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                              `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,Σ*_0))
   ev-slbcm]

  [(InExpand (ζ stx_exp '• '• Θ_new (Σ* Σ_2 _ _))
             `(,(SApp `(,ph ,maybe-scp_i ,ξ)
                      `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                              `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,_))
   #:with                  ast_exp :=<1> (parse #:phase (add1 ph) stx_exp Σ_2)
   #:with (values loc_new store_1) :=    (push-cont store cont)
   `(,(AstEnv ph ast_exp (init-env) 'no-scope ξ)
     ,(KApp `(,ph ,maybe-scp_i ,ξ)
            `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                    `((0 . ,scps_p) (1 . ,scps_u)))
              (,id_arg) ,(Defs scp_defs 𝓁)) '() loc_new)
     ,store_1 ,(Σ* Σ_2 scps_p (set)))
   ev-slbcm2]

  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(,(Stx (Sym 'syntax-local-bind-syntaxes2)
                    `((0 . ,scps_p) (1 . ,scps_u)))
              (,(? id? id_arg)) ,(Defs scp_defs 𝓁) ,val_exp) '())
     ,cont ,store ,(Σ* Σ _ _))
   #:with               ξ_defs := (def-ξ-lookup Σ 𝓁)
   #:with              id_defs := (add ph (prune ph (flip ph id_arg maybe-scp_i)
                                                 scps_u)
                                       scp_defs)
   #:with (values nam_new Σ_2) := (alloc-name id_defs Σ)
   #:with                  Σ_3 := (bind #:phase ph Σ_2 id_defs nam_new)
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
   #:with nams_stop :=<1> (resolve* ph val_idstops Σ)
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
   #:with nams_stop :=<1> (resolve* ph val_idstops Σ)
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
   #:when (and (pair? vals) (prim? (car vals)) (not (stx-prim? (car vals))))
   `(,(delta (car vals) (cdr vals)) ,cont ,store ,Σ*)
   ev-delta]

  ;; if
  [`(,(SIf (? (λ (x) (not (val? x))) ser_test) tm_then tm_else)
     ,cont ,store ,Σ*)
   #:with (values loc_new store_1) := (push-cont store cont)
   `(,ser_test ,(KIf tm_then tm_else loc_new) ,store_1 ,Σ*)
   ev-push-if]

  [`(,(? val? val) ,(KIf tm_then tm_else loc_cont) ,store ,Σ*)
   #:with cont :=<1> (lookup-store store loc_cont)
   `(,(SIf val tm_then tm_else) ,cont ,store ,Σ*)
   ev-pop-if]

  [`(,(SIf #f _ tm_else) ,cont ,store ,Σ*)
   `(,tm_else ,cont ,store ,Σ*)
   ev-if-#f]

  [`(,(SIf (? val? val) tm_then _) ,cont ,store ,Σ*)
   #:when (not (equal? val #f))
   `(,tm_then ,cont ,store ,Σ*)
   ev-if-#t]

  ;; in-expand
  [(InExpand ζ1 s0)
   #:with ζ2 <- (lift ((==>) ζ1)) ;; extra call due to mut. rec. defs
   (InExpand ζ2 s0)
   ex-in-expand])

(define-unit-from-reduction red@ -->)

(define-unit eval/red@
  (import (only terms^
                AstEnv% Σ*%)
          (only terms-extra^
                val?)
          (only delta^
                delta)
          (only env^
                init-env)
          (only store^
                init-store)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only expand^
                ==>)
          (only red^
                reducer))
  (export eval^)

  (use-terms AstEnv Σ*)

  (define --> (λ () (reducer delta ==> :=)))

  ; eval : Ph Ast MaybeScp ξ Σ* -> (Values Val Σ*)
  (define (eval ph ast maybe-scp_i ξ Σ*)
    (match-let ([(set `(,(? val? val) • ,_store ,Σ*_2))
                 (apply-reduction-relation*
                  (-->) `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                          • ,(init-store) ,Σ*))])
      (values val Σ*_2)))

  ; evaluate : Ast -> Val
  (define (evaluate ast)
    (call-with-values
     (λ () (eval 0 ast 'no-scope (init-ξ) (Σ* (init-Σ) (set) (set))))
     (λ (val Σ*) val))))

(define-compound-unit/infer eval@
  (import terms^ terms-extra^ syntax^ env^ store^ cont^ delta^
          menv^ mstore^ bind^ mcont^ parser^ expand^)
  (export eval^)
  (link   red@ eval/red@))
