#lang racket
(require "../../interp/set.rkt"
         (only-in "../../interp/reduction.rkt"
                  reducer-of
                  define-parameterized-extended-reduction-relation
                  apply-reduction-relation*)
         "../../interp/full/struct.rkt"
         "../../interp/core/delta.rkt"
         (only-in "../../interp/core/syntax.rkt" zip unzip snoc union)
         (only-in "../../interp/phases/syntax.rkt"
                  empty-ctx add flip prune at-phase)
         (only-in "../../interp/full/syntax.rkt" in-hole)
         (only-in "../../interp/core/eval.rkt"
                  init-env update-env lookup-env init-store)
         (only-in "../../interp/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ)
         (only-in "../../interp/phases/expand.rkt"
                  id-seq id-kont id-snoc stx-nil)
         (only-in "../../interp/full/expand-eval.rkt"
                  extend-ξ* unstop
                  [-->f/store interp:-->f/store]
                  [==>f/Σ interp:==>f/Σ]
                  eval/--> expand/==>)

         ;; Abstract version
         (only-in "../core/eval.rkt"
                  lookup-store update-store* alloc-loc* push-cont)
         (only-in "../core/expand.rkt" alloc-name alloc-scope)
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/expand.rkt" regist-vars)
         (only-in "../phases/syntax.rkt" bind resolve)
         (only-in "syntax.rkt" resolve*/resolve id=?))
(provide (all-defined-out))


;; ----------------------------------------
;; Box allocations and updates:

;(: alloc-box : Σ -> (Values 𝓁 Σ))
(define (alloc-box Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (𝓁 (string->symbol (format "b:~a" size)))
            (Σ (add1 size) tbl))))

;(: box-lookup : Σ 𝓁 -> Val)
(define (box-lookup Σ 𝓁)
  (hash-ref (Σ-tbl Σ) 𝓁))

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
(define (def-ξ-lookup Σ0 𝓁)
  (hash-ref (Σ-tbl Σ0) 𝓁))

;(: def-ξ-update : Σ 𝓁 ξ -> Σ)
(define (def-ξ-update Σ0 𝓁 ξ)
  (match-let ([(Σ size tbl) Σ0])
    (Σ size (hash-set tbl 𝓁 ξ))))


(define-parameterized-extended-reduction-relation
  (-->f/store lookup-store update-store* alloc-loc* push-cont
              alloc-box box-lookup box-update
              alloc-def-ξ def-ξ-lookup def-ξ-update
              bind resolve alloc-name alloc-scope
              parse ==>f)
  (interp:-->f/store lookup-store update-store* alloc-loc* push-cont
                     alloc-box box-lookup box-update
                     alloc-def-ξ def-ξ-lookup def-ξ-update
                     bind resolve alloc-name alloc-scope
                     parse ==>f)

  ;; reference
  [`(,(AstEnv ph (? Var? var) env maybe-scp_i ξ) ,cont ,store ,Σ*)
   #:with val <- (lookup-store store (lookup-env env var))
   `(,(AstEnv ph val env maybe-scp_i ξ) ,cont ,store ,Σ*)
   ev-x]

  ;; application
  [`(,(? Val? val) ,(KApp `(,ph ,maybe-scp_i ,ξ) vals clos loc_cont) ,store ,Σ*)
   #:with cont <- (lookup-store store loc_cont)
   `(,(SApp `(,ph ,maybe-scp_i ,ξ) (append vals (list val)) clos)
     ,cont ,store ,Σ*)
   ev-pop-app]

  ;; local value
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(syntax-local-value ,(? Id? id)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with nam <- (resolve ph id Σ)
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
   #:with ξ_defs := (def-ξ-lookup Σ 𝓁)
   #:with nam <- (resolve ph id Σ)
   `(,(lookup-ξ ξ_defs nam) ,cont ,store ,Σ*_0)
   ev-lval-defs]

  [(InExpand (ζ stx_exp '• '• Θ_new (Σ* Σ_2 _ _))
             `(,(SApp `(,ph ,maybe-scp_i ,ξ)
                      `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                                 `((0 . ,scps_p) (1 . ,scps_u)))
                        (,id_arg) ,(Defs scp_defs 𝓁)) '())
               ,cont ,store ,_))
   #:with ast_exp <- (parse (add1 ph) stx_exp Σ_2)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,(AstEnv ph ast_exp (init-env) 'no-scope ξ)
       ,(KApp `(,ph ,maybe-scp_i ,ξ)
              `(,(GenStx (Sym 'syntax-local-bind-syntaxes2)
                         `((0 . ,scps_p) (1 . ,scps_u)))
                (,id_arg) ,(Defs scp_defs 𝓁)) '() loc_new)
       ,store_1 ,(Σ* Σ_2 scps_p (set))))
   ev-slbcm2]


  ;; local expand
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(local-expand ,(? Stx? stx) ,val_contextv ,val_idstops) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_unstops := (make-immutable-hash
                         (map (λ (p) (cons (car p) (unstop (cdr p))))
                              (hash->list ξ)))
   #:with nams_stop <- ((resolve*/resolve resolve) ph val_idstops Σ)
   #:with ξ_stops := (extend-ξ*
                       ξ_unstops
                       (map (λ (n) (cons n (TStop (lookup-ξ ξ_unstops n))))
                            nams_stop))
   (InExpand
    (ζ (Stxξ ph (flip ph stx maybe-scp_i) ξ_stops) '∘ '• (init-Θ) Σ*_0)
    `(,(SApp `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) `())
      ,cont ,store ,Σ*_0))
   ev-lexpand]

  ;; local expand with definition context
  ;; - similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [`(,(SApp `(,ph ,maybe-scp_i ,ξ)
            `(local-expand ,(? Stx? stx) ,val_contextv ,val_idstops
                           ,(Defs scp_defs 𝓁)) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_defs := (def-ξ-lookup Σ 𝓁)
   #:with ξ_unstops := (make-immutable-hash
                         (map (λ (p) (cons (car p) (unstop (cdr p))))
                              (hash->list ξ_defs)))
   #:with nams_stop <- ((resolve*/resolve resolve) ph val_idstops Σ)
   #:with ξ_stops := (extend-ξ*
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

  ;; if
  [`(,(? Val? val) ,(KIf tm_then tm_else loc_cont) ,store ,Σ*)
   #:with cont <- (lookup-store store loc_cont)
   `(,(SIf val tm_then tm_else) ,cont ,store ,Σ*)
   ev-pop-if])


(define-parameterized-extended-reduction-relation 
  (==>f/Σ bind resolve id=? alloc-name alloc-scope regist-vars parse -->f)
  (interp:==>f/Σ bind resolve id=? alloc-name alloc-scope regist-vars parse -->f)

  ;; stops
  [(ζ (Stxξ ph (and stx (GenStx `(,(? Id? id_stop)
                                    ,@stl_args) ctx)) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam_stop <- (resolve ph id_stop Σ)
   #:when (TStop? (lookup-ξ ξ nam_stop))
   (ζ stx '• κ Θ Σ*_0)
   ex-stop]


  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ)
                 ,(GenStx #f ctx_new)) ctx)
       '∘ κ Θ (Σ* Σ scps_p _))
   #:when (and (id=? ph id_kont '#%kont     ξ Σ)
               (id=? ph id_ls   'let-syntax ξ Σ))
   #:with nam_new <- (resolve ph id_new Σ)
   #:with ast_exp <- (parse (add1 ph) stx_exp Σ)
   (InEval `(,(AstEnv ph ast_exp (init-env) 'no-scope ξ)
             • ,(init-store) ,(Σ* Σ scps_p (set)))
           (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                         ,(Stxξ ph stx_body ξ)
                         ,(GenStx #f ctx_new)) (empty-ctx))
               '∘ κ Θ (Σ* Σ scps_p (set))))
   ex-ls-eval]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with nam_mac <- (resolve ph id_mac Σ)
   #:with val := (lookup-ξ ξ nam_mac)
   #:when (Val? val)
   (let*-values ([(scp_u Σ_1) (alloc-scope 'u Σ)]
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

  ;; application (same as phases)
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (Id? stx_fun)
   #:with name <- (resolve ph stx_fun Σ)
   #:when (let* ([at (lookup-ξ ξ name)])
            (or (TVar? at)
                (and (eq? 'not-found at)
                     (not (member name
                                  '(lambda let quote syntax let-syntax if
                                     #%app #%kont #%seq #%ls-kont
                                     #%snoc))))))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-app]

  ;; primitive application (NEW)
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ)
       '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (not (Id? stx_fun))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
         Θ_1 (Σ* Σ scps_p (set))))
   ex-prim-app]

  ;; reference (same as phases)
  [(ζ (Stxξ ph (and id (GenStx (Sym nam) ctx)) ξ)
       '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam <- (resolve ph id Σ)
   #:with val := (lookup-ξ ξ nam)
   #:when (TVar? val)
   (match-let ([(TVar id_new) val]) (ζ id_new '• κ Θ Σ*_0))
   ex-var])


(define-values (-->f ==>f)
  (letrec ([-->f (λ () ((reducer-of -->f/store)
                         lookup-store update-store* alloc-loc* push-cont
                         alloc-box box-lookup box-update
                         alloc-def-ξ def-ξ-lookup def-ξ-update
                         bind resolve alloc-name alloc-scope
                         parse ==>f))]
           [==>f (λ () ((reducer-of ==>f/Σ)
                         bind resolve id=? alloc-name alloc-scope regist-vars
                         parse -->f))])
    (values (-->f) (==>f))))

;(: eval : Ph Ast MaybeScp ξ Σ* -> (Setof (Cons Val Σ*)))
(define ((eval/--> -->) ph ast maybe-scp_i ξ Σ*)
  (match-let ([(set `(,(? Val? val) • ,_store ,Σ*_2) ...)
               (apply-reduction-relation*
                --> `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                      • ,(init-store) ,Σ*))])
    (list->set (map cons val Σ*_2))))

;(: expand : Ph Stx ξ Σ* -> (Setof (Cons Stx Σ*)))
(define ((expand/==> ==>) ph stx ξ Σ*)
  (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• (init-Θ) Σ*)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ*_new) ...)
                 (apply-reduction-relation* ==> init-ζ)])
      (list->set (map cons stx_new Σ*_new)))))

(define eval (eval/--> -->f))
(define expand (expand/==> ==>f))
