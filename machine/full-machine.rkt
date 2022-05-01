#lang racket
(require redex/reduction-semantics redex/parameter
         "common.rkt"
         (only-in "core-machine.rkt"
                  plus biggest-subset
                  init-env init-store init-ξ init-Θ init-Σ union
                  core:examples
                  [run core:run]
                  [lookup-store core:lookup-store]
                  [update-store core:update-store]
                  [update-store* core:update-store*]
                  [alloc-loc core:alloc-loc]
                  [alloc-loc* core:alloc-loc*]
                  [push-cont core:push-cont]
                  [alloc-κ core:alloc-κ]
                  [lookup-κ core:lookup-κ]
                  [update-κ core:update-κ]
                  [push-κ core:push-κ])
         (only-in "phases-machine.rkt"
                  Lph unzip zip snoc
                  δ flip add strip prune
                  bind resolve parse
                  lookup-ξ extend-ξ
                  alloc-scope alloc-name regist-vars
                  phases:examples
                  [run phases:run])
         (for-syntax racket/list
                     syntax/parse))

(define-extended-language Lfull Lph
  [maybe-scp ::= scp no-scope] ; new
  [Σ* ::= (Tup Σ scps scps)] ; new

  [val ::= ....
       (syntax-local-bind-syntaxes2 scps_p scps_u) ; new
       ]
  [cont ::=
        •
        (App (ph maybe-scp ξ) val ... hole clo ... loc) ; updated (ph maybe-scp ξ)
        (If hole clo clo loc)]
  [ser ::=
       (ph ast env maybe-scp ξ) ; updated (ph env maybe-scp ξ)
       (App (ph maybe-scp ξ) clo ...) ; updated (ph maybe-scp ξ)
       (If clo clo clo)
       (Seq clo ...) ; new
       ]
  [state ::=
         (clo cont store Σ*) ; updated (Σ*)
         (in-expand ζ state) ; new
         ]

  [stx∘ ::=
        stx
        (ph stx ξ) ; updated (remove scps)
        (Stx (Cons stx∘ stl∘) ctx)]
  [STX ::=
       hole
       (ph STX ξ)  ; updated (remove scps)
       (Stx (Cons STX stl∘) ctx)
       (Stx (Cons stx∘ STL) ctx)]
  [κ ::=
     •
     (STX ex? Σ* loc) ; updated (Σ*)
     ]
  [ζ ::=
      (stx∘ ex? κ Θ Σ*) ; updated (Σ -> Σ*)
      (in-eval state ζ)])

;; ----------------------------------------
;; Evaluating AST:

(define-extended-metafunction* core:lookup-store Lfull
  lookup-store : store loc -> u)

(define-extended-metafunction* core:update-store Lfull
  update-store : store loc u -> store)

(define-extended-metafunction* core:update-store* Lfull
  update-store* : store (loc u) ... -> store)

(define-extended-metafunction* core:alloc-loc Lfull
  alloc-loc : store -> (values loc store))

;; for eval-time value binding
(define-extended-metafunction* core:alloc-loc* Lfull
  alloc-loc* : (nam ...) store -> (values (loc ...) store))

(define-extended-metafunction* core:push-cont Lfull
  push-cont : store cont -> (values loc store))

(define-metafunction Lfull
  extend-ξ* : ξ ((nam all-transform) ...) -> ξ
  [(extend-ξ* ξ ((nam all-transform) ...)) ((nam all-transform) ... . ξ)])

(define-metafunction Lfull
  unstop : all-transform -> all-transform
  [(unstop (TStop all-transform)) all-transform]
  [(unstop all-transform) all-transform])

(define-metafunction Lfull
  resolve* : ph val Σ -> (nam ...)
  [(resolve* ph () Σ) ()]
  [(resolve* ph (Cons id val) Σ)
   ((resolve ph id Σ) nam ...)
   (where (nam ...) (resolve* ph val Σ))])


;; ----------------------------------------
;; Box allocations and updates:

(define-metafunction Lfull
  alloc-box : Σ -> (values 𝓁 Σ)
  [(alloc-box (Sto number (binds ...)))
   (values ,(string->symbol (format "b:~a" (term number)))
           (Sto ,(add1 (term number)) (binds ...)))])

(define-metafunction Lfull
  box-lookup : Σ 𝓁 -> val
  [(box-lookup (Sto _ (_ ... [𝓁 val] _ ...)) 𝓁) val])

(define-metafunction Lfull
  box-update : Σ 𝓁 val -> Σ
  [(box-update (Sto number (binds_1 ... [𝓁 _] binds_2 ...)) 𝓁 val)
   (Sto number (binds_1 ... [𝓁 val] binds_2 ...))]
  [(box-update (Sto number (binds ...)) 𝓁 val)
   (Sto number ([𝓁 val] binds ...))])

;; ----------------------------------------
;; Definition-context environment allocations and updates:

(define-metafunction Lfull
  alloc-def-ξ : Σ -> (values 𝓁 Σ)
  [(alloc-def-ξ (Sto number (binds ...)))
   (values ,(string->symbol (format "ξ:~a" (term number)))
           (Sto ,(add1 (term number)) (binds ...)))])

(define-metafunction Lfull
  def-ξ-lookup : Σ 𝓁 -> ξ
  [(def-ξ-lookup (Sto _ (_ ... [𝓁 ξ] _ ...)) 𝓁) ξ])

(define-metafunction Lfull
  def-ξ-update : Σ 𝓁 ξ -> Σ
  [(def-ξ-update (Sto number (binds_1 ... [𝓁 _] binds_2 ...)) 𝓁 ξ)
   (Sto number (binds_1 ... [𝓁 ξ] binds_2 ...))]
  [(def-ξ-update (Sto number (binds ...)) 𝓁 ξ)
   (Sto number ([𝓁 ξ] binds ...))])


(define-reduction-relation -->c Lfull
  #:domain state

  ;; propagate env into subterms
  [--> ((ph (If ast_test ast_then ast_else) env maybe-scp ξ) cont store Σ*)
       ((If (ph ast_test env maybe-scp ξ)
            (ph ast_then env maybe-scp ξ)
            (ph ast_else env maybe-scp ξ)) cont store Σ*)
       ev-env-if]

  [--> ((ph (App ast_fun ast_arg ...) env maybe-scp ξ) cont store Σ*)
       ((App (ph maybe-scp ξ)
             (ph ast_fun env maybe-scp ξ)
             (ph ast_arg env maybe-scp ξ) ...) cont store Σ*)
       ev-env-app]

  ;; value
  [--> ((ph val env maybe-scp ξ) cont store Σ*)
       (val cont store Σ*)
       ;(side-condition (printf "val: ~a\n" (term val)))
       ev-val]

  ;; reference
  [--> ((ph var env maybe-scp ξ) cont store Σ*)
       ((ph u env maybe-scp ξ) cont store Σ*)
       ;(side-condition (printf "var: ~a\n" (term var)))
       (where loc (find env var))
       (where u (lookup-store store loc))
       ;(side-condition (printf "var cont: ~a\n" (term cont)))
       ev-x]

  ;; lambda
  [--> ((ph (Fun (var ...) ast) env maybe-scp ξ) cont store Σ*)
       ((ph (Fun (var ...) ast env) env maybe-scp ξ) cont store Σ*)
       ev-lam]

  ;; application
  [--> ((App (ph maybe-scp ξ)
             val ... ser clo ...) cont store Σ*)
       (ser (App (ph maybe-scp ξ)
                 val ... hole clo ... loc_new) store_1 Σ*)
       (where (values loc_new store_1) (push-cont store cont))
       ev-push-app]

  [--> (val_0 (App (ph maybe-scp ξ)
                   val ... hole clo ... loc_cont) store Σ*)
       ((App (ph maybe-scp ξ)
             val ... val_0 clo ...) (lookup-store store loc_cont) store Σ*)
       ev-pop-app]

  ;; local value
  [--> ((App (ph maybe-scp ξ)
             syntax-local-value id) cont store Σ*)
       ((lookup-ξ ξ (resolve ph id Σ)) cont store Σ*)
       (where (Tup Σ _ _) Σ*)
       ev-lval]

  ;; local value with definition context
  ;; - similar to the basic local value case, but using definition
  ;;   context's environment
  ;; - Unlike the fourth argument to local-expand, the scopes associated with
  ;;   the provided definition contexts are not used to enrich id-stx’s
  ;;   lexical information.
  [--> ((App (ph maybe-scp ξ)
             syntax-local-value id #f (Defs scp_defs 𝓁)) cont store Σ*)
       ((lookup-ξ ξ_defs (resolve ph id Σ)) cont store Σ*)
       (where (Tup Σ _ _) Σ*)
       (where ξ_defs (def-ξ-lookup Σ 𝓁))
       ev-lval-defs]

  ;; local binder
  [--> ((App (ph maybe-scp ξ)
             syntax-local-identifier-as-binding id) cont store Σ*)
       ((prune ph id scps_u) cont store Σ*)
       (where (Tup _ _ scps_u) Σ*)
       ev-lbinder]

  ;; create definition context
  [--> ((App (ph scp_i ξ)
             syntax-local-make-definition-context) cont store Σ*)
       ((Defs scp_defs 𝓁) cont store Σ*_3)
       (where (Tup Σ scps_p scps_u) Σ*)
       (where (values scp_defs Σ_2) (alloc-scope Σ))
       (where (values 𝓁 Σ_3) (alloc-def-ξ Σ_2))
       (where Σ*_3 (Tup (def-ξ-update Σ_2 𝓁 ξ)
                         (union (Set scp_defs) scps_p)
                         scps_u))
       ev-slmdc]

  ;; create definition binding (for a variable)
  [--> ((App (ph scp_i ξ)
             syntax-local-bind-syntaxes
             (Cons id_arg ()) #f (Defs scp_defs 𝓁)) cont store Σ*)
       ((Cons id_defs ()) cont store (Tup Σ_3 scps_p scps_u))
       (where (Tup Σ scps_p scps_u) Σ*)

       (where id_defs (add ph (prune ph (flip ph id_arg scp_i) scps_u)
                           scp_defs))
       (where (values nam_new Σ_1) (alloc-name id_defs Σ))
       (where Σ_2 (bind ph Σ_1 id_defs nam_new))
       (where ξ_defs (def-ξ-lookup Σ_2 𝓁))
       (where Σ_3 (def-ξ-update Σ_2 𝓁
                     (extend-ξ ξ_defs nam_new (TVar id_defs))))
       ev-slbcv]

  ;; create macro definition binding
  [--> ((App (ph scp_i ξ)
             syntax-local-bind-syntaxes
             (Cons id_arg ()) stx_arg (Defs scp_defs 𝓁)) cont store Σ*)
       ((ph (parse (plus ph 1) stx_exp Σ_2) () no-scope ξ)
        (App (ph scp_i ξ)
             (syntax-local-bind-syntaxes2 scps_p scps_u)
             (Cons id_arg ()) (Defs scp_defs 𝓁) hole loc_new)
        store_1 (Tup Σ_2 scps_p (Set)))
       (where (Tup Σ scps_p scps_u) Σ*)
       (where stx_arg2 (add ph (flip ph stx_arg scp_i) scp_defs))
       (where (values stx_exp (Tup Σ_2 _ _))
              (expand (plus ph 1)
                      stx_arg2 (init-ξ) (Tup Σ (Set) (Set))))
       (where (values loc_new store_1) (push-cont store cont))
       ev-slbcm]

  [--> ((App (ph scp_i ξ)
             (syntax-local-bind-syntaxes2 scps_p scps_u)
             (Cons id_arg ()) (Defs scp_defs 𝓁) val_exp) cont store Σ*)
       ((Cons id_defs ()) cont store Σ*_4)
       ;(side-condition (printf "local-bind-syntaxes2:\n"))
       (where (Tup Σ _ _) Σ*)
       (where ξ_defs (def-ξ-lookup Σ 𝓁))
       (where id_defs (add ph
                           (prune ph (flip ph id_arg scp_i) scps_u)
                           scp_defs))
       (where (values nam_new Σ_2) (alloc-name id_defs Σ))
       (where Σ_3 (bind ph Σ_2 id_defs nam_new))
       (where Σ*_4 (Tup (def-ξ-update Σ_3 𝓁
                           (extend-ξ ξ_defs nam_new val_exp))
                         scps_p scps_u))
       ev-slbcm2]

  ;; local expand
  [--> ((App (ph scp_i ξ)
             local-expand stx val_contextv val_idstops) cont store Σ*)
       (in-expand ((ph (flip ph stx scp_i) ξ_stops) ∘ • (init-Θ) Σ*)
                  ((App (ph scp_i ξ) local-expand2) cont store Σ*))

       (where ξ_unstops
              ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p)))))
                    (term ξ)))
       (where (Tup Σ _ _) Σ*)
       (where (nam_stop ...) (resolve* ph val_idstops Σ))
       (where ξ_stops
              (extend-ξ* ξ_unstops
                           ((nam_stop (TStop (lookup-ξ ξ_unstops nam_stop)))
                            ...)))
       ev-lexpand]

  [--> (in-expand (stx_exp • • Θ_new Σ*)
                  ((App (ph scp_i ξ) local-expand2) cont store _))
       ((flip ph stx_exp scp_i) cont store Σ*)
       ev-lexpand2]

  ;; local expand with definition context
  ;; - similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [--> ((App (ph scp_i ξ)
             local-expand stx val_contextv val_idstops val_defs)
        cont store Σ*)
       ; TODO?: 下の(flip ph stx scp_i)は間違い？？しかしdefsを使わない場合にもこれはある．．．
       ;   これがあると，少なくともunit-4が通らない
       ;   しかし，flipしなければdefs-begin-with-defnの挙動が実際の処理系と異なってしまう．
       (in-expand ((ph (add ph #;stx (flip ph stx scp_i) scp_defs
                            ) ξ_stops)
                   ∘ • (init-Θ) Σ*)
                  ((App (ph scp_i ξ) local-expand2) cont store Σ*))
       ;(side-condition (printf "local-expand2:\n"))
       (where (Defs scp_defs 𝓁) val_defs)
       (where (Tup Σ _ _) Σ*)
       (where ξ_defs (def-ξ-lookup Σ 𝓁))
       (where ξ_unstops
              ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p)))))
                    (term ξ_defs)))
       (where (nam_stop ...) (resolve* ph val_idstops Σ))
       (where ξ_stops
              (extend-ξ* ξ_unstops
                           ((nam_stop (TStop (lookup-ξ ξ_unstops nam_stop)))
                            ...)))
       ev-lexpand-defs]

  ;; ----------------------------------------
  ;; Including boxes lets us implement recursive definitions as a
  ;; macro, including variable definitions that are in a recursive
  ;; binding scope with macros.

  ;; box
  [--> ((App (ph maybe-scp ξ)
             box val) cont store Σ*)
       (𝓁 cont store (Tup (box-update Σ_1 𝓁 val) scps_p scps_u))
       (where (Tup Σ scps_p scps_u) Σ*)
       (where (values 𝓁 Σ_1) (alloc-box Σ))
       ev-box]

  ;; unbox
  [--> ((App (ph maybe-scp ξ)
             unbox 𝓁) cont store Σ*)
       ((box-lookup Σ 𝓁) cont store Σ*)
       (where (Tup Σ _ _) Σ*)
       ev-unbox]

  ;; set-box!
  [--> ((App (ph maybe-scp ξ)
             set-box! 𝓁 val) cont store Σ*)
       (val cont store (Tup (box-update Σ 𝓁 val) scps_p scps_u))
       (where (Tup Σ scps_p scps_u) Σ*)
       ev-set-box!]

  ;; β
  [--> ((App (ph maybe-scp ξ)
             (Fun ((Var nam) ...) ast env) val ...) cont store Σ*)
       ((ph ast env_new maybe-scp ξ) cont store_2 Σ*)

       (where (values (loc ...) store_1) (alloc-loc* (nam ...) store))
       (where env_new (ext env ((Var nam) loc) ...))
       (where store_2 (update-store* store_1 (loc val) ...))
       ev-β]

  ;; primitive application
  [--> ((App (ph maybe-scp ξ)
             prim val ...) cont store Σ*)
       ((δ prim (val ...)) cont store Σ*)

       (side-condition (not (redex-match? Lfull stx-prim (term prim))))
       ev-δ]

  ;; if
  [--> ((If ser_test clo_then clo_else) cont store Σ*)
       (ser_test (If hole clo_then clo_else loc_new) store_1 Σ*)

       (where (values loc_new store_1) (push-cont store cont))
       ev-push-if]

  [--> (val (If hole clo_then clo_else loc_cont) store Σ*)
       ((If val clo_then clo_else) (lookup-store store loc_cont) store Σ*)
       ev-pop-if]

  [--> ((If #f clo_then clo_else) cont store Σ*)
       (clo_else cont store Σ*)
       ev-if-#f]

  [--> ((If val clo_then clo_else) cont store Σ*)
       (clo_then cont store Σ*)

       (side-condition (not (equal? (term val) #f)))
       ev-if-#t]

  ;; one-step eval (==>c)
  [==>c ζ ζ_new
        (where (ζ_new)
               ,(apply-reduction-relation ==>c (term ζ)))]

  with
  [(--> (in-expand c1 state) (in-expand c2 state))
   (==>c c1 c2)])

(define-metafunction Lfull
  eval : ph ast maybe-scp ξ Σ* -> (values val Σ*)
  [(eval ph ast maybe-scp ξ Σ*)
   (values val Σ*_2)
   (where ((val • store Σ*_2))
          ,(apply-reduction-relation*
            -->c
            (term ((ph ast (init-env) maybe-scp ξ) • (init-store) Σ*))))])

;; for debug

(module+ gui
  (require redex/gui)
  (define (trace--> form)
    (traces
     -->c
     (term ((0 ,(run form 'parse) (init-env) no-scope (init-ξ))
            • (init-sotre) (Tup (init-Σ) (Set) (Set)))))))

(define (eval--> form)
  (apply-reduction-relation*
   -->c
   (term ((0 ,(run form 'parse) (init-env) no-scope (init-ξ))
          • (init-store) (Tup (init-Σ) (Set) (Set))))))


;; ----------------------------------------
;; Expand-time store operations:

(define-extended-metafunction* core:alloc-κ Lfull
  alloc-κ : Θ  -> (values 𝓁 Θ))

(define-metafunction/extension core:lookup-κ Lfull
  lookup-κ : Θ 𝓁 -> κ)

(define-extended-metafunction* core:update-κ Lfull
  update-κ : Θ 𝓁 κ -> Θ)

(define-extended-metafunction* core:push-κ Lfull
  push-κ : Θ κ -> (values 𝓁 Θ))

;; ----------------------------------------
;; The expander:

(define-term id-kont (Stx (Sym #%kont) (Map)))
(define-term id-seq (Stx (Sym #%seq) (Map)))
(define-term id-snoc (Stx (Sym #%snoc) (Map)))
(define-term stx-nil (Stx () (Map)))

(define-reduction-relation ==>c Lfull
  #:domain ζ #:arrow ==> ;; ζ = (stx∘ ex? κ Θ Σ*)

  ;; stops
  (==> ((ph (Stx (Cons id_stop stl_args) ctx) ξ) ∘ κ Θ Σ*)
       ((Stx (Cons id_stop stl_args) ctx) • κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where (TStop _) (lookup-ξ ξ (resolve ph id_stop Σ)))
       ex-stop)

  ;; lambda (unchanged)
  (==> ((ph (Stx (Cons id_lam (Cons (Stx stl_args ctx_0)
                                    (Cons stx_body ()))) ctx) ξ)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph (add ph stx_body scp_new) ξ_new)
        ∘
        ((Stx (Cons id_lam (Cons (Stx stl_args2 ctx_0)
                                 (Cons hole ()))) ctx)
         • (Tup Σ scps_p scps_u) 𝓁_new) ;; Σ not used
        Θ_1 Σ*_2)

       (where lambda (resolve ph id_lam Σ))
       (where (values scp_new Σ_1) (alloc-scope Σ))
       (where (values stl_args2 ξ_new Σ_2)
              (regist-vars ph scp_new stl_args ξ Σ_1))
       (where Σ*_2 (Tup Σ_2 (union (Set scp_new) scps_p) (Set)))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-lam-body)

  ;; let
  (==> ((ph (Stx (Cons id_let
                       (Cons (Stx stl_binds ctx_1)
                             (Cons stx_body ()))) ctx) ξ)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph (add ph stx_body scp_new) ξ_new)
        ∘
        ((Stx (Cons id-kont
                    (Cons id_let
                          (Cons (Stx (Cons
                                      (Stx stl_vars2 ctx_1)
                                      (ph (Stx stl_rhs ctx_1) ξ))
                                     ctx_1)
                                (Cons hole ())))) ctx)
         ∘ (Tup Σ scps_p scps_u) 𝓁_new) ;; Σ not used
        Θ_1 Σ*_2)

       (where let (resolve ph id_let Σ))
       (where (values stl_vars stl_rhs) (unzip stl_binds))
       (where (values scp_new Σ_1) (alloc-scope Σ))
       (where (values stl_vars2 ξ_new Σ_2)
              (regist-vars ph scp_new stl_vars ξ Σ_1))
       (where Σ*_2 (Tup Σ_2 (union (Set scp_new) scps_p) (Set)))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-let-body)

  (==> ((Stx (Cons id_kont
                   (Cons id_let
                         (Cons (Stx (Cons
                                     (Stx stl_vars ctx_1)
                                     (ph (Stx stl_rhs ctx_1) ξ))
                                    ctx_1)
                               (Cons stx_body ())))) ctx)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq (Cons stx-nil stl_rhs)) ctx_1) ξ)
        ∘
        ((ph (Stx (Cons id_kont
                        (Cons id_let
                              (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                               hole) ctx_1)
                                    (Cons stx_body ())))) ctx) ξ)
         ∘ (Tup Σ scps_p scps_u) 𝓁_new)
        Θ_1 (Tup Σ scps_p (Set)))

       (where let (resolve ph id_let Σ))
       (where #%kont (resolve ph id_kont Σ))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-let-rhs)

  (==> ((ph (Stx (Cons id_kont
                       (Cons id_let
                             (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                              (Stx val_rhs ctx_1)) ctx_1)
                                   (Cons stx_body ())))) ctx) ξ)
        ∘ κ Θ Σ*)
       ((Stx (Cons id_let
                   (Cons (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                         (Cons stx_body ()))) ctx)
        • κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where let (resolve ph id_let Σ))
       (where #%kont (resolve ph id_kont Σ))
       ex-let-rhs2)

  ;; quote (unchanged)
  (==> ((ph (Stx (Cons id_quote (Cons stx ())) ctx) ξ) ∘ κ Θ Σ*)
       ((Stx (Cons id_quote (Cons stx ())) ctx) • κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where quote (resolve ph id_quote Σ))
       ex-quote)

  ;; syntax (unchanged)
  (==> ((ph (Stx (Cons id_syntax (Cons stx ())) ctx) ξ) ∘ κ Θ Σ*)
       ((Stx (Cons id_syntax (Cons stx_pruned ())) ctx) • κ Θ Σ*)

       (where (Tup Σ scps_p scps_u) Σ*)
       (where syntax (resolve ph id_syntax Σ))
       (where stx_pruned (prune ph stx scps_p))
       ex-stx)

  ;; macro creation (eval gets more and updates store)
  (==> ((ph (Stx (Cons id_ls
                       (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                             ctx_0) ()) ctx_1)
                             (Cons stx_body ()))) ctx) ξ) ∘ κ Θ Σ*)
       ((Stx (Cons id_ls
                   (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                         ctx_0) ()) ctx_1)
                         (Cons (ph stx_body ξ) ()))) ctx) ∘ κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where let-syntax (resolve ph id_ls Σ))
       ex-ξ-ls)

  (==> ((Stx (Cons id_ls
                   (Cons (Stx (Cons (Stx (Cons id (Cons stx_rhs ()))
                                         ctx_0) ()) ctx_1)
                         (Cons (ph stx_body ξ) ()))) ctx)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       (((plus ph 1) stx_rhs (init-ξ))
        ∘
        ((Stx (Cons
               id-kont
               (Cons id_ls
                     (Cons (Stx (Cons (Stx (Cons id_new (Cons hole ()))
                                           ctx_0) ()) ctx_1)
                           (Cons (ph stx_body ξ)
                                 (Stx #f (Map [ph (Set scp_new)])))))) ctx)
         ∘ (Tup Σ scps_p scps_u) 𝓁_new) ;; Σ not used
        Θ_1 (Tup Σ_3 (Set) (Set)))

       (where let-syntax (resolve ph id_ls Σ))
       (where (values nam_new Σ_1) (alloc-name id Σ))
       (where (values scp_new Σ_2) (alloc-scope Σ_1))
       (where id_new (add ph id scp_new))
       (where Σ_3 (bind ph Σ_2 id_new nam_new))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-ls-push-rhs)

  (==> ((Stx
         (Cons id_kont
               (Cons id_ls
                     (Cons (Stx (Cons (Stx (Cons id (Cons stx_exp ()))
                                           ctx_0) ()) ctx_1)
                           (Cons (ph stx_body ξ)
                                 (Stx #f (Map [ph (Set scp_new)])))))) ctx)
        ∘ κ Θ (Tup Σ scps_p _))
       (in-eval ((ph (parse (plus ph 1) stx_exp Σ) () no-scope ξ)
                 • (init-store) (Tup Σ scps_p (Set)))
                ((Stx (Cons (Stx (Sym nam_new) (Map))
                            (Cons (ph stx_body ξ)
                                  (Stx #f (Map [ph (Set scp_new)])))) (Map))
                 ∘ κ Θ (Tup Σ scps_p (Set)))) ;; Σ not used

       (where let-syntax (resolve ph id_ls Σ))
       (where #%kont (resolve ph id_kont Σ))
       (where nam_new (resolve ph id Σ))
       ex-ls-eval)

  (==> (in-eval (val • store_0 (Tup Σ _ _))
                ((Stx (Cons (Stx (Sym nam_new) (Map))
                            (Cons (ph stx_body ξ)
                                  (Stx #f (Map [ph (Set scp_new)])))) (Map))
                 ∘ κ Θ (Tup _ scps_p _)))
       ((ph stx_body2 ξ_new)
        ∘ κ Θ (Tup Σ (union (Set scp_new) scps_p) (Set)))

       (where ξ_new (extend-ξ ξ nam_new val))
       (where stx_body2 (add ph stx_body scp_new))
       ex-ls-ξ)

  ;; macro invocation
  (==> ((ph stx_macapp ξ) ∘ κ Θ (Tup Σ scps_p scps_u))
       (in-eval ((ph (App val stx_macapp2) () scp_i ξ) • (init-store) Σ*_2)
                ((ph (Stx #f (Map [ph (Set scp_i)])) ξ)
                 ∘ κ Θ Σ*_2)) ;; Σ*_2 not used

       (where (Stx (Cons id_mac stl_args) ctx) stx_macapp)
       (where val (lookup-ξ ξ (resolve ph id_mac Σ)))
       (where (values scp_u Σ_1) (alloc-scope Σ))
       (where (values scp_i Σ_2) (alloc-scope Σ_1))
       (where Σ*_2 (Tup Σ_2
                         (union (Set scp_u) scps_p)
                         (union (Set scp_u) scps_u)))
       (where stx_macapp2 (flip ph (add ph stx_macapp scp_u) scp_i))
       ex-macapp-eval)

  (==> (in-eval (stx_exp • store_0 Σ*)
                ((ph (Stx #f (Map [ph (Set scp_i)])) ξ) ∘ κ Θ _))
       ((ph (flip ph stx_exp scp_i) ξ) ∘ κ Θ Σ*)
       ex-macapp-flip)

  ;; if
  (==> ((ph (Stx (Cons id_if stl_exps) ctx) ξ)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq (Cons stx-nil stl_exps)) ctx) ξ)
        ∘
        ((ph (Stx (Cons id-kont (Cons id_if hole)) ctx) ξ)
         ∘ (Tup Σ scps_p scps_u) 𝓁_new) ;; Σ not used
        Θ_1 (Tup Σ scps_p (Set)))

       (where if (resolve ph id_if Σ))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-if)

  (==> ((ph (Stx (Cons id_kont
                       (Cons id_if (Stx val_exps ctx))) ctx) ξ)
        ∘ κ Θ Σ*)
       ((Stx (Cons id_if val_exps) ctx) • κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where #%kont (resolve ph id_kont Σ))
       (where if (resolve ph id_if Σ))
       ex-if-kont)

  ;; application (non-canonical #%app version, unchanged)
  (==> ((ph (Stx (Cons id_app (Cons stx_fun stl_args)) ctx) ξ)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq
                       (Cons stx-nil
                             (Cons stx_fun stl_args))) ctx) ξ)
        ∘
        ((Stx (Cons id_app hole) ctx) • (Tup Σ scps_p scps_u) 𝓁_new)
        Θ_1 (Tup Σ scps_p (Set)))

       (where #%app (resolve ph id_app Σ))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-#%app)

  ;; application (canonical #%app version, unchanged)
  (==> ((ph (Stx (Cons id_app
                       (Stx (Cons stx_fun stl_args) ctx_1)) ctx) ξ)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq
                       (Cons stx-nil
                             (Cons stx_fun stl_args))) ctx) ξ)
        ∘
        ((Stx (Cons id_app hole) ctx) • (Tup Σ scps_p scps_u) 𝓁_new)
        Θ_1 (Tup Σ scps_p (Set)))

       (where #%app (resolve ph id_app Σ))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-#%app2)

  ;; application (unchanged)
  (==> ((ph (Stx (Cons stx_fun stl_args) ctx) ξ)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph (Stx (Cons id-seq
                       (Cons stx-nil
                             (Cons stx_fun stl_args))) ctx) ξ)
        ∘
        ((Stx (Cons id_app hole) ctx) • (Tup Σ scps_p scps_u) 𝓁_new)
        Θ_1 (Tup Σ scps_p (Set)))

       (side-condition
        (or (not (redex-match? Lfull id (term stx_fun)))
            (let ([name (term (resolve ph stx_fun Σ))])
              (and (redex-match? Lfull not-found (term (lookup-ξ ξ ,name)))
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))))
       (where id_app (Stx (Sym #%app) ctx))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-app)

  ;; reference (unchanged)
  (==> ((ph id ξ) ∘ κ Θ Σ*)
       (id_new • κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where (TVar id_new) (lookup-ξ ξ (resolve ph id Σ)))
       ex-var)

  ;; literal (unchanged)
  (==> ((ph (Stx atom ctx) ξ) ∘ κ Θ Σ*)
       ((Stx (Cons (Stx (Sym quote) ctx) (Cons (Stx atom ctx) ())) ctx)
        • κ Θ Σ*)

       (side-condition (not (redex-match? Lfull id (term (Stx atom ctx)))))
       ex-lit)

  ;; pop κ (merge Σ*)
  (==> (stx • (STX ex? (Tup _ scps_p scps_u) 𝓁) Θ (Tup Σ _ _))
       ((in-hole STX stx) ex? κ Θ (Tup Σ scps_p scps_u))

       (where κ (lookup-κ Θ 𝓁))
       ex-pop-κ)

  ;; expression sequence
  ;;  (expand (seq (exped ...))) --> (exped ...)
  (==> ((ph (Stx (Cons id_seq
                       (Cons (Stx val_expeds (Map)) ())) ctx) ξ)
        ∘ κ Θ Σ*)
       ((Stx val_expeds ctx) • κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where #%seq (resolve ph id_seq Σ))
       ex-seq-nil)

  ;; (expand (seq (done ...) exp0 exp ...)) -->
  ;;   (expand (seq (done ... (expand exp0)) exp ...))
  (==> ((ph (Stx (Cons id_seq
                       (Cons (Stx val_dones (Map))
                             (Cons stx_exp0 stl_exps))) ctx) ξ)
        ∘ κ Θ (Tup Σ scps_p scps_u))
       ((ph stx_exp0 ξ)
        ∘
        ((ph (Stx (Cons id-kont
                        (Cons id_seq
                              (Cons
                               (Stx (Cons id-snoc
                                          (Cons (Stx val_dones (Map)) hole))
                                    (Map))
                               stl_exps))) ctx) ξ)
         ∘ (Tup Σ scps_p scps_u) 𝓁_new)
        Θ_1 (Tup Σ scps_p (Set)))

       (where #%seq (resolve ph id_seq Σ))
       (where (values 𝓁_new Θ_1) (push-κ Θ κ))
       ex-seq-cons)

  (==> ((ph (Stx (Cons id_kont
                       (Cons id_seq
                             (Cons (Stx (Cons id_snoc
                                              (Cons (Stx val_exps ctx_1)
                                                    (Stx val_exp ctx_2)))
                                        (Map))
                                   stl_exps))) ctx) ξ)
        ∘ κ Θ Σ*)
       ((ph (Stx (Cons id_seq
                       (Cons (Stx val_exps2 ctx_1)
                             stl_exps)) ctx) ξ)
        ∘ κ Θ Σ*)

       (where (Tup Σ _ _) Σ*)
       (where #%seq (resolve ph id_seq Σ))
       (where #%kont (resolve ph id_kont Σ))
       (where #%snoc (resolve ph id_snoc Σ))
       (where val_exps2 (snoc val_exps (Stx val_exp ctx_2)))
       ex-seq-snoc)


  ;; one-step eval (-->c)
  (-->c state state_new
        (where (state_new) ,(apply-reduction-relation -->c (term state))))

  with
  ((==> (in-eval s1 ζ) (in-eval s2 ζ))
   (-->c s1 s2)))

(define-metafunction Lfull
  expand : ph stx ξ Σ* -> (values stx Σ*)
  [(expand ph stx ξ Σ*)
   (values stx_new Σ*_new)
   (where ((stx_new • • Θ_new Σ*_new))
          ,(apply-reduction-relation*
            ==>c
            (term ((ph stx ξ) ∘ • (init-Θ) Σ*))))])

;; for debug

(module+ gui
  (define (step==> form)
    (stepper
     ==>c (term ((0 ,(run form 'read) (init-ξ))
                 ∘ • (init-Θ) (Tup (init-Σ) (Set) (Set))))))

  (define (trace==> form)
    (traces
     ==>c (term ((0 ,(run form 'read) (init-ξ))
                 ∘ • (init-Θ) (Tup (init-Σ) (Set) (Set)))))))

(define (expand==> form)
  (apply-reduction-relation*
   ==>c (term ((0 ,(run form 'read) (init-ξ))
               ∘ • (init-Θ 0) (Tup (init-Σ) (Set) (Set))))))

(define (expand&parse form)
  (let ([r (expand==> form)])
    (and (= (length r) 1)
         (term (parse/values (values ,(caar r) ,(fifth (car r))))))))


;; ----------------------------------------
;; Drivers

(define-helpers Lfull (Map)
  reader printer)

(define-metafunction Lfull
  stripper : (values stx Σ*) -> val
  [(stripper (values stx Σ*)) (strip stx)])

(define-metafunction Lfull
  expander : stx -> (values stx Σ*)
  [(expander stx) (expand 0 stx (init-ξ) (Tup (init-Σ) (Set) (Set)))])

(define-metafunction Lfull
  parse/values : (values stx Σ*) -> ast
  [(parse/values (values stx (Tup Σ _ _))) (parse 0 stx Σ)])

(define-metafunction Lfull
  evaluate : ast -> val
  [(evaluate ast)
   val
   (where (values val Σ*)
          (eval 0 ast no-scope (init-ξ) (Tup (init-Σ) (Set) (Set))))])

(define-runner run
  reader
  expander
  stripper printer
  evaluate
  parse/values)

;; ----------------------------------------
;; Examples:

(define ex-local-value
  '[local-value
    (let-syntax ([a '8])
      (let-syntax ([b '9])
        (let-syntax ([x (lambda (stx)
                          (datum->syntax
                           #'here
                           (list #'quote
                                 (datum->syntax
                                  #'here
                                  (syntax-local-value (second (syntax-e stx)))))))])
          (x a))))])


(define ex-local-expand
  '[local-expand
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx)
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (second (syntax-e (cdr (syntax-e (local-expand
                                                          (second (syntax-e stx))
                                                          'expression
                                                          '()))))))])
        (x (q))))])
(define (raw-local-expand)
  (let-syntax ([q (lambda (stx) #'(car 8))])
    (let-syntax ([x (lambda (stx)
                      ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                      (let ([stx2 (syntax-e (local-expand
                                             (second (syntax-e stx))
                                             'expression
                                             '()))])
                        (second (syntax-e (cdr stx2)))))])
      (x (q)))))


(define ex-local-expand-stop
  '[local-expand-stop
    (let-syntax ([p (lambda (stx) '0)])
      (let-syntax ([q (lambda (stx) #'(car 8))])
        (let-syntax ([x (lambda (stx)
                          ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                          (second (syntax-e (cdr (syntax-e (local-expand
                                                            (second (syntax-e stx))
                                                            'expression
                                                            (list #'p)))))))])
          (x (q)))))])
(define (raw-local-expand-stop)
  (let-syntax ([p (lambda (stx) '0)])
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx)
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (let ([stx2 (syntax-e (local-expand
                                               (second (syntax-e stx))
                                               'expression
                                               (list #'p)))])
                          (second (syntax-e (cdr stx2)))))])
        (x (q))))))


(define ex-nested-local-expand
  '[nested-local-expand
    (let-syntax ([z (lambda (stx) #''0)])
      (let-syntax ([a (lambda (stx)
                        ;; When `b' forces `a', then `a'
                        ;; drops `z' form the stop list, so it
                        ;; should expand to 0
                        (local-expand (second (syntax-e stx))
                                      'expression
                                      '()))])
        (let-syntax ([b (lambda (stx)
                          (datum->syntax
                           stx
                           (list
                            #'quote
                            (local-expand (second (syntax-e stx))
                                          'expression
                                          (list #'z)))))])
          (list (b (z)) (b (a (z)))))))])
(define (raw-nested-local-expand)
  (let-syntax ([z (lambda (stx) #''0)])
    (let-syntax ([a (lambda (stx)
                      ;; When `b' forces `a', then `a'
                      ;; drops `z' form the stop list, so it
                      ;; should expand to 0
                      (local-expand (second (syntax-e stx))
                                    'expression
                                    '()))])
      (let-syntax ([b (lambda (stx)
                        (datum->syntax
                         stx
                         (list
                          #'quote
                          (local-expand (second (syntax-e stx))
                                        'expression
                                        (list #'z)))))])
        (list (b (z)) (b (a (z))))))))


(define ex-local-binder
  '[local-binder
    (let-syntax ([q (lambda (stx)
                      ;; quotes its argument
                      (datum->syntax
                       stx
                       (list #'quote (second (syntax-e stx)))))])
      (let-syntax ([a (lambda (stx)
                        ;; expands first argument, expected quoted name
                        ;; to use as binder with second arguments body
                        (datum->syntax
                         stx
                         (list
                          #'lambda
                          (datum->syntax
                           stx
                           (list (syntax-local-identifier-as-binding
                                  (second (syntax-e
                                           (local-expand (second (syntax-e stx))
                                                         'expression
                                                         '()))))))
                          (third (syntax-e stx)))))])
        ;; removing the syntax-local-identifier-as-binding call above
        ;; leaves the second `x` as unbound:
        ;; TODO: 実装と不一致．取り除いても↓の実装では unbound にならない
        ((a (q x) x) 'FOOOO)))])
(define (raw-local-binder)
  (let-syntax ([q (lambda (stx)
                    ;; quotes its argument
                    (datum->syntax
                     stx
                     (list #'quote (second (syntax-e stx)))))])
    (let-syntax ([a (lambda (stx)
                      ;; expands first argument, expected quoted name
                      ;; to use as binder with second arguments body
                      (datum->syntax
                       stx
                       (list
                        #'lambda
                        (datum->syntax
                         stx
                         (list (syntax-local-identifier-as-binding
                                (second (syntax-e
                                         (local-expand (second (syntax-e stx))
                                                       'expression
                                                       '()))))))
                        (third (syntax-e stx)))))])
      ;; removing the syntax-local-identifier-as-binding call above
      ;; leaves the second `x` as unbound:
      ;; TODO: 実装と不一致．取り除いても unbound にならない
      (a (q x) x))))

(define local:examples
  (list ex-local-value
        ex-local-expand
        ex-local-expand-stop
        ex-nested-local-expand
        ex-local-binder))


(define ex-box
  '[box
    (let-syntax ([m (lambda (stx)
                      (datum->syntax
                       stx
                       (list
                        #'quote
                        (datum->syntax
                         stx
                         (let ([b (box 0)])
                           (unbox b))))))])
      (m))])


(define ex-set-box
  '[set-box
    (let-syntax ([m (lambda (stx)
                      (datum->syntax
                       stx
                       (list
                        #'quote
                        (datum->syntax
                         stx
                         (let ([b (box 0)])
                           (let ([x (set-box! b 1)])
                             (unbox b)))))))])
      (m))])


(define ex-defs-shadow
  '[defs-shadow
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([p (lambda (stx) #'0)])
         (let-syntax ([q (lambda (stx)
                           (let ([defs (syntax-local-make-definition-context)])
                             (let ([ignored (syntax-local-bind-syntaxes
                                             (list (second (syntax-e stx))) #f defs)])
                               (datum->syntax
                                #'here
                                (list
                                 #'lambda
                                 ; not necessary in this case, but sensible
                                 (datum->syntax
                                  #'here
                                  (list (syntax-local-identifier-as-binding 
                                         (second
                                          (syntax-e
                                           (local-expand (datum->syntax
                                                          #'here
                                                          (list #'quote
                                                                (second (syntax-e stx))))
                                                         'expression
                                                         '()
                                                         defs))))))
                                 (local-expand (third (syntax-e stx))
                                               'expression
                                               (list #'call)
                                               defs))))))])
           ((q p (call p)) (lambda () 'FOOOO)))))])
(define (raw-defs-shadow)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
    (let-syntax ([p (lambda (stx) #'0)])
      (let-syntax ([q (lambda (stx)
                        (let ([defs (syntax-local-make-definition-context)])
                          (let ([ignored
                                 (syntax-local-bind-syntaxes
                                  (list (second (syntax-e stx))) #f defs)])
                            (datum->syntax
                             #'here
                             (list
                              #'lambda
                              ; not necessary in this case, but sensible
                              (datum->syntax
                               #'here
                               (list (syntax-local-identifier-as-binding 
                                      (second
                                       (syntax-e
                                        (local-expand (datum->syntax
                                                       #'here
                                                       (list #'quote
                                                             (second (syntax-e stx))))
                                                      'expression
                                                      '()
                                                      defs))))))
                              (local-expand (third (syntax-e stx))
                                            'expression
                                            (list #'call)
                                            defs))))))])
        ((q p (call p)) (lambda () 'FOOOO))))))


;; Like the previous example, but using a macro that expands to `quote`:
(define ex-defs-shadow2
  '[defs-shadow2
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([qt (lambda (stx)
                          (datum->syntax
                           #'here
                           (list #'quote (second (syntax-e stx)))))])
         (let-syntax ([p (lambda (stx) #'0)])
           (let-syntax ([q (lambda (stx)
                             (let ([defs (syntax-local-make-definition-context)])
                               (let ([ignored 
                                      (syntax-local-bind-syntaxes
                                       (list (second (syntax-e stx))) #f defs)])
                                 (datum->syntax
                                  #'here
                                  (list
                                   #'lambda
                                   (datum->syntax
                                    #'here
                                    ; necessary in this case
                                    (list (syntax-local-identifier-as-binding
                                           (second
                                            (syntax-e
                                             (local-expand (datum->syntax
                                                            #'here
                                                            (list #'qt
                                                                  (second (syntax-e stx))))
                                                           'expression
                                                           '()
                                                           defs))))))
                                   (local-expand (third (syntax-e stx))
                                                 'expression
                                                 (list #'call)
                                                 defs))))))])
             ((q p (call p)) (lambda () 'BOOOOOON))))))])
(define (raw-defs-shadow2)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
    (let-syntax ([qt (lambda (stx)
                       (datum->syntax
                        #'here
                        (list #'quote (second (syntax-e stx)))))])
      (let-syntax ([p (lambda (stx) #'0)])
        (let-syntax ([q (lambda (stx)
                          (let ([defs (syntax-local-make-definition-context)])
                            (let ([ignored (syntax-local-bind-syntaxes
                                            (list (second (syntax-e stx)))
                                            #f
                                            defs)])
                              (datum->syntax
                               #'here
                               (list
                                #'lambda
                                (datum->syntax
                                 #'here
                                 ; necessary in this case
                                 (list (syntax-local-identifier-as-binding
                                        (second
                                         (syntax-e
                                          (local-expand (datum->syntax
                                                         #'here
                                                         (list #'qt
                                                               (second (syntax-e stx))))
                                                        'expression
                                                        '()
                                                        defs))))))
                                (local-expand (third (syntax-e stx))
                                              'expression
                                              (list #'call)
                                              defs))))))])
          ((q p (call p)) (lambda () 'BOOOOOON)))))))


(define ex-defs-local-macro
  '[defs-local-macro
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([p (lambda (stx) #'44)])
         (let-syntax ([q (lambda (stx)
                           (let ([defs (syntax-local-make-definition-context) ])
                             (let ([ignored 
                                    (syntax-local-bind-syntaxes
                                     (list (second (syntax-e stx)))
                                     (datum->syntax
                                      #'here
                                      (list #'lambda
                                            (datum->syntax #'here (list #'stx))
                                            (fourth (syntax-e stx))))
                                     defs)])
                               (datum->syntax                             
                                #'here
                                (list
                                 #'lambda
                                 (datum->syntax
                                  #'here
                                  (list
                                   (second
                                    (syntax-e
                                     (local-expand (datum->syntax
                                                    #'here
                                                    (list
                                                     #'quote
                                                     (second (syntax-e stx))))
                                                   'expression
                                                   '()
                                                   defs)))))
                                 (local-expand (third (syntax-e stx))
                                               'expression
                                               '()
                                               defs))))))])
           ((q p (call p) #'13) '0))))])
(define (raw-defs-local-macro)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
    (let-syntax ([p (lambda (stx) #'44)])
      (let-syntax ([q (lambda (stx)
                        (let ([defs (syntax-local-make-definition-context)])
                          (let ([ignored 
                                 (syntax-local-bind-syntaxes
                                  (list (second (syntax-e stx)))
                                  (datum->syntax
                                   #'here
                                   (list #'lambda
                                         (datum->syntax #'here (list #'stx))
                                         (fourth (syntax-e stx))))
                                  defs)])
                            (datum->syntax                             
                             #'here
                             (list
                              #'lambda
                              (datum->syntax
                               #'here
                               (list
                                (second
                                 (syntax-e
                                  (datum->syntax
                                   #'here
                                   (list
                                    #'quote
                                    (second (syntax-e stx))))
                                  #;
                                  (local-expand (datum->syntax
                                                 #'here
                                                 (list
                                                  #'quote
                                                  (second (syntax-e stx))))
                                                'expression
                                                '()
                                                defs)))))
                              (local-expand (third (syntax-e stx))
                                            'expression
                                            '()
                                            defs))))))])
        ((q p (call p) #'13) 0)))))


(define (enriched-defs-local-macro)
  (let-syntax ([call (syntax-rules ()
                       [(call p) (p)])])
    (let-syntax ([p (syntax-rules () [(p) 44])])
      (let-syntax ([q (lambda (stx)
                        (syntax-parse stx
                          [(q p c e)
                           (let ([defs (syntax-local-make-definition-context)])
                             (let ([ignored
                                    (syntax-local-bind-syntaxes
                                     (list #'p)
                                     #'(lambda (stx) e)
                                     defs)])
                               (let* ([body (local-expand #'c
                                                          'expression
                                                          '()
                                                          defs)]
                                      [e2 #'(quote p)
                                       #;
                                       (local-expand #'(quote p)
                                                     'expression
                                                     '()
                                                     defs)])
                                 (syntax-case e2 ()
                                   [(_ p2)
                                    #`(lambda (p2) #,body)]))))]))])
        ((q p (call p) #'13) 0)))))


(define ex-defs-begin-with-defn
  '[defs-begin-with-defn
     (let-syntax ([bwd (lambda (stx)
                         (let ([;; create ctx
                                ctx (syntax-local-make-definition-context)]
                               [; the x in (define x '10)
                                id1 (second (syntax-e (second (syntax-e stx))))]
                               [; the 10 in (define x '10)
                                e1 (third (syntax-e (second (syntax-e stx))))]
                               [; the q in (define-syntax q (lambda (v) ...))
                                id2 (second (syntax-e (third (syntax-e stx))))]
                               [; the (lambda (v) ...) in (define-syntax q (lambda (v) ...))
                                e2 (third (syntax-e (third (syntax-e stx))))]
                               [; the last body expression, expands to (lambda (i) x)
                                e3 (fourth (syntax-e stx))])
                           (let ([; for side-effect of binding x in ctx
                                  ;; bind id1 (i.e., x)
                                  ignored (syntax-local-bind-syntaxes
                                           (list id1) #f ctx) ]
                                 [; for side-effect of binding q in ctx
                                  ;; bind id2 (i.e., q)
                                  ignored2 (syntax-local-bind-syntaxes
                                            (list id2) e2 ctx)]
                                 [; local-expand e3
                                  ;; local-expand e3 (i.e., the body expression):
                                  ee3 (local-expand e3
                                                    'expression
                                                    (list #'lambda)
                                                    ctx)]
                                 [; local-expand id1 (in a syntax form)
                                  ;; local-expand of id1 (to give it context from ctx):
                                  qid1 (local-expand (datum->syntax
                                                      #'here
                                                      (list #'quote
                                                            id1))
                                                     'expression
                                                     (list #'quote)
                                                     ctx)])
                             (let ([; extract expanded id1 from qid1
                                    eid1 (second (syntax-e qid1))])
                               ;; generate ((lambda (eid1) ee3) '10):
                               (datum->syntax
                                #'here
                                (list
                                 (datum->syntax
                                  #'here
                                  (list #'lambda
                                        (datum->syntax
                                         #'here
                                         (list eid1))
                                        ee3))
                                 e1))))))])
       ;; `bwd' is short for `begin-with-definitions', which
       ;; assumes a `define' followed by a `define-syntax' followed
       ;; by a body form
       ((bwd (define x '10)
             (define-syntax q (lambda (v) (syntax (lambda (i) x))))
             #;(lambda i x)
             (q)) 0))])

(define (raw-defs-begin-with-defn)
  (let-syntax
      ([bwd (lambda (stx)
              (syntax-case stx (define define-syntax)
                [(bwd (define id1 e1)
                      (define-syntax id2 e2)
                      e3)
                 (let* ([;; create ctx
                         ctx (syntax-local-make-definition-context)]
                        [; for side-effect of binding x in ctx
                         ;; bind id1 (i.e., x)
                         ignored (syntax-local-bind-syntaxes (list #'id1) #f ctx)]
                        [; for side-effect of binding q in ctx
                         ;; bind id2 (i.e., q)
                         ignored (syntax-local-bind-syntaxes (list #'id2) #'e2 ctx)]
                        [;; local-expand e3 (i.e., the body expression)
                         ee3 (local-expand #'e3
                                           'expression
                                           (list #'lambda)
                                           ctx)]
                        [; local-expand id1 (in a syntax form)
                         ;; local-expand of id1 (to give it context from ctx):
                         qid1 (local-expand (datum->syntax
                                             #'here
                                             (list #'quote
                                                   #'id1))
                                            'expression
                                            (list #'quote)
                                            ctx)]
                        [; extract expanded id1 from qid1
                         eid1 (second (syntax-e qid1))])
                   ;; generate ((lambda (eid1) ee3) '10):
                   (datum->syntax
                    #'here
                    (list
                     (datum->syntax
                      #'here
                      (list #'lambda
                            (datum->syntax
                             #'here
                             (list eid1))
                            ee3))
                     #'e1)))]))])
    ;; `bwd' is short for `begin-with-definitions', which
    ;; assumes a `define' followed by a `define-syntax' followed
    ;; by a body form
    ((bwd (define x '10)
          (define-syntax q (lambda (v) (syntax (lambda (i) x))))
          #;(lambda i x)
          (q)) 0)))   ;; TODO: ((bwd ...) 10) ではなく ((q) 0) とするとなぜか
                      ;; q が out-of-contextとエラーになる．スコープ関係？

(define defs:examples
  (list ex-box
        ex-set-box
        ;ex-defs-shadow
        ;ex-defs-shadow2
        ex-defs-local-macro
        ;ex-defs-begin-with-defn
        ))

(define main
  (let ([all-runs `([core ,core:run]
                    [phases ,phases:run]
                    [full ,run])]
        [all-examples (list core:examples
                            phases:examples
                            (append local:examples defs:examples))])
    (run-all-examples all-runs all-examples)))


;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           ;pict
           "config.rkt")
  (provide (all-defined-out))

  #;
  (define eval-pict
    (if narrow-mode?
        ;; Independent form:
        (vl-append
         (metafunction-rule-gap-space)
         (parameterize ([metafunction-cases '(0)])
           (WR (metafunction->pict eval #:contract? #t)))
         (parameterize ([metafunction-cases '(1)])
           (WR (metafunction->pict eval)))
         (parameterize ([metafunction-cases '(2)])
           (WR (metafunction->pict eval))))
        ;; Table form:
        (parameterize ([metafunction-cases '(0 1 2)])
          (WR (metafunction->pict eval #:contract? #t)))))

  #;
  (define (make-expand-pict pos [contract? #f])
    (parameterize ([metafunction-cases (list pos)])
      (WR (metafunction->pict expand #:contract? contract?))))

  #;(define expand-stop-pict (make-expand-pict 0 #t))
  #;(define expand-macro-app-pict (make-expand-pict 5))
  #;
  (define expand-lambda-pict
    (parameterize ([linebreaks (list #t)])
      (make-expand-pict 1)))

  (define unstop-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict unstop #:contract? #t))))

  (define newer-nts '(maybe-scp Σ*))
  (define language-delta-pict
    (WR (language->pict Lfull #:nts newer-nts)))

  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Lfull lw))))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view language-delta-pict
        eval-pict
        expand-stop-pict
        expand-lambda-pict
        unstop-pict))

(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-machine.rkt" pict) all-nts)
           (only-in (submod "phases-machine.rkt" pict)
                    changed-nts
                    new-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Local-Expansion"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lfull #:nts (append newer-nts
                                              new-nts
                                              all-nts))))
     (WR (metafunction->pict eval #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict unstop #:contract? #t)))
     #;
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     #;
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict core:strip #:contract? #t)))
     #;
     (parameterize ([linebreaks '(#f #f #t #f #t #f #f #t #f)])
       (WR (metafunction->pict expand #:contract? #t)))
     #;
     (parameterize ([linebreaks '(#f #f #t)])
       (WR (metafunction->pict expand* #:contract? #t)))
     (WR (metafunction->pict prune #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))


;;;; unit tests

(define (unit-0)
 (run '(let-syntax
           ([f (lambda (stx)
                 (let ([defs (syntax-local-make-definition-context)])
                   (let ([id (syntax-local-bind-syntaxes
                              (list #'id) #f defs)])
                     #'(+ 1 2))))])
         (f)) 'eval))


;; これは本物の処理系でもダメ
(define (unit-1)
  (run '(let-syntax
            ([f (lambda (stx)
                  (syntax-case stx ()
                    [(f x)
                     (let ([defs (syntax-local-make-definition-context)])
                       (let ([ignored (syntax-local-bind-syntaxes
                                       (list #'x) #'#''id defs)])
                         (syntax-local-value #'x #f defs)))]))])
          (f a)) 'eval))

(define (unit-2)
  (run '(let-syntax
            ([m (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([ids (syntax-local-bind-syntaxes
                                (list #'f) #'#''id defs)])
                      (syntax-local-value (car ids) #f defs))))])
          (m)) 'eval))

(define (unit-3)
  (run '(let-syntax
            ([m (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([ignored (syntax-local-bind-syntaxes
                                    (list (second (syntax-e stx)))
                                    #'(lambda (stx) #'(+ 1 2))
                                    defs)])
                      (local-expand (datum->syntax
                                     #'here
                                     (list (second (syntax-e stx))))
                                    'expression '() defs))))])
          (m f)) 'eval))

(define (unit-4)
  (run '(let-syntax
            ([m (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([ids (syntax-local-bind-syntaxes
                                (list #'f)
                                #'(lambda (stx) #'(+ 1 2))
                                defs)])
                      (local-expand (datum->syntax
                                     #'here
                                     (list (car ids)))
                                    'expression '() defs))))])
          (m)) 'eval))
