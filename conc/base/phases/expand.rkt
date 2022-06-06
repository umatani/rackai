#lang racket
(require "../set.rkt" "../reduction.rkt"
         (only-in "../core/syntax.rkt" zip unzip snoc union)
         (only-in "../core/eval.rkt" init-env init-store -->c)
         (only-in "../core/expand.rkt"
                  alloc-name alloc-scope init-ξ lookup-ξ extend-ξ
                  init-Θ lookup-κ push-κ)
         "struct.rkt"
         (only-in "syntax.rkt"
                  empty-ctx in-hole add flip prune
                  bind at-phase resolve id=?)
         (only-in "parse.rkt" parse))
(provide (all-defined-out))

;; This is the same as the single-phase one, but with `ph`
;; threaded through to `add` & `bind`
;(: regist-vars : Ph Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define ((regist-vars/bind/alloc-name bind alloc-name) ph scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) stx) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1)
                    ((regist-vars/bind/alloc-name bind alloc-name)
                     ph scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (add ph id scp)]
                   [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))
(define regist-vars (regist-vars/bind/alloc-name bind alloc-name))

(define id-kont (GenStx (Sym '#%kont) (empty-ctx)))
(define id-seq (GenStx (Sym '#%seq)  (empty-ctx)))
(define id-snoc (GenStx (Sym '#%snoc) (empty-ctx)))
(define stx-nil (GenStx '() (empty-ctx)))

;; (: ==>p :  ζ -> (Setof ζ))
(define-parameterized-reduction-relation (==>p/Σ :=<1> -->c)

  ;; lambda
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_lam)
                         ,(GenStx (? ProperStl? stl_args) ctx_0)
                         ,stx_body) ctx)
            ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_lam 'lambda Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars ph scp_new stl_args ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
      '∘
      (κ (GenStx `(,id_lam
                   ,(GenStx stl_args2 ctx_0)
                   ,(Hole)) ctx)
         '• 𝓁_new) Θ_1 Σ_2)
   ex-lam-body]

  ;; let
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_let)
                         ,(GenStx (? ProperStl? stl_binds) ctx_1)
                         ,stx_body) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_let 'let Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with         (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars ph scp_new stl_vars ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
      '∘
      (κ (GenStx `(,id-kont
                   ,id_let
                   ,(Stxξ ph (GenStx `(,(GenStx stl_vars2 ctx_1)
                                       ,(GenStx stl_rhs ctx_1)
                                       ) ctx_1) ξ scps_p)
                   ,(Hole)) ctx) '∘ 𝓁_new) Θ_1 Σ_2)
   ex-let-body]

  [(ζ (GenStx `(,(? Id? id_kont)
                ,(? Id? id_let)
                ,(Stxξ ph (GenStx
                           `(,(GenStx (? ProperStl? stl_vars) _)
                             ,(GenStx (? ProperStl? stl_rhs) _)) ctx_1)
                       ξ scps_p)
                ,stx_body) ctx) '∘ κ0 Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_let  'let    Σ))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ scps_p)
      '∘
      (κ
       (Stxξ ph (GenStx `(,id_kont
                          ,id_let
                          ,(GenStx `(,(GenStx stl_vars ctx_1) ,(Hole)) ctx_1)
                          ,stx_body) ctx)
             ξ scps_p) '∘ 𝓁_new) Θ_1 Σ)
   ex-let-rhs]

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                         ,(? Id? id_let)
                         ,(GenStx `(,(GenStx (? ProperStl? stl_vars) _)
                                    ,(GenStx (? ProperStl? val_rhs) _)) ctx_1)
                         ,stx_body) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_let  'let    Σ))
   (ζ (GenStx `(,id_let ,(GenStx (zip stl_vars val_rhs ctx_1) ctx_1)
                        ,stx_body) ctx) '• κ Θ Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ ph (and stx (GenStx `(,(? Id? id_quote) ,_) _)) _ _) '∘ κ Θ Σ)
   #:when (id=? ph id_quote 'quote Σ)
   (ζ stx '• κ Θ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_syntax) ,stx) ctx) _ scps_p) '∘ κ Θ Σ)
   #:when (id=? ph id_syntax 'syntax Σ)
   #:with stx_pruned := (prune ph stx scps_p)
   (ζ (GenStx `(,id_syntax ,stx_pruned) ctx) '• κ Θ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_ls)
                         ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                         ,stx_body) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (id=? ph id_ls 'let-syntax Σ)
   (ζ (GenStx `(,id_ls
                ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                ,(Stxξ ph stx_body ξ scps_p)) ctx) '∘ κ Θ Σ)
   ex-ξ-ls]

  [(ζ (GenStx `(,(? Id? id_ls)
                ,(GenStx `(,(GenStx `(,(? Id? id) ,stx_rhs) ctx_0)) ctx_1)
                ,(Stxξ ph stx_body ξ scps_p)) ctx) '∘ κ0 Θ Σ)
   #:when (id=? ph id_ls 'let-syntax Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ) 
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add ph id scp_new)
   #:with                  Σ_3 := (bind ph Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   #:with            stx_body2 := (add ph stx_body scp_new)
   #:with              scps_p2 := (union (set scp_new) scps_p)
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ) (set))
      '∘
      (κ (GenStx `(,id-kont
                   ,id_ls
                   ,(GenStx `(,(GenStx `(,id_new ,(Hole)) ctx_0)) ctx_1)
                   ,(Stxξ ph stx_body2 ξ scps_p2)) ctx) '∘ 𝓁_new)
      Θ_1 Σ_3)
   ex-ls-push-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body2 ξ scps_p2)) ctx) '∘ κ Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_ls 'let-syntax Σ))
   #:with nam_new :=<1> (resolve ph id_new Σ)
   #:with ast_exp :=<1> (parse (add1 ph) stx_exp Σ)
   (InEval `(,(AstEnv ast_exp (init-env)) • ,(init-store))
           (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                         ,(Stxξ ph stx_body2 ξ scps_p2)) (empty-ctx))
               '∘ κ Θ Σ))
   ex-ls-eval]

  [(InEval `(,(? Val? val) • ,_)
           (ζ (GenStx `(,(GenStx (Sym nam_new) _)
                        ,(Stxξ ph stx_body2 ξ scps_p2)) _) '∘ κ Θ Σ))
   #:with ξ_new := (extend-ξ ξ nam_new val)
   (ζ (Stxξ ph stx_body2 ξ_new scps_p2) '∘ κ Θ Σ)
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ scps_p)
       '∘ κ Θ Σ)
   #:with            nam_mac :=<1> (resolve ph id_mac Σ)
   #:with                val :=    (lookup-ξ ξ nam_mac)
   #:when (Val? val)
   #:with (values scp_u Σ_1) :=    (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) :=    (alloc-scope 'i Σ_1)
   (InEval
    `(,(AstEnv (App val (list (flip ph (add ph stx_macapp scp_u) scp_i)))
               (init-env))
      • ,(init-store))
    (ζ (Stxξ ph (GenStx #f (list (cons ph (set scp_i))))
               ξ (union (set scp_u) scps_p)) '∘ κ Θ Σ_2))
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0)
           (ζ (Stxξ ph (GenStx #f ctx_i) ξ scps_p) '∘ κ Θ Σ))
   #:with scp_i := (car (set->list (at-phase ctx_i ph)))
   (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ scps_p) '∘ κ Θ Σ)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_if) ,stl_exps ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_if 'if Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ scps_p)
      '∘
      (κ (Stxξ ph (GenStx `(,id-kont ,id_if ,(Hole)) ctx) ξ scps_p)
         '∘ 𝓁_new) Θ_1 Σ)
   ex-if]

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                         ,(? Id? id_if)
                         ,(GenStx (? ProperStl? val_exps) ctx)) _)
            ξ scps_p) '∘ κ Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_if   'if     Σ))
   (ζ (GenStx `(,id_if ,@val_exps) ctx) '• κ Θ Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_app)
                         ,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
      '∘
      (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new) Θ_1 Σ)
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (GenStx
                (cons (? Id? id_app)
                      (GenStx `(,stx_fun ,stl_args ...) _)) ctx) ξ scps_p)
      '∘ κ0 Θ Σ)
   #:when (id=? ph id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
      '∘
      (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
      Θ_1 Σ)
   ex-#%app2]

  ;; application
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
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
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new) Θ_1 Σ)
   ex-app]

  ;; primitive application
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (not (Id? stx_fun))
   #:with             id_app := (GenStx (Sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new) Θ_1 Σ)
   ex-prim-app]

  ;; reference
  [(ζ (Stxξ ph (and id (GenStx (Sym nam) ctx)) ξ scps_p) '∘ κ Θ Σ)
   #:with           nam :=<1> (resolve ph id Σ)
   #:with all-transform :=    (lookup-ξ ξ nam)
   (match all-transform
     [(TVar id_new) (ζ id_new '• κ Θ Σ)]
     [_ (error '==>p "unbound identifier: ~a" nam)])
   ex-var]

  ;; literal
  [(ζ (Stxξ ph (GenStx (? Atom? atom) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (not (Id? (GenStx atom ctx)))
   (ζ (GenStx `(,(GenStx (Sym 'quote) ctx) ,(GenStx atom ctx)) ctx) '• κ Θ Σ)
   ex-lit]

  ;; pop κ
  [(ζ stx '• (κ stx_c ex? 𝓁) Θ Σ)
   #:with κ0 := (lookup-κ Θ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 Θ Σ)
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                         ,(GenStx (? ProperStl? val_dones) _)
                         ,stx_exp0 ,stl_exps ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_seq '#%seq Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph stx_exp0 ξ scps_p) '∘
      (κ
       (GenStx
        `(,(Stxξ ph id_seq ξ scps_p)
          ,(GenStx `(,id-snoc ,(GenStx val_dones (empty-ctx)) ,(Hole))
                   (empty-ctx))
          ,@stl_exps) ctx) '∘ 𝓁_new) Θ_1 Σ)
   ex-seq-cons]

  [(ζ (GenStx `(,(Stxξ ph (? Id? id_seq) ξ scps_p)
                ,(GenStx `(,(? Id? id_snoc)
                           ,(GenStx (? ProperStl? val_dones) ctx_1)
                           ,(? Stx? stx_done)) _)
                ,stl_exps ...) ctx) '∘ κ Θ Σ)
   #:when (and (id=? ph id_seq '#%seq  Σ) (id=? ph id_snoc '#%snoc Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ ph (GenStx `(,id_seq ,(GenStx val_dones2 ctx_1)
                                 ,@stl_exps) ctx) ξ scps_p) '∘ κ Θ Σ)
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                         ,(GenStx (? ProperStl? val_dones) _)) ctx) ξ scps_p)
      '∘ κ Θ Σ)
   #:when (id=? ph id_seq '#%seq Σ)
   (ζ (GenStx val_dones ctx) '• κ Θ Σ)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift (-->c s1))
   (InEval s2 ζ0)
   ex-in-eval])

(define ==>p ((reducer-of ==>p/Σ) := -->c))

;(: expand : Ph Stx ξ Scps Σ -> (Cons Stx Σ))
(define ((expand/==> ==>) ph stx ξ scps_p Σ)
  (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new))
                 (apply-reduction-relation* ==> init-ζ)])
      (cons stx_new Σ_new))))

(define expand (expand/==> ==>p))
