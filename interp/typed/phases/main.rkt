#lang typed/racket
(require "../reduction.rkt"
         "types.rkt"
         "misc.rkt"
         (only-in "../example.rkt" core:examples phases:examples)
         (for-syntax racket))
;; for Ctx ::= Ph -> Scps
(require/typed racket/dict
  [dict-ref (-> Ctx Ph (-> Scps) Scps)]
  [dict-set (-> Ctx Ph Scps Ctx)])
(provide run)

;; ----------------------------------------
;; Implementation of primitives:

(include "../delta.rktl")

;; ----------------------------------------
;; Evaluating AST:

(include "../eval.rktl")
(include "../core/eval.rktl")

;; ----------------------------------------
;; Parsing:

(include "../core/parsing.rktl")
(include "parsing.rktl")

;; ----------------------------------------
;; The expander:

(define (empty-ctx) : Ctx (ann '() Ctx))

(include "../core/expand.rktl")

;; This is the same as the single-phase one, but with `ph`
;; threaded through to `add` & `bind`
(: regist-vars : Ph Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define (regist-vars ph scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) (cast stx Id)) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars ph scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (cast (add ph id scp) Id)]
                   [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))

;; (: ==>c :  ζ -> (Setof ζ))
(define-reduction-relation ==>c ζ State

  ;; lambda
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_lam)
                           ,(GenStx (? ProperStl? stl_args) ctx_0)
                           ,stx_body) ctx)
              ξ scps_p) '∘ κ Θ Σ)
   #:when (eq? 'lambda (resolve ph id_lam Σ))
   (let*-values ([(scp_new Σ_1) (alloc-scope Σ)]
                 [(stl_args2 ξ_new Σ_2)
                  (regist-vars ph scp_new stl_args ξ Σ_1)]
                 [(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
         '∘
         (Mk-κ (GenStx `(,id_lam
                          ,(GenStx stl_args2 ctx_0)
                          ,(Hole)) ctx)
                '• 𝓁_new)
         Θ_1 Σ_2))
   ex-lam-body]

  ;; let
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_let)
                           ,(GenStx (? ProperStl? stl_binds) ctx_1)
                           ,stx_body) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (eq? 'let (resolve ph id_let Σ))
   (let*-values ([(stl_vars stl_rhs) (unzip stl_binds)]
                 [(scp_new Σ_1) (alloc-scope Σ)]
                 [(stl_vars2 ξ_new Σ_2) (regist-vars ph scp_new stl_vars ξ Σ_1)]
                 [(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
         '∘
         (Mk-κ (GenStx `(,id-kont
                          ,id_let
                          ,(Stxξ ph (GenStx `(,(GenStx stl_vars2 ctx_1)
                                               ,(GenStx stl_rhs ctx_1)
                                               ) ctx_1) ξ scps_p)
                          ,(Hole)) ctx) '∘ 𝓁_new)
         Θ_1 Σ_2))
   ex-let-body]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_let)
                 ,(Stxξ ph (GenStx
                             `(,(GenStx (? ProperStl? stl_vars) _)
                               ,(GenStx (? ProperStl? stl_rhs) _)) ctx_1)
                         ξ scps_p)
                 ,stx_body) ctx) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont (resolve ph id_kont Σ))
               (eq? 'let (resolve ph id_let Σ)))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ scps_p)
         '∘
         (Mk-κ
          (Stxξ ph (GenStx `(,id_kont
                              ,id_let
                              ,(GenStx `(,(GenStx stl_vars ctx_1) ,(Hole)) ctx_1)
                              ,stx_body) ctx)
                 ξ scps_p) '∘ 𝓁_new)
         Θ_1 Σ))
   ex-let-rhs]

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                           ,(? Id? id_let)
                           ,(GenStx `(,(GenStx (? ProperStl? stl_vars) _)
                                      ,(GenStx (? ProperStl? val_rhs) _)) ctx_1)
                           ,stx_body) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont (resolve ph id_kont Σ))
               (eq? 'let (resolve ph id_let Σ)))
   (ζ (GenStx `(,id_let ,(GenStx (zip stl_vars val_rhs ctx_1) ctx_1)
                         ,stx_body) ctx)
       '• κ Θ Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ ph (and stx (GenStx `(,(? Id? id_quote) ,_) _)) _ _) '∘ κ Θ Σ)
   #:when (eq? 'quote (resolve ph id_quote Σ))
   (ζ stx '• κ Θ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_syntax) ,stx) ctx) _ scps_p) '∘ κ Θ Σ)
   #:when (eq? 'syntax (resolve ph id_syntax Σ))
   (let ([stx_pruned (prune ph stx scps_p)])
     (ζ (GenStx `(,id_syntax ,stx_pruned) ctx) '• κ Θ Σ))
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_ls)
                           ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                           ,stx_body) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (eq? 'let-syntax (resolve ph id_ls Σ))
   (ζ (GenStx `(,id_ls
                 ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ scps_p)) ctx)
       '∘ κ Θ Σ)
   ex-ξ-ls]

  [(ζ (GenStx `(,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id) ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body ξ scps_p)) ctx) '∘ κ Θ Σ)
   #:when (eq? 'let-syntax (resolve ph id_ls Σ))
   (let*-values ([(nam_new Σ_1) (alloc-name id Σ)]
                 [(scp_new Σ_2) (alloc-scope Σ_1)]
                 [(id_new) (cast (add ph id scp_new) Id)]
                 [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                 [(𝓁_new Θ_1) (push-κ Θ κ)]
                 [(stx_body2) (add ph stx_body scp_new)]
                 [(scps_p2) (union (set scp_new) scps_p)] ; new
                 )
     (ζ (Stxξ (add1 ph) stx_rhs (init-ξ) (set))
         '∘
         (Mk-κ (GenStx `(,id-kont
                          ,id_ls
                          ,(GenStx `(,(GenStx `(,id_new ,(Hole)) ctx_0)) ctx_1)
                          ,(Stxξ ph stx_body2 ξ scps_p2)) ctx) '∘ 𝓁_new)
         Θ_1 Σ_3))
   ex-ls-push-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ ph stx_body2 ξ scps_p2)) ctx) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont     (resolve ph id_kont Σ))
               (eq? 'let-syntax (resolve ph id_ls Σ)))
   (let ([nam_new (resolve ph id_new Σ)])
     (InEval `(,(AstEnv (parse (add1 ph) stx_exp Σ) (init-env)) • ,(init-store))
             (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                           ,(Stxξ ph stx_body2 ξ scps_p2)) (empty-ctx))
                 '∘ κ Θ Σ)))
   ex-ls-eval]

  [(InEval `(,(? Val? val) • ,_)
           (ζ (GenStx `(,(GenStx (Sym nam_new) _)
                         ,(Stxξ ph stx_body2 ξ scps_p2)) _) '∘ κ Θ Σ))
   (let ([ξ_new (extend-ξ ξ nam_new val)])
     (ζ (Stxξ ph stx_body2 ξ_new scps_p2) '∘ κ Θ Σ))
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ scps_p)
       '∘ κ Θ Σ)
   #:when (Val? (lookup-ξ ξ (resolve ph id_mac Σ)))
   (let*-values ([(val) (lookup-ξ ξ (resolve ph id_mac Σ))]
                 [(scp_u Σ_1) (alloc-scope Σ)]
                 [(scp_i Σ_2) (alloc-scope Σ_1)])
     (InEval
      `(,(AstEnv (App (cast val Val)
                      (list (flip ph (add ph stx_macapp scp_u) scp_i)))
                 (init-env))
        • ,(init-store))
      (ζ (Stxξ ph (GenStx #f (list (cons ph (set scp_i))))
                 ξ (union (set scp_u) scps_p)) '∘ κ Θ Σ_2)))
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0)
           (ζ (Stxξ ph (GenStx #f ctx_i) ξ scps_p) '∘ κ Θ Σ))
   (let ([scp_i (car (set->list (at-phase ctx_i ph)))])
     (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ scps_p) '∘ κ Θ Σ))
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_if) ,stl_exps ...) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (eq? 'if (resolve ph id_if Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ scps_p)
         '∘
         (Mk-κ (Stxξ ph (GenStx `(,id-kont ,id_if ,(Hole)) ctx) ξ scps_p)
                '∘ 𝓁_new)
         Θ_1 Σ))
   ex-if]

  [(ζ (Stxξ ph (GenStx `(,(? Id? id_kont)
                           ,(? Id? id_if)
                           ,(GenStx (? ProperStl? val_exps) ctx)) _)
              ξ scps_p) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont (resolve ph id_kont Σ))
               (eq? 'if     (resolve ph id_if Σ)))
   (ζ (GenStx `(,id_if ,@val_exps) ctx) '• κ Θ Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_app)
                           ,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (eq? '#%app (resolve ph id_app Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
         '∘
         (Mk-κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (GenStx
                  (cons (? Id? id_app)
                        (GenStx `(,stx_fun ,stl_args ...) _)) ctx) ξ scps_p)
       '∘ κ Θ Σ)
   #:when (eq? '#%app (resolve ph id_app Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
         '∘
         (Mk-κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-#%app2]

  ;; application
  [(ζ (Stxξ ph (GenStx `(,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ Θ Σ)
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
                [(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
         '∘
         (Mk-κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-app]

  ;; reference
  [(ζ (Stxξ ph (and id (GenStx (Sym nam) ctx)) ξ scps_p) '∘ κ Θ Σ)
   (let ([all-transform (lookup-ξ ξ (resolve ph id Σ))])
     (match all-transform
       [(TVar id_new) (ζ id_new '• κ Θ Σ)]
       [_ (error '==>c "unbound identifier: ~a" nam)]))
   ex-var]

  ;; literal
  [(ζ (Stxξ ph (GenStx (? Atom? atom) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (not (Id? (GenStx atom ctx)))
   (ζ (GenStx `(,(GenStx (Sym 'quote) ctx) ,(GenStx atom ctx)) ctx) '• κ Θ Σ)
   ex-lit]

  ;; pop κ
  [(ζ stx '• (Mk-κ stx_c ex? 𝓁) Θ Σ)
   (let ([κ (lookup-κ Θ 𝓁)])
     (ζ (in-hole stx_c stx) ex? κ Θ Σ))
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                           ,(GenStx (? ProperStl? val_dones) _)
                           ,stx_exp0 ,stl_exps ...) ctx) ξ scps_p) '∘ κ Θ Σ)
   #:when (eq? '#%seq (resolve ph id_seq Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ ph stx_exp0 ξ scps_p) '∘
         (Mk-κ
          (GenStx
           `(,(Stxξ ph id_seq ξ scps_p)
             ,(GenStx `(,id-snoc ,(GenStx val_dones (empty-ctx)) ,(Hole))
                      (empty-ctx))
             ,@stl_exps) ctx) '∘ 𝓁_new)
         Θ_1 Σ))
   ex-seq-cons]

  [(ζ (GenStx `(,(Stxξ ph (? Id? id_seq) ξ scps_p)
                 ,(GenStx `(,(? Id? id_snoc)
                            ,(GenStx (? ProperStl? val_dones) ctx_1)
                            ,(? Stx? stx_done)) _)
                 ,stl_exps ...) ctx) '∘ κ Θ Σ)
   #:when (and (eq? '#%seq  (resolve ph id_seq Σ))
               (eq? '#%snoc (resolve ph id_snoc Σ)))
   (let ([val_dones2 (snoc val_dones stx_done)])
     (ζ (Stxξ ph (GenStx `(,id_seq ,(GenStx val_dones2 ctx_1)
                                     ,@stl_exps) ctx) ξ scps_p)
         '∘ κ Θ Σ))
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (GenStx `(,(? Id? id_seq)
                           ,(GenStx (? ProperStl? val_dones) _)) ctx) ξ scps_p)
       '∘ κ Θ Σ)
   #:when (eq? '#%seq (resolve ph id_seq Σ))
   (ζ (GenStx val_dones ctx) '• κ Θ Σ)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with ((reducer-of -->c) s1)
   (λ ([s2 : State]) (InEval s2 ζ0))
   ex-in-eval])

(: expand : Ph Stx ξ Scps Σ -> (Values Stx Σ))
(define (expand ph stx ξ scps_p Σ)
  (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
    (match-let ([(list (ζ stx_new '• '• Θ_new Σ_new))
                 (apply-reduction-relation* (reducer-of ==>c) init-ζ)])
      (values stx_new Σ_new))))

;; for debug

(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  ((reducer-of ==>c)
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))))

(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Listof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   (reducer-of ==>c)
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))


;; ----------------------------------------
;; Drivers

(include "../core/drivers.rktl")

(: expander : Stx -> (Values Stx Σ))
(define (expander stx)
  (expand 0 stx (init-ξ) (set) (init-Σ)))

(: parser : Stx Σ -> Ast)
(define (parser stx Σ) (parse 0 stx Σ))

(define-runner run
  reader
  expander
  stripper printer
  eval
  parser)


(define (main [mode : Symbol 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))
