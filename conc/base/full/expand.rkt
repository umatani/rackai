#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ eval^
          menv^ mstore^ mcont^ parser^ expand^)
 (only-in "terms.rkt" terms^ #%term-forms))
(provide (all-defined-out))

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> --> :=<1>)
  #:within-signatures [(only terms^
                             App% Sym% Stx% AstEnv% Stxξ% TVar% TStop% κ% Σ*%
                             ζ% InEval% Hole%)
                       (only terms-extra^
                             id? val? atom? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip snoc add flip union in-hole
                             prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-scope bind resolve id=?)
                       (only mcont^
                             lookup-κ push-κ)
                       (only parser^
                             parse)]

  #:do [(use-terms App Sym Stx AstEnv Stxξ κ ζ Σ* TVar TStop Hole InEval)
        ;; Constants:
        (define id-kont (Stx (Sym '#%kont) (empty-ctx)))
        (define id-seq  (Stx (Sym '#%seq)  (empty-ctx)))
        (define id-snoc (Stx (Sym '#%snoc) (empty-ctx)))
        (define stx-nil (Stx '()           (empty-ctx)))
        ;; This is the same as the single-phase one, but with `ph`
        ;; threaded through to `add` & `bind`
        ; regist-vars : Ph Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ)
        (define (regist-vars ph scp stl ξ Σ)
          (match stl
            ['() (values '() ξ Σ)]
            [(cons (app (λ (stx) stx) id) stl)
             (let*-values ([(stl_reg ξ_1 Σ_1)
                            (regist-vars ph scp stl ξ Σ)]
                           [(nam_new Σ_2) (alloc-name id Σ_1)]
                           [(id_new) (add ph id scp)]
                           [(Σ_3) (bind ph Σ_2 id_new nam_new)]
                           [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
               (values (cons id_new stl_reg) ξ_2 Σ_3))]))]

  ;; stops
  [(ζ (Stxξ ph (and stx (Stx `(,(? id? id_stop)
                                 ,@stl_args) ctx)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam_stop :=<1> (resolve #:phase ph id_stop Σ)
   #:when (TStop? (lookup-ξ ξ nam_stop))
   (ζ stx '• κ Θ Σ*_0)
   ex-stop]

  ;; lambda (same as phases)
  [(ζ (Stxξ ph (Stx `(,(? id? id_lam)
                        ,(Stx (? proper-stl? stl_args) ctx_0)
                        ,stx_body) ctx)
              ξ) '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_lam 'lambda #:ξ ξ Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars ph scp_new stl_args ξ Σ_1)
   #:with                         Σ*_2 := (Σ* Σ_2
                                                (union (set scp_new) scps_p)
                                                (set))
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '∘
       (κ (Stx `(,id_lam
                  ,(Stx stl_args2 ctx_0)
                  ,(Hole)) ctx) '• Σ*_0 𝓁_new) Θ_1 Σ*_2)
   ex-lam-body]

  ;; let
  [(ζ (Stxξ ph (Stx `(,(? id? id_let)
                        ,(Stx (? proper-stl? stl_binds) ctx_1)
                        ,stx_body) ctx)
              ξ) '∘ κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_let 'let #:ξ ξ Σ)
   #:with (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars ph scp_new stl_vars ξ Σ_1)
   #:with Σ*_2 := (Σ* Σ_2 (union (set scp_new) scps_p) (set))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (add ph stx_body scp_new) ξ_new) '∘
       (κ (Stx `(,id-kont
                  ,id_let
                  ,(Stxξ ph (Stx `(,(Stx stl_vars2 ctx_1)
                                    ,(Stx stl_rhs ctx_1))
                                  ctx_1) ξ)
                  ,(Hole)) ctx) '∘ Σ*_0 𝓁_new) Θ_1 Σ*_2)
   ex-let-body]
  
  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_let)
              ,(Stxξ ph (Stx
                          `(,(Stx (? proper-stl? stl_vars) _)
                            ,(Stx (? proper-stl? stl_rhs) _)) ctx_1)
                      ξ) ,stx_body) ctx) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (and (id=? #:phase ph id_kont '#%kont #:ξ ξ Σ)
               (id=? #:phase ph id_let  'let    #:ξ ξ Σ))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (Stx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ) '∘
       (κ (Stxξ ph (Stx
                      `(,id_kont
                        ,id_let
                        ,(Stx `(,(Stx stl_vars ctx_1) ,(Hole)) ctx_1)
                        ,stx_body) ctx)
                  ξ) '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-let-rhs]  

  [(ζ (Stxξ ph (Stx `(,(? id? id_kont)
                        ,(? id? id_let)
                        ,(Stx `(,(Stx (? proper-stl? stl_vars) _)
                                ,(Stx (? proper-stl? val_rhs) _)) ctx_1)
                        ,stx_body) ctx)
              ξ) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? #:phase ph id_kont '#%kont #:ξ ξ Σ)
               (id=? #:phase ph id_let 'let     #:ξ ξ Σ))
   (ζ (Stx `(,id_let ,(Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                      ,stx_body) ctx) '• κ Θ Σ*_0)
   ex-let-rhs2]

  ;; quote (same as phases)
  [(ζ (Stxξ ph (and stx (Stx `(,(? id? id_quote) ,_) _)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_quote 'quote #:ξ ξ Σ)
   (ζ stx '• κ Θ Σ*_0)
   ex-quote]

  ;; syntax (same as phases)
  [(ζ (Stxξ ph (Stx `(,(? id? id_syntax) ,stx) ctx) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_syntax 'syntax #:ξ ξ Σ)
   #:with stx_pruned := (prune ph stx scps_p)
   (ζ (Stx `(,id_syntax ,stx_pruned) ctx) '• κ Θ Σ*_0)
   ex-stx]

  ;; macro creation (eval gets more and updates store)
  [(ζ (Stxξ ph (Stx `(,(? id? id_ls)
                        ,(Stx `(,(Stx `(,id ,stx_rhs) ctx_0)) ctx_1)
                        ,stx_body) ctx) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_ls 'let-syntax #:ξ ξ Σ)
   (ζ (Stx `(,id_ls
              ,(Stx `(,(Stx `(,id ,stx_rhs) ctx_0)) ctx_1)
              ,(Stxξ ph stx_body ξ)) ctx) '∘ κ Θ Σ*_0)
   ex-ξ-ls]

  [(ζ (Stx `(,(? id? id_ls)
              ,(Stx `(,(Stx `(,(? id? id) ,stx_rhs) ctx_0)) ctx_1)
              ,(Stxξ ph stx_body ξ)) ctx) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_ls 'let-syntax #:ξ ξ Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ)
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add ph id scp_new)
   #:with                  Σ_3 := (bind ph Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (add1 ph) stx_rhs (init-ξ)) '∘
       (κ (Stx `(,id-kont
                  ,id_ls
                  ,(Stx `(,(Stx `(,id_new ,(Hole)) ctx_0)) ctx_1)
                  ,(Stxξ ph stx_body ξ)
                  ,(Stx #f (list (cons ph (set scp_new))))) ctx)
           '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ_3 (set) (set)))
   ex-ls-push-rhs]

  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_ls)
              ,(Stx `(,(Stx `(,(? id? id_new) ,stx_exp) ctx_0)) ctx_1)
              ,(Stxξ ph stx_body ξ)
              ,(Stx #f ctx_new)) ctx) '∘ κ Θ (Σ* Σ scps_p _))
   #:when (and (id=? #:phase ph id_kont '#%kont     #:ξ ξ Σ)
               (id=? #:phase ph id_ls   'let-syntax #:ξ ξ Σ))
   #:with nam_new :=<1> (resolve #:phase ph id_new Σ)
   #:with ast_exp :=<1> (parse #:phase (add1 ph) stx_exp Σ)
   (InEval `(,(AstEnv ph ast_exp (init-env) 'no-scope ξ)
             • ,(init-store) ,(Σ* Σ scps_p (set)))
           (ζ (Stx `(,(Stx (Sym nam_new) (empty-ctx))
                      ,(Stxξ ph stx_body ξ)
                      ,(Stx #f ctx_new)) (empty-ctx)) '∘
               κ Θ (Σ* Σ scps_p (set))))
   ex-ls-eval]

  [(InEval `(,(? val? val) • ,store_0 ,(Σ* Σ _ _))
           (ζ (Stx `(,(Stx (Sym nam_new) _)
                      ,(Stxξ ph stx_body ξ)
                      ,(Stx #f ctx_new)) _) '∘ κ Θ (Σ* _ scps_p _)))
   #:with scp_new   := (car (set->list (at-phase ctx_new ph)))
   #:with ξ_new     := (extend-ξ ξ nam_new val)
   #:with stx_body2 := (add ph stx_body scp_new)
   (ζ (Stxξ ph stx_body2 ξ_new) '∘
       κ Θ (Σ* Σ (union (set scp_new) scps_p) (set)))
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (Stx `(,(? id? id_mac) ,_ ...) ctx)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ scps_p scps_u)))
   #:with            nam_mac :=<1> (resolve #:phase ph id_mac Σ)
   #:with                val :=    (lookup-ξ ξ nam_mac)
   #:when (val? val)
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
    (ζ (Stxξ ph (Stx #f (list (cons ph (set scp_i)))) ξ)
        '∘ κ Θ Σ*_2)) ;; Σ*_2 not used
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0 ,Σ*)
           (ζ (Stxξ ph (Stx #f ctx_i) ξ) '∘ κ Θ _))
   #:with scp_i := (car (set->list (at-phase ctx_i ph)))
   ;(printf "after expand: ~a\n" stx_exp)
   (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ) '∘ κ Θ Σ*)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (Stx `(,(? id? id_if) ,stl_exps ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_if 'if #:ξ ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (Stx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ) '∘
       (κ (Stxξ ph (Stx `(,id-kont ,id_if ,(Hole)) ctx) ξ)
           '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-if]

  [(ζ (Stxξ ph (Stx `(,(? id? id_kont)
                        ,(? id? id_if)
                        ,(Stx (? proper-stl? val_exps) ctx)) _)
              ξ) '∘ κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? #:phase ph id_kont '#%kont #:ξ ξ Σ)
               (id=? #:phase ph id_if   'if     #:ξ ξ Σ))
   (ζ (Stx `(,id_if ,@val_exps) ctx) '• κ Θ Σ*_0)
   ex-if-kont]

  ;; application (non-canonical #%app version, same as phases)
  [(ζ (Stxξ ph (Stx `(,(? id? id_app)
                        ,stx_fun ,stl_args ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_app '#%app #:ξ ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (Stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (Stx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-#%app]

  ;; application (canonical #%app version, same as phases)
  [(ζ (Stxξ ph (Stx
                  (cons (? id? id_app)
                        (Stx `(,stx_fun ,stl_args ...) _)) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_app '#%app #:ξ ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (Stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (Stx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-#%app2]

  ;; application (same as phases)
  [(ζ (Stxξ ph (Stx `(,stx_fun ,stl_args ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id? stx_fun)
   #:with name :=<1> (resolve #:phase ph stx_fun Σ)
   #:with   at :=    (lookup-ξ ξ name)
   #:when (or (TVar? at)
              (and (eq? 'not-found at)
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (Stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (Stx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-app]

  ;; primitive application
  [(ζ (Stxξ ph (Stx `(,stx_fun ,stl_args ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (not (id? stx_fun))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph (Stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (Stx (cons id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-prim-app]

  ;; reference (same as phases)
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam :=<1> (resolve #:phase ph id Σ)
   #:with val :=    (lookup-ξ ξ nam)
   #:when (TVar? val)
   #:with (TVar id_new) := val
   (ζ id_new '• κ Θ Σ*_0)
   ex-var]

  ;; literal (same as phases)
  [(ζ (Stxξ ph (Stx (? atom? atom) ctx) ξ) '∘ κ Θ Σ*)
   #:when (not (id? (Stx atom ctx)))
   (ζ (Stx `(,(Stx (Sym 'quote) ctx) ,(Stx atom ctx)) ctx) '• κ Θ Σ*)
   ex-lit]

  ;; pop κ (merge Σ*)
  [(ζ stx '• (κ stx_c ex? (Σ* _ scps_p scps_u) 𝓁) Θ (Σ* Σ _ _))
   #:with κ0 := (lookup-κ Θ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 Θ (Σ* Σ scps_p scps_u))
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (Stx `(,(? id? id_seq)
                        ,(Stx (? proper-stl? val_dones) _)
                        ,stx_exp0 ,stl_exps ...) ctx) ξ) '∘
       κ0 Θ (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id=? #:phase ph id_seq '#%seq #:ξ ξ Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ ph stx_exp0 ξ) '∘
       (κ
        (Stx
         `(,(Stxξ ph id_seq ξ)
           ,(Stx `(,id-snoc ,(Stx val_dones (empty-ctx)) ,(Hole))
                 (empty-ctx))
           ,@stl_exps) ctx) '∘ Σ*_0 𝓁_new)
       Θ_1 (Σ* Σ scps_p (set)))
   ex-seq-cons]

  [(ζ (Stx `(,(Stxξ ph (? id? id_seq) ξ)
              ,(Stx `(,(? id? id_snoc)
                      ,(Stx (? proper-stl? val_dones) ctx_1)
                      ,(? Stx? stx_done)) _)
              ,stl_exps ...) ctx) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (and (id=? #:phase ph id_seq  '#%seq  #:ξ ξ Σ)
               (id=? #:phase ph id_snoc '#%snoc #:ξ ξ Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ ph (Stx `(,id_seq ,(Stx val_dones2 ctx_1)
                                ,@stl_exps) ctx) ξ) '∘ κ Θ Σ*_0)
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (Stx `(,(? id? id_seq)
                        ,(Stx (? proper-stl? val_dones) _)) ctx) ξ) '∘
       κ Θ (and Σ*_0 (Σ* Σ _ _)))
   #:when (id=? #:phase ph id_seq '#%seq #:ξ ξ Σ)
   (ζ (Stx val_dones ctx) '• κ Θ Σ*_0)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift ((-->) s1)) ;; extra call due to mut. rec. defs
   (InEval s2 ζ0)
   ex-in-eval])

(define-unit-from-reduction expand-red@ ==>)

(define-unit expand@
  (import (only terms^
                ζ% Stxξ% Σ*%)
          (only eval^
                -->)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only mcont^
                init-Θ)
          (only red^
                reducer))
  (export expand^)

  (use-terms ζ Stxξ Σ*)
  
  (define ==> (λ () (reducer --> :=)))

  ; expand : Ph Stx ξ Σ* -> (Cons Stx Σ*)
  (define (expand ph stx ξ Σ*)
    (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• (init-Θ) Σ*)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ*_new))
                   (apply-reduction-relation* (==>) init-ζ)])
        (cons stx_new Σ*_new))))

  ; expander : Stx -> (Cons Stx Σ*)
  (define (expander stx)
    (expand 0 stx (init-ξ) (Σ* (init-Σ) (set) (set)))))
