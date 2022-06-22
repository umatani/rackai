#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"

 (only-in "../../../struct-common-sig.rkt" struct-common^)
 (only-in "struct-stxe-sig.rkt"            struct-stxe^)

 (only-in "../../../syntax-sig.rkt"        syntax^)
 (only-in "../../../env-sig.rkt"           env^)
 (only-in "../../../store-sig.rkt"         store^)
 (only-in "../../../menv-sig.rkt"          menv^)
 (only-in "../../../mstore-sig.rkt"        mstore^)
 (only-in "../../../mcont-sig.rkt"         mcont^)
 (only-in "../../../eval-sig.rkt"          eval^)
 (only-in "../../../expand-sig.rkt"        expand^)
 (only-in "../../../parse-sig.rkt"         parse^)
 (only-in "../../../phase-sig.rkt"         phase^))
(provide expand-red@ expand@ ==>)

;; ----------------------------------------
;; The expander:


;; ==> :  ζ -> (Setof ζ)
(define-reduction (==> :=<1> -->)
  #:within-signatures [struct-common^ struct-stxe^ syntax^ env^ store^
                       menv^ mstore^ mcont^ phase^ parse^]
  #:do [;; Constants:
        (define id-kont (stx (sym '#%kont) (empty-ctx)))
        (define id-seq  (stx (sym '#%seq)  (empty-ctx)))
        (define id-snoc (stx (sym '#%snoc) (empty-ctx)))
        (define stx-nil (stx '()           (empty-ctx)))
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
                           [(ξ_2) (extend-ξ ξ_1 nam_new (tvar id_new))])
               (values (cons id_new stl_reg) ξ_2 Σ_3))]))]

  ;; lambda
  [(ζ (Stxξ ph (Stx `(,(? id? id_lam)
                        ,(Stx (? proper-stl? stl_args) ctx_0)
                        ,stx_body) ctx)
              ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_lam 'lambda Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars ph scp_new stl_args ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
          '∘
          (mk-κ (stx `(,id_lam
                        ,(stx stl_args2 ctx_0)
                        ,(hole)) ctx)
                 '• 𝓁_new) Θ_1 Σ_2)
   ex-lam-body]

  ;; let
  [(ζ (Stxξ ph (Stx `(,(? id? id_let)
                        ,(Stx (? proper-stl? stl_binds) ctx_1)
                        ,stx_body) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_let 'let Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with         (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars ph scp_new stl_vars ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (add ph stx_body scp_new) ξ_new (union (set scp_new) scps_p))
          '∘
          (mk-κ (stx `(,id-kont
                        ,id_let
                        ,(stx&ξ ph (stx `(,(stx stl_vars2 ctx_1)
                                           ,(stx stl_rhs ctx_1)
                                           ) ctx_1) ξ scps_p)
                        ,(hole)) ctx) '∘ 𝓁_new) Θ_1 Σ_2)
   ex-let-body]

  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_let)
              ,(Stxξ ph (Stx
                          `(,(Stx (? proper-stl? stl_vars) _)
                            ,(Stx (? proper-stl? stl_rhs) _)) ctx_1)
                      ξ scps_p)
              ,stx_body) ctx) '∘ κ0 Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_let  'let    Σ))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (stx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ scps_p)
          '∘
          (mk-κ
           (stx&ξ ph (stx `(,id_kont
                             ,id_let
                             ,(stx `(,(stx stl_vars ctx_1) ,(hole)) ctx_1)
                             ,stx_body) ctx)
                   ξ scps_p) '∘ 𝓁_new) Θ_1 Σ)
   ex-let-rhs]

  [(ζ (Stxξ ph (Stx `(,(? id? id_kont)
                        ,(? id? id_let)
                        ,(Stx `(,(Stx (? proper-stl? stl_vars) _)
                                ,(Stx (? proper-stl? val_rhs) _)) ctx_1)
                        ,stx_body) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_let  'let    Σ))
   (mk-ζ (stx `(,id_let ,(stx (zip stl_vars val_rhs ctx_1) ctx_1)
                         ,stx_body) ctx) '• κ0 Θ Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ ph (and stx0 (Stx `(,(? id? id_quote) ,_) _)) _ _) '∘ κ0 Θ Σ)
   #:when (id=? ph id_quote 'quote Σ)
   (mk-ζ stx0 '• κ0 Θ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ ph (Stx `(,(? id? id_syntax) ,stx0) ctx) _ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_syntax 'syntax Σ)
   #:with stx_pruned := (prune ph stx0 scps_p)
   (mk-ζ (stx `(,id_syntax ,stx_pruned) ctx) '• κ0 Θ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ ph (Stx `(,(? id? id_ls)
                        ,(Stx `(,(Stx `(,id ,stx_rhs) ctx_0)) ctx_1)
                        ,stx_body) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_ls 'let-syntax Σ)
   (mk-ζ (stx `(,id_ls
                 ,(stx `(,(stx `(,id ,stx_rhs) ctx_0)) ctx_1)
                 ,(stx&ξ ph stx_body ξ scps_p)) ctx) '∘ κ0 Θ Σ)
   ex-ξ-ls]

  [(ζ (Stx `(,(? id? id_ls)
              ,(Stx `(,(Stx `(,(? id? id) ,stx_rhs) ctx_0)) ctx_1)
              ,(Stxξ ph stx_body ξ scps_p)) ctx) '∘ κ0 Θ Σ)
   #:when (id=? ph id_ls 'let-syntax Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ) 
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add ph id scp_new)
   #:with                  Σ_3 := (bind ph Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   #:with            stx_body2 := (add ph stx_body scp_new)
   #:with              scps_p2 := (union (set scp_new) scps_p)
   (mk-ζ (stx&ξ (add1 ph) stx_rhs (init-ξ) (set))
          '∘
          (mk-κ (stx `(,id-kont
                        ,id_ls
                        ,(stx `(,(stx `(,id_new ,(hole)) ctx_0)) ctx_1)
                        ,(stx&ξ ph stx_body2 ξ scps_p2)) ctx) '∘ 𝓁_new)
          Θ_1 Σ_3)
   ex-ls-push-rhs]

  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_ls)
              ,(Stx `(,(Stx `(,(? id? id_new) ,stx_exp) ctx_0)) ctx_1)
              ,(Stxξ ph stx_body2 ξ scps_p2)) ctx) '∘ κ0 Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_ls 'let-syntax Σ))
   #:with nam_new :=<1> (resolve ph id_new Σ)
   #:with ast_exp :=<1> (parse (add1 ph) stx_exp Σ)
   (in-eval `(,(ast&env ast_exp (init-env)) • ,(init-store))
            (mk-ζ (stx `(,(stx (sym nam_new) (empty-ctx))
                          ,(stx&ξ ph stx_body2 ξ scps_p2)) (empty-ctx))
                   '∘ κ0 Θ Σ))
   ex-ls-eval]

  [(InEval `(,(? val? val) • ,_)
           (ζ (Stx `(,(Stx (Sym nam_new) _)
                      ,(Stxξ ph stx_body2 ξ scps_p2)) _) '∘ κ0 Θ Σ))
   #:with ξ_new := (extend-ξ ξ nam_new val)
   (mk-ζ (stx&ξ ph stx_body2 ξ_new scps_p2) '∘ κ0 Θ Σ)
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ ph (and stx_macapp (Stx `(,(? id? id_mac) ,_ ...) ctx)) ξ scps_p)
       '∘ κ0 Θ Σ)
   #:with            nam_mac :=<1> (resolve ph id_mac Σ)
   #:with                val :=    (lookup-ξ ξ nam_mac)
   #:when (val? val)
   #:with (values scp_u Σ_1) :=    (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) :=    (alloc-scope 'i Σ_1)
   (in-eval
    `(,(ast&env (app val (list (flip ph (add ph stx_macapp scp_u) scp_i)))
                (init-env))
      • ,(init-store))
    (mk-ζ (stx&ξ ph (stx #f (list (cons ph (set scp_i))))
                   ξ (union (set scp_u) scps_p)) '∘ κ0 Θ Σ_2))
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0)
           (ζ (Stxξ ph (Stx #f ctx_i) ξ scps_p) '∘ κ0 Θ Σ))
   #:with scp_i := (car (set->list (at-phase ctx_i ph)))
   (mk-ζ (stx&ξ ph (flip ph stx_exp scp_i) ξ scps_p) '∘ κ0 Θ Σ)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ ph (Stx `(,(? id? id_if) ,stl_exps ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_if 'if Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (stx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ scps_p)
          '∘
          (mk-κ (stx&ξ ph (stx `(,id-kont ,id_if ,(hole)) ctx) ξ scps_p)
                 '∘ 𝓁_new) Θ_1 Σ)
   ex-if]

  [(ζ (Stxξ ph (Stx `(,(? id? id_kont)
                        ,(? id? id_if)
                        ,(Stx (? proper-stl? val_exps) ctx)) _)
              ξ scps_p) '∘ κ0 Θ Σ)
   #:when (and (id=? ph id_kont '#%kont Σ) (id=? ph id_if   'if     Σ))
   (mk-ζ (stx `(,id_if ,@val_exps) ctx) '• κ0 Θ Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ ph (Stx `(,(? id? id_app)
                        ,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
          '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new) Θ_1 Σ)
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ ph (Stx
                  (cons (? id? id_app)
                        (Stx `(,stx_fun ,stl_args ...) _)) ctx) ξ scps_p)
       '∘ κ0 Θ Σ)
   #:when (id=? ph id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p)
          '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new)
          Θ_1 Σ)
   ex-#%app2]

  ;; application
  [(ζ (Stxξ ph (Stx `(,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id? stx_fun)
   #:with name :=<1> (resolve ph stx_fun Σ)
   #:with   at :=    (lookup-ξ ξ name)
   #:when (or (TVar? at)
              (and (eq? 'not-found at)
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))
   #:with             id_app := (stx (sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p) '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new) Θ_1 Σ)
   ex-app]

  ;; primitive application
  [(ζ (Stxξ ph (Stx `(,stx_fun ,stl_args ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (not (id? stx_fun))
   #:with             id_app := (stx (sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ scps_p) '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new) Θ_1 Σ)
   ex-prim-app]

  ;; reference
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ scps_p) '∘ κ0 Θ Σ)
   #:with           nam :=<1> (resolve ph id Σ)
   #:with all-transform :=    (lookup-ξ ξ nam)
   (match all-transform
     [(TVar id_new) (mk-ζ id_new '• κ0 Θ Σ)]
     [_ (error '==>p "unbound identifier: ~a" nam)])
   ex-var]

  ;; literal
  [(ζ (Stxξ ph (Stx (? atom? atom) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (not (id? (stx atom ctx)))
   (mk-ζ (stx `(,(stx (sym 'quote) ctx) ,(stx atom ctx)) ctx) '• κ0 Θ Σ)
   ex-lit]

  ;; pop κ
  [(ζ stx0 '• (κ stx_c ex? 𝓁) Θ Σ)
   #:with κ0 := (lookup-κ Θ 𝓁)
   (mk-ζ (in-hole stx_c stx0) ex? κ0 Θ Σ)
   ex-pop-κ]

  ;;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ ph (Stx `(,(? id? id_seq)
                        ,(Stx (? proper-stl? val_dones) _)
                        ,stx_exp0 ,stl_exps ...) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   #:when (id=? ph id_seq '#%seq Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ ph stx_exp0 ξ scps_p) '∘
          (mk-κ
           (stx
            `(,(stx&ξ ph id_seq ξ scps_p)
              ,(stx `(,id-snoc ,(stx val_dones (empty-ctx)) ,(hole))
                    (empty-ctx))
              ,@stl_exps) ctx) '∘ 𝓁_new) Θ_1 Σ)
   ex-seq-cons]

  [(ζ (Stx `(,(Stxξ ph (? id? id_seq) ξ scps_p)
              ,(Stx `(,(? id? id_snoc)
                      ,(Stx (? proper-stl? val_dones) ctx_1)
                      ,(? Stx? stx_done)) _)
              ,stl_exps ...) ctx) '∘ κ0 Θ Σ)
   #:when (and (id=? ph id_seq '#%seq  Σ) (id=? ph id_snoc '#%snoc Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (mk-ζ (stx&ξ ph (stx `(,id_seq ,(stx val_dones2 ctx_1)
                                    ,@stl_exps) ctx) ξ scps_p) '∘ κ0 Θ Σ)
   ex-seq-snoc]

  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ ph (Stx `(,(? id? id_seq)
                        ,(Stx (? proper-stl? val_dones) _)) ctx) ξ scps_p)
       '∘ κ0 Θ Σ)
   #:when (id=? ph id_seq '#%seq Σ)
   (mk-ζ (stx val_dones ctx) '• κ0 Θ Σ)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift (--> s1))
   (in-eval s2 ζ0)
   ex-in-eval])

(define expand-red@ (reduction->unit ==>))

(define-unit expand@
  (import (only struct-common^
                ζ mk-ζ)
          (only struct-stxe^
                stx&ξ)
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

  (define ==> (reducer := -->))

  ; expand : Ph Stx ξ Scps Σ -> (Cons Stx Σ)
  (define (expand ph stx ξ scps_p Σ)
    (let ([init-ζ (mk-ζ (stx&ξ ph stx ξ scps_p) '∘ '• (init-Θ) Σ)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new))
                   (apply-reduction-relation* ==> init-ζ)])
        (cons stx_new Σ_new))))

  ; expander : Stx -> (Values Stx Σ)
  (define (expander stx)
    (expand 0 stx (init-ξ) (set) (init-Σ))))
