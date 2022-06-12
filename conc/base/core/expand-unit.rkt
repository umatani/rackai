#lang racket
(require (except-in racket set do)
         "../set.rkt" "../dprint.rkt" "../reduction.rkt"

         "../struct-sig.rkt"
         "../syntax-sig.rkt"
         "../parse-sig.rkt"
         "../env-sig.rkt"
         "../store-sig.rkt"
         "../eval-sig.rkt"
         "../menv-sig.rkt"
         "../mstore-sig.rkt"
         "../mcont-sig.rkt"
         "../expand-sig.rkt")
(provide expand-red@ expand@)

;; ----------------------------------------
;; The expander:

;; (: ==> : ζ -> (Setof ζ))
(define-reduction (==> --> :=<1>)
  #:within-signatures [struct^ syntax^ env^ store^ menv^ mstore^ mcont^ parse^]

  ;; lambda
  [(ζ (Stxξ (Stx `(,(? id? id_lam)
                     ,(Stx (? proper-stl? stl_args) ctx_0)
                     ,stx_body) ctx)
              ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_lam 'lambda Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars scp_new stl_args ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (add stx_body scp_new) ξ_new)
          '∘
          (mk-κ (stx `(,id_lam
                        ,(stx stl_args2 ctx_0)
                        ,(hole)) ctx) '• 𝓁_new)
          Θ_1 Σ_2)
   ex-lam-body]

  ;; let
  [(ζ (Stxξ (Stx `(,(? id? id_let)
                     ,(Stx (? proper-stl? stl_binds) ctx_1)
                     ,stx_body) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_let 'let Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with         (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars scp_new stl_vars ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (add stx_body scp_new) ξ_new)
          '∘
          (mk-κ (stx `(,id-kont
                        ,id_let
                        ,(stx&ξ (stx `(,(stx stl_vars2 ctx_1)
                                        ,(stx stl_rhs ctx_1))
                                      ctx_1) ξ)
                        ,(hole)) ctx) '∘ 𝓁_new)
          Θ_1 Σ_2)
   ex-let-body]

  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_let)
              ,(Stxξ (Stx
                       `(,(Stx (? proper-stl? stl_vars) _)
                         ,(Stx (? proper-stl? stl_rhs) _)) ctx_1) ξ)
              ,stx_body) ctx) '∘ κ0 Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_let  'let    Σ))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (stx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ)
          '∘
          (mk-κ (stx `(,id_kont
                        ,id_let
                        ,(stx `(,(stx stl_vars ctx_1) ,(hole)) ctx_1)
                        ,stx_body) ctx) '∘ 𝓁_new)
          Θ_1 Σ)
   ex-let-rhs]

  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_let)
              ,(Stx `(,(Stx (? proper-stl? stl_vars) _)
                      ,(Stx (? proper-stl? val_rhs) _)) ctx_1)
              ,stx_body) ctx) '∘ κ Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_let  'let    Σ))
   (mk-ζ (stx `(,id_let ,(stx (zip stl_vars val_rhs ctx_1) ctx_1)
                         ,stx_body) ctx) '• κ Θ Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ (and stx0 (Stx `(,(? id? id_quote) ,_) _)) _) '∘ κ Θ Σ)
   #:when (id=? id_quote 'quote Σ)
   (mk-ζ stx0 '• κ Θ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ (and stx0 (Stx `(,(? id? id_syntax) ,_) _)) _) '∘ κ Θ Σ)
   #:when (id=? id_syntax 'syntax Σ)
   (mk-ζ stx0 '• κ Θ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ (Stx `(,(? id? id_ls)
                     ,(Stx `(,(Stx `(,id ,stx_rhs) ctx_0)) ctx_1)
                     ,stx_body) ctx) ξ) '∘ κ Θ Σ)
   #:when (id=? id_ls 'let-syntax Σ)
   (mk-ζ (stx `(,id_ls
                 ,(stx `(,(stx `(,id ,stx_rhs) ctx_0)) ctx_1)
                 ,(stx&ξ stx_body ξ)) ctx) '∘ κ Θ Σ)
   ex-ξ-ls]

  [(ζ (Stx `(,(? id? id_ls)
              ,(Stx `(,(Stx `(,(? id? id) ,stx_rhs) ctx_0)) ctx_1)
              ,(Stxξ stx_body ξ)) ctx) '∘ κ0 Θ Σ)
   #:when (id=? id_ls 'let-syntax Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ)
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add id scp_new)
   #:with                  Σ_3 := (bind Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   #:with            stx_body2 := (add stx_body scp_new)
   (mk-ζ (stx&ξ stx_rhs (init-ξ))
          '∘
          (mk-κ (stx `(,id-kont
                        ,id_ls
                        ,(stx `(,(stx `(,id_new ,(hole)) ctx_0)) ctx_1)
                        ,(stx&ξ stx_body2 ξ)) ctx) '∘ 𝓁_new) Θ_1 Σ_3)
   ex-ls-push-rhs]

  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_ls)
              ,(Stx `(,(Stx `(,(? id? id_new) ,stx_exp) ctx_0)) ctx_1)
              ,(Stxξ stx_body2 ξ)) ctx) '∘ κ Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_ls 'let-syntax Σ))
   #:with nam_new :=<1> (resolve id_new Σ)
   #:with ast_exp :=<1> (parse stx_exp Σ)
   (in-eval `(,(ast&env ast_exp (init-env)) • ,(init-store))
            (mk-ζ (stx `(,(stx (sym nam_new) (empty-ctx))
                          ,(stx&ξ stx_body2 ξ)) (empty-ctx))
                   '∘ κ Θ Σ))
   ex-ls-eval]

  [(InEval `(,(? val? val) • ,_)
           (ζ (Stx `(,(Stx (Sym nam_new) _)
                      ,(Stxξ stx_body2 ξ)) _) '∘ κ Θ Σ))
   #:with ξ_new := (extend-ξ ξ nam_new val)
   (mk-ζ (stx&ξ stx_body2 ξ_new) '∘ κ Θ Σ)
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ (and stx_macapp (Stx `(,(? id? id_mac) ,_ ...) ctx)) ξ)
       '∘ κ Θ Σ)
   #:with            nam_mac :=<1> (resolve id_mac Σ)
   #:with                val :=    (lookup-ξ ξ nam_mac)
   #:when (val? val)
   #:with (values scp_u Σ_1) :=    (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) :=    (alloc-scope 'i Σ_1)
   (in-eval
    `(,(ast&env (app val
                     (list (flip (add stx_macapp scp_u) scp_i))) (init-env))
      • ,(init-store))
    (mk-ζ (stx&ξ (stx #f (set scp_i)) ξ) '∘ κ Θ Σ_2))
   ex-macapp-eval]

  [(InEval `(,(? stx? stx_exp) • ,store_0)
           (ζ (Stxξ (Stx #f scps) ξ) '∘ κ Θ Σ))
   #:with scp_i := (car (set->list scps))
   (mk-ζ (stx&ξ (flip stx_exp scp_i) ξ) '∘ κ Θ Σ)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ (Stx `(,(? id? id_if) ,stl_exps ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_if 'if Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (stx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ)
          '∘
          (mk-κ (stx `(,id-kont ,id_if ,(hole)) ctx) '∘ 𝓁_new)
          Θ_1 Σ)
   ex-if]

  [(ζ (Stx `(,(? id? id_kont)
              ,(? id? id_if)
              ,(Stx (? proper-stl? val_exps) ctx)) _) '∘ κ Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_if   'if     Σ))
   (mk-ζ (stx `(,id_if ,@val_exps) ctx) '• κ Θ Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ (Stx `(,(? id? id_app)
                     ,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
          '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new)
          Θ_1 Σ)
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ (Stx (cons (? id? id_app)
                         (Stx `(,stx_fun ,stl_args ...) _)) ctx) ξ)
       '∘ κ0 Θ Σ)
   #:when (id=? id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
          '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new)
          Θ_1 Σ)
   ex-#%app2]

  ;; application
  [(ζ (Stxξ (Stx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id? stx_fun)
   #:with name :=<1> (resolve stx_fun Σ)
   #:with   at :=    (lookup-ξ ξ name)
   #:when (or (TVar? at)
              (and (eq? 'not-found at)
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))
   #:with             id_app := (stx (sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new)
          Θ_1 Σ)
   ex-app]

  ;; primitive application
  [(ζ (Stxξ (Stx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (not (id? stx_fun))
   #:with             id_app := (stx (sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ (stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
          (mk-κ (stx (cons id_app (hole)) ctx) '• 𝓁_new)
          Θ_1 Σ)
   ex-prim-app]

  ;; reference
  [(ζ (Stxξ (and id (Stx (Sym nam) ctx)) ξ) '∘ κ Θ Σ)
   #:with           nam :=<1> (resolve id Σ)
   #:with all-transform :=    (lookup-ξ ξ nam)
   (match all-transform
     [(TVar id_new) (mk-ζ id_new '• κ Θ Σ)]
     [_ (error '==> "unbound identifier: ~a" nam)])
   ex-var]
  
  ;; literal
  [(ζ (Stxξ (Stx (? atom? atom) ctx) ξ) '∘ κ Θ Σ)
   #:when (not (id? (stx atom ctx)))
   (mk-ζ (stx `(,(stx (sym 'quote) ctx) ,(stx atom ctx)) ctx) '• κ Θ Σ)
   ex-lit]

  ;; pop κ
  [(ζ stx0 '• (κ stx_c ex? 𝓁) Θ Σ)
   #:with κ0 := (lookup-κ Θ 𝓁)
   (mk-ζ (in-hole stx_c stx0) ex? κ0 Θ Σ)
   ex-pop-κ]

  ;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ (Stx `(,(? id? id_seq)
                     ,(Stx (? proper-stl? val_dones) _)
                     ,stx_exp0 ,stl_exps ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_seq '#%seq Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (mk-ζ (stx&ξ stx_exp0 ξ) '∘
          (mk-κ
           (stx
            `(,(stx&ξ id_seq ξ)
              ,(stx `(,id-snoc ,(stx val_dones (empty-ctx)) ,(hole))
                    (empty-ctx))
              ,@stl_exps) ctx) '∘ 𝓁_new)
          Θ_1 Σ)
   ex-seq-cons]

  [(ζ (Stx `(,(Stxξ (? id? id_seq) ξ)
              ,(Stx `(,(? id? id_snoc)
                      ,(Stx (? proper-stl? val_dones) ctx_1)
                      ,(? stx? stx_done)) _)
              ,stl_exps ...) ctx) '∘ κ Θ Σ)
   #:when (and (id=? id_seq  '#%seq  Σ) (id=? id_snoc '#%snoc Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (mk-ζ (stx&ξ (stx `(,id_seq ,(stx val_dones2 ctx_1)
                                 ,@stl_exps) ctx) ξ) '∘ κ Θ Σ)
   ex-seq-snoc]
  
  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ (Stx `(,(? id? id_seq)
                     ,(Stx (? proper-stl? val_dones) _)) ctx) ξ) '∘ κ Θ Σ)
   #:when (id=? id_seq '#%seq Σ)
   (mk-ζ (stx val_dones ctx) '• κ Θ Σ)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift (--> s1))
   (in-eval s2 ζ0)
   ex-in-eval])

(define expand-red@ (reduction->unit ==>))

(define-unit expand@
  (import (only struct^ ζ mk-ζ stx&ξ)
          (only eval^ -->)
          (only menv^ init-ξ)
          (only mstore^ init-Σ)
          (only mcont^ init-Θ)
          (only red^ reducer))
  (export expand^)

  (define ==> (reducer --> :=))

  ;(: expand : Stx ξ Σ -> (Cons Stx Σ))
  (define (expand stx0 ξ Σ)
    (let ([init-ζ (mk-ζ (stx&ξ stx0 ξ) '∘ '• (init-Θ) Σ)])
      (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new))
                   (apply-reduction-relation* ==> init-ζ)])
        (cons stx_new Σ_new))))

  ;(: expander : Stx -> (Values Stx Σ))
  (define (expander stx)
    (expand stx (init-ξ) (init-Σ))))
