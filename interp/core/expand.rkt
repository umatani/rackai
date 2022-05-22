#lang racket
(require "../reduction.rkt"
         "../dprint.rkt"
         "struct.rkt"
         (only-in "syntax.rkt"
                  empty-ctx snoc zip unzip in-hole
                  add flip bind resolve)
         (only-in "parse.rkt" parse)
         (only-in "eval.rkt" init-env init-store -->c))
(provide (all-defined-out))

;; ----------------------------------------
;; The expander:

;; ----------------------------------------
;; Expand-time environment operations:

; (: init-ξ : -> ξ)
(define (init-ξ) (make-immutable-hash))

; (: lookup-ξ : ξ Nam -> AllTransform)
(define (lookup-ξ ξ nam) (hash-ref ξ nam (λ () 'not-found)))

; (: extend-ξ : ξ Nam AllTransform -> ξ)
(define (extend-ξ ξ nam all-transform) (hash-set ξ nam all-transform))

;; ----------------------------------------
;; Expand-time stack operations:

; (: init-Θ : -> Θ)
(define (init-Θ) (Θ 0 (make-immutable-hash)))

; (: alloc-κ : Θ -> (Values 𝓁 Θ))
(define (alloc-κ θ)
  (match-let ([(Θ size tbl) θ])
    (values (𝓁 (string->symbol (format "k~a" size)))
            (Θ (add1 size) tbl))))

; (: lookup-κ : Θ 𝓁 -> κ)
(define (lookup-κ θ 𝓁) (hash-ref (Θ-tbl θ) 𝓁))

; (: update-κ : Θ 𝓁 κ -> Θ)
(define (update-κ θ 𝓁 κ)
  (match-let ([(Θ size tbl) θ])
    (Θ size (hash-set tbl 𝓁 κ))))

; (: push-κ : Θ κ -> (Values 𝓁 Θ))
(define (push-κ θ κ)
  (let-values ([(𝓁 θ_1) (alloc-κ θ)])
    (values 𝓁 (update-κ θ_1 𝓁 κ))))

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

; (: init-Σ : -> Σ)
(define (init-Σ) (Σ 0 (make-immutable-hash)))

; (: alloc-name : Id Σ -> (Values Nam Σ))
(define (alloc-name id Σ0)
  (dprint 'core 'alloc-name "")
  (match-let ([(GenStx (Sym nam) _) id]
              [(Σ size tbl) Σ0])
    (values (string->symbol (format "~a:~a" nam size))
            (Σ (add1 size) tbl))))

; (: alloc-scope : Symbol Σ -> (Values Scp Σ))
(define (alloc-scope s Σ0)
  (dprint 'core 'alloc-scope "")
  (match-let ([(Σ size tbl) Σ0])
    (values (string->symbol (format "~a::~a" s size))
            (Σ (add1 size) tbl))))

;(: regist-vars : Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define (regist-vars scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) stx) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (add id scp)]
                   [(Σ_3) (bind Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))

(define id-kont (GenStx (Sym '#%kont) (empty-ctx)))
(define id-seq (GenStx (Sym '#%seq)  (empty-ctx)))
(define id-snoc (GenStx (Sym '#%snoc) (empty-ctx)))
(define stx-nil (GenStx '() (empty-ctx)))

;; (: ==>c : ζ -> (Setof ζ))
(define-parameterized-reduction-relation ==>c/Σ
  (bind resolve alloc-name alloc-scope regist-vars parse -->c)

  ;; lambda
  [(ζ (Stxξ (GenStx `(,(? Id? id_lam)
                        ,(GenStx (? ProperStl? stl_args) ctx_0)
                        ,stx_body) ctx)
              ξ) '∘ κ0 Θ Σ)
   #:when (eq? 'lambda (resolve id_lam Σ))
   (let*-values ([(scp_new Σ_1) (alloc-scope 'lam Σ)]
                 [(stl_args2 ξ_new Σ_2)
                  (regist-vars scp_new stl_args ξ Σ_1)]
                 [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (add stx_body scp_new) ξ_new)
         '∘
         (κ (GenStx `(,id_lam
                       ,(GenStx stl_args2 ctx_0)
                       ,(Hole)) ctx)
             '• 𝓁_new)
         Θ_1 Σ_2))
   ex-lam-body]

  ;; let
  [(ζ (Stxξ (GenStx `(,(? Id? id_let)
                        ,(GenStx (? ProperStl? stl_binds) ctx_1)
                        ,stx_body) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (eq? 'let (resolve id_let Σ))
   (let*-values ([(stl_vars stl_rhs) (unzip stl_binds)]
                 [(scp_new Σ_1) (alloc-scope 'let Σ)]
                 [(stl_vars2 ξ_new Σ_2) (regist-vars scp_new stl_vars ξ Σ_1)]
                 [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (add stx_body scp_new) ξ_new)
         '∘
         (κ (GenStx `(,id-kont
                       ,id_let
                       ,(Stxξ (GenStx `(,(GenStx stl_vars2 ctx_1)
                                         ,(GenStx stl_rhs ctx_1)
                                         ) ctx_1) ξ)
                       ,(Hole)) ctx) '∘ 𝓁_new)
         Θ_1 Σ_2))
   ex-let-body]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_let)
                 ,(Stxξ (GenStx
                          `(,(GenStx (? ProperStl? stl_vars) _)
                            ,(GenStx (? ProperStl? stl_rhs) _)) ctx_1) ξ)
                 ,stx_body) ctx) '∘ κ0 Θ Σ)
   #:when (and (eq? '#%kont (resolve id_kont Σ))
               (eq? 'let (resolve id_let Σ)))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ)
         '∘
         (κ (GenStx `(,id_kont
                       ,id_let
                       ,(GenStx `(,(GenStx stl_vars ctx_1) ,(Hole)) ctx_1)
                       ,stx_body) ctx) '∘ 𝓁_new)
         Θ_1 Σ))
   ex-let-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_let)
                 ,(GenStx `(,(GenStx (? ProperStl? stl_vars) _)
                            ,(GenStx (? ProperStl? val_rhs) _)) ctx_1)
                 ,stx_body) ctx) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont (resolve id_kont Σ))
               (eq? 'let (resolve id_let Σ)))
   (ζ (GenStx `(,id_let ,(GenStx (zip stl_vars val_rhs ctx_1) ctx_1)
                         ,stx_body) ctx)
       '• κ Θ Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ (and stx (GenStx `(,(? Id? id_quote) ,_) _)) _) '∘ κ Θ Σ)
   #:when (eq? 'quote (resolve id_quote Σ))
   (ζ stx '• κ Θ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ (and stx (GenStx `(,(? Id? id_syntax) ,_) _)) _) '∘ κ Θ Σ)
   #:when (eq? 'syntax (resolve id_syntax Σ))
   (ζ stx '• κ Θ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ (GenStx `(,(? Id? id_ls)
                        ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                        ,stx_body) ctx) ξ) '∘ κ Θ Σ)
   #:when (eq? 'let-syntax (resolve id_ls Σ))
   (ζ (GenStx `(,id_ls
                 ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ stx_body ξ)) ctx)
       '∘ κ Θ Σ)
   ex-ξ-ls]

  [(ζ (GenStx `(,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id) ,stx_rhs) ctx_0)) ctx_1)
                 ,(Stxξ stx_body ξ)) ctx) '∘ κ0 Θ Σ)
   #:when (eq? 'let-syntax (resolve id_ls Σ))
   (let*-values ([(nam_new Σ_1) (alloc-name id Σ)]
                 [(scp_new Σ_2) (alloc-scope 'ls Σ_1)]
                 [(id_new) (add id scp_new)]
                 [(Σ_3) (bind Σ_2 id_new nam_new)]
                 [(𝓁_new Θ_1) (push-κ Θ κ0)]
                 [(stx_body2) (add stx_body scp_new)])
     (ζ (Stxξ stx_rhs (init-ξ))
         '∘
         (κ (GenStx `(,id-kont
                       ,id_ls
                       ,(GenStx `(,(GenStx `(,id_new ,(Hole)) ctx_0)) ctx_1)
                       ,(Stxξ stx_body2 ξ)) ctx) '∘ 𝓁_new)
         Θ_1 Σ_3))
   ex-ls-push-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ stx_body2 ξ)) ctx) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont     (resolve id_kont Σ))
               (eq? 'let-syntax (resolve id_ls Σ)))
   (let ([nam_new (resolve id_new Σ)])
     (InEval `(,(AstEnv (parse stx_exp Σ) (init-env)) • ,(init-store))
             (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                           ,(Stxξ stx_body2 ξ)) (empty-ctx))
                 '∘ κ Θ Σ)))
   ex-ls-eval]

  [(InEval `(,(? Val? val) • ,_)
           (ζ (GenStx `(,(GenStx (Sym nam_new) _)
                         ,(Stxξ stx_body2 ξ)) _) '∘ κ Θ Σ))
   (let ([ξ_new (extend-ξ ξ nam_new val)])
     (ζ (Stxξ stx_body2 ξ_new) '∘ κ Θ Σ))
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ)
       '∘ κ Θ Σ)
   #:when (Val? (lookup-ξ ξ (resolve id_mac Σ)))
   (let*-values ([(val) (lookup-ξ ξ (resolve id_mac Σ))]
                 [(scp_u Σ_1) (alloc-scope 'u Σ)]
                 [(scp_i Σ_2) (alloc-scope 'i Σ_1)])
     (InEval
      `(,(AstEnv (App val
                      (list (flip (add stx_macapp scp_u) scp_i))) (init-env))
        • ,(init-store))
      (ζ (Stxξ (GenStx #f (set scp_i)) ξ) '∘ κ Θ Σ_2)))
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0)
           (ζ (Stxξ (GenStx #f scps) ξ) '∘ κ Θ Σ))
   (let ([scp_i (car (set->list scps))])
     (ζ (Stxξ (flip stx_exp scp_i) ξ) '∘ κ Θ Σ))
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ (GenStx `(,(? Id? id_if) ,stl_exps ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (eq? 'if (resolve id_if Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ)
         '∘
         (κ (GenStx `(,id-kont ,id_if ,(Hole)) ctx) '∘ 𝓁_new)
         Θ_1 Σ))
   ex-if]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_if)
                 ,(GenStx (? ProperStl? val_exps) ctx)) _) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont (resolve id_kont Σ))
               (eq? 'if     (resolve id_if Σ)))
   (ζ (GenStx `(,id_if ,@val_exps) ctx) '• κ Θ Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ (GenStx `(,(? Id? id_app)
                        ,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (eq? '#%app (resolve id_app Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ (GenStx (cons (? Id? id_app)
                            (GenStx `(,stx_fun ,stl_args ...) _)) ctx) ξ)
       '∘ κ0 Θ Σ)
   #:when (eq? '#%app (resolve id_app Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-#%app2]

  ;; application
  [(ζ (Stxξ (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (or (not (Id? stx_fun))
              (let* ([name (resolve stx_fun Σ)]
                     [at (lookup-ξ ξ name)])
                (or (TVar? at)
                    (and (eq? 'not-found at)
                         (not (member name
                                      '(lambda let quote syntax let-syntax if
                                         #%app #%kont #%seq #%ls-kont
                                         #%snoc)))))))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-app]

  ;; reference
  [(ζ (Stxξ (and id (GenStx (Sym nam) ctx)) ξ) '∘ κ Θ Σ)
   (let ([all-transform (lookup-ξ ξ (resolve id Σ))])
     (match all-transform
       [(TVar id_new) (ζ id_new '• κ Θ Σ)]
       [_ (error '==>c "unbound identifier: ~a" nam)]))
   ex-var]
  
  ;; literal
  [(ζ (Stxξ (GenStx (? Atom? atom) ctx) ξ) '∘ κ Θ Σ)
   #:when (not (Id? (GenStx atom ctx)))
   (ζ (GenStx `(,(GenStx (Sym 'quote) ctx) ,(GenStx atom ctx)) ctx) '• κ Θ Σ)
   ex-lit]

  ;; pop κ
  [(ζ stx '• (κ stx_c ex? 𝓁) Θ Σ)
   (let ([κ0 (lookup-κ Θ 𝓁)])
     (ζ (in-hole stx_c stx) ex? κ0 Θ Σ))
   ex-pop-κ]

  ;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ (GenStx `(,(? Id? id_seq)
                        ,(GenStx (? ProperStl? val_dones) _)
                        ,stx_exp0 ,stl_exps ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (eq? '#%seq (resolve id_seq Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ0)])
     (ζ (Stxξ stx_exp0 ξ) '∘
         (κ
          (GenStx
           `(,(Stxξ id_seq ξ)
             ,(GenStx `(,id-snoc ,(GenStx val_dones (empty-ctx)) ,(Hole))
                      (empty-ctx))
             ,@stl_exps) ctx) '∘ 𝓁_new)
         Θ_1 Σ))
   ex-seq-cons]

  [(ζ (GenStx `(,(Stxξ (? Id? id_seq) ξ)
                 ,(GenStx `(,(? Id? id_snoc)
                            ,(GenStx (? ProperStl? val_dones) ctx_1)
                            ,(? Stx? stx_done)) _)
                 ,stl_exps ...) ctx) '∘ κ Θ Σ)
   #:when (and (eq? '#%seq  (resolve id_seq Σ))
               (eq? '#%snoc (resolve id_snoc Σ)))
   (let ([val_dones2 (snoc val_dones stx_done)])
     (ζ (Stxξ (GenStx `(,id_seq ,(GenStx val_dones2 ctx_1)
                                  ,@stl_exps) ctx) ξ)
         '∘ κ Θ Σ))
   ex-seq-snoc]
  
  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ (GenStx `(,(? Id? id_seq)
                        ,(GenStx (? ProperStl? val_dones) _)) ctx) ξ) '∘ κ Θ Σ)
   #:when (eq? '#%seq (resolve id_seq Σ))
   (ζ (GenStx val_dones ctx) '• κ Θ Σ)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with (-->c s1)
   (λ (s2) (InEval s2 ζ0))
   ex-in-eval])

(define ==>c ((reducer-of ==>c/Σ)
              bind resolve alloc-name alloc-scope regist-vars parse -->c))

;(: expand : Stx ξ Σ -> (Values Stx Σ))
(define ((expand/==> ==>) stx ξ Σ)
  (let ([init-ζ (ζ (Stxξ stx ξ) '∘ '• (init-Θ) Σ)])
    (match-let ([(list (ζ stx_new '• '• Θ_new Σ_new))
                 (apply-reduction-relation* ==> init-ζ)])
      (values stx_new Σ_new))))

(define expand (expand/==> ==>c))
