#lang racket
(require "../set.rkt" "../dprint.rkt" "../reduction.rkt"
         "struct.rkt"
         (only-in "syntax.rkt"
                  empty-ctx snoc zip unzip in-hole
                  add flip bind resolve id=?)
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
(define ((regist-vars/bind/alloc-name bind alloc-name) scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) stx) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1)
                    ((regist-vars/bind/alloc-name bind alloc-name) scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (add id scp)]
                   [(Σ_3) (bind Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))
(define regist-vars (regist-vars/bind/alloc-name bind alloc-name))

(define id-kont (GenStx (Sym '#%kont) (empty-ctx)))
(define id-seq (GenStx (Sym '#%seq)  (empty-ctx)))
(define id-snoc (GenStx (Sym '#%snoc) (empty-ctx)))
(define stx-nil (GenStx '() (empty-ctx)))

;; (: ==>c : ζ -> (Setof ζ))
(define-parameterized-reduction-relation (==>c/Σ :=<1>)

  ;; lambda
  [(ζ (Stxξ (GenStx `(,(? Id? id_lam)
                      ,(GenStx (? ProperStl? stl_args) ctx_0)
                      ,stx_body) ctx)
            ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_lam 'lambda Σ)
   #:with         (values scp_new Σ_1) := (alloc-scope 'lam Σ)
   #:with (values stl_args2 ξ_new Σ_2) := (regist-vars scp_new stl_args ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (add stx_body scp_new) ξ_new)
      '∘
      (κ (GenStx `(,id_lam
                   ,(GenStx stl_args2 ctx_0)
                   ,(Hole)) ctx) '• 𝓁_new)
      Θ_1 Σ_2)
   ex-lam-body]

  ;; let
  [(ζ (Stxξ (GenStx `(,(? Id? id_let)
                      ,(GenStx (? ProperStl? stl_binds) ctx_1)
                      ,stx_body) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_let 'let Σ)
   #:with    (values stl_vars stl_rhs) := (unzip stl_binds)
   #:with         (values scp_new Σ_1) := (alloc-scope 'let Σ)
   #:with (values stl_vars2 ξ_new Σ_2) := (regist-vars scp_new stl_vars ξ Σ_1)
   #:with           (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (add stx_body scp_new) ξ_new)
      '∘
      (κ (GenStx `(,id-kont
                   ,id_let
                   ,(Stxξ (GenStx `(,(GenStx stl_vars2 ctx_1)
                                    ,(GenStx stl_rhs ctx_1)
                                    ) ctx_1) ξ)
                   ,(Hole)) ctx) '∘ 𝓁_new)
      Θ_1 Σ_2)
   ex-let-body]

  [(ζ (GenStx `(,(? Id? id_kont)
                ,(? Id? id_let)
                ,(Stxξ (GenStx
                        `(,(GenStx (? ProperStl? stl_vars) _)
                          ,(GenStx (? ProperStl? stl_rhs) _)) ctx_1) ξ)
                ,stx_body) ctx) '∘ κ0 Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_let  'let    Σ))
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ)
      '∘
      (κ (GenStx `(,id_kont
                   ,id_let
                   ,(GenStx `(,(GenStx stl_vars ctx_1) ,(Hole)) ctx_1)
                   ,stx_body) ctx) '∘ 𝓁_new)
      Θ_1 Σ)
   ex-let-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                ,(? Id? id_let)
                ,(GenStx `(,(GenStx (? ProperStl? stl_vars) _)
                           ,(GenStx (? ProperStl? val_rhs) _)) ctx_1)
                ,stx_body) ctx) '∘ κ Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_let  'let    Σ))
   (ζ (GenStx `(,id_let ,(GenStx (zip stl_vars val_rhs ctx_1) ctx_1)
                        ,stx_body) ctx) '• κ Θ Σ)
   ex-let-rhs2]

  ;; quote
  [(ζ (Stxξ (and stx (GenStx `(,(? Id? id_quote) ,_) _)) _) '∘ κ Θ Σ)
   #:when (id=? id_quote 'quote Σ)
   (ζ stx '• κ Θ Σ)
   ex-quote]

  ;; syntax
  [(ζ (Stxξ (and stx (GenStx `(,(? Id? id_syntax) ,_) _)) _) '∘ κ Θ Σ)
   #:when (id=? id_syntax 'syntax Σ)
   (ζ stx '• κ Θ Σ)
   ex-stx]

  ;; macro creation
  [(ζ (Stxξ (GenStx `(,(? Id? id_ls)
                      ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                      ,stx_body) ctx) ξ) '∘ κ Θ Σ)
   #:when (id=? id_ls 'let-syntax Σ)
   (ζ (GenStx `(,id_ls
                ,(GenStx `(,(GenStx `(,id ,stx_rhs) ctx_0)) ctx_1)
                ,(Stxξ stx_body ξ)) ctx) '∘ κ Θ Σ)
   ex-ξ-ls]

  [(ζ (GenStx `(,(? Id? id_ls)
                ,(GenStx `(,(GenStx `(,(? Id? id) ,stx_rhs) ctx_0)) ctx_1)
                ,(Stxξ stx_body ξ)) ctx) '∘ κ0 Θ Σ)
   #:when (id=? id_ls 'let-syntax Σ)
   #:with (values nam_new Σ_1) := (alloc-name id Σ)
   #:with (values scp_new Σ_2) := (alloc-scope 'ls Σ_1)
   #:with               id_new := (add id scp_new)
   #:with                  Σ_3 := (bind Σ_2 id_new nam_new)
   #:with   (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   #:with            stx_body2 := (add stx_body scp_new)
   (ζ (Stxξ stx_rhs (init-ξ))
      '∘
      (κ (GenStx `(,id-kont
                   ,id_ls
                   ,(GenStx `(,(GenStx `(,id_new ,(Hole)) ctx_0)) ctx_1)
                   ,(Stxξ stx_body2 ξ)) ctx) '∘ 𝓁_new) Θ_1 Σ_3)
   ex-ls-push-rhs]

  [(ζ (GenStx `(,(? Id? id_kont)
                 ,(? Id? id_ls)
                 ,(GenStx `(,(GenStx `(,(? Id? id_new) ,stx_exp) ctx_0)) ctx_1)
                 ,(Stxξ stx_body2 ξ)) ctx) '∘ κ Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_ls 'let-syntax Σ))
   #:with nam_new :=<1> (resolve id_new Σ)
   #:with ast_exp :=<1> (parse stx_exp Σ)
   (InEval `(,(AstEnv ast_exp (init-env)) • ,(init-store))
           (ζ (GenStx `(,(GenStx (Sym nam_new) (empty-ctx))
                         ,(Stxξ stx_body2 ξ)) (empty-ctx))
               '∘ κ Θ Σ))
   ex-ls-eval]

  [(InEval `(,(? Val? val) • ,_)
           (ζ (GenStx `(,(GenStx (Sym nam_new) _)
                        ,(Stxξ stx_body2 ξ)) _) '∘ κ Θ Σ))
   #:with ξ_new := (extend-ξ ξ nam_new val)
   (ζ (Stxξ stx_body2 ξ_new) '∘ κ Θ Σ)
   ex-ls-ξ]

  ;; macro invocation
  [(ζ (Stxξ (and stx_macapp (GenStx `(,(? Id? id_mac) ,_ ...) ctx)) ξ)
       '∘ κ Θ Σ)
   #:with            nam_mac :=<1> (resolve id_mac Σ)
   #:with                val :=    (lookup-ξ ξ nam_mac)
   #:when (Val? val)
   #:with (values scp_u Σ_1) :=    (alloc-scope 'u Σ)
   #:with (values scp_i Σ_2) :=    (alloc-scope 'i Σ_1)
   (InEval
    `(,(AstEnv (App val
                    (list (flip (add stx_macapp scp_u) scp_i))) (init-env))
      • ,(init-store))
    (ζ (Stxξ (GenStx #f (set scp_i)) ξ) '∘ κ Θ Σ_2))
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0)
           (ζ (Stxξ (GenStx #f scps) ξ) '∘ κ Θ Σ))
   #:with scp_i := (car (set->list scps))
   (ζ (Stxξ (flip stx_exp scp_i) ξ) '∘ κ Θ Σ)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ (GenStx `(,(? Id? id_if) ,stl_exps ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_if 'if Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ)
      '∘
      (κ (GenStx `(,id-kont ,id_if ,(Hole)) ctx) '∘ 𝓁_new)
      Θ_1 Σ)
   ex-if]

  [(ζ (GenStx `(,(? Id? id_kont)
                ,(? Id? id_if)
                ,(GenStx (? ProperStl? val_exps) ctx)) _) '∘ κ Θ Σ)
   #:when (and (id=? id_kont '#%kont Σ) (id=? id_if   'if     Σ))
   (ζ (GenStx `(,id_if ,@val_exps) ctx) '• κ Θ Σ)
   ex-if-kont]

  ;; application (non-canonical #%app version)
  [(ζ (Stxξ (GenStx `(,(? Id? id_app)
                      ,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
      '∘
      (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
      Θ_1 Σ)
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ (GenStx (cons (? Id? id_app)
                          (GenStx `(,stx_fun ,stl_args ...) _)) ctx) ξ)
      '∘ κ0 Θ Σ)
   #:when (id=? id_app '#%app Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
      '∘
      (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
      Θ_1 Σ)
   ex-#%app2]

  ;; application
  [(ζ (Stxξ (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (Id? stx_fun)
   #:with name :=<1> (resolve stx_fun Σ)
   #:with   at :=    (lookup-ξ ξ name)
   #:when (or (TVar? at)
              (and (eq? 'not-found at)
                   (not (member name
                                '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))
   #:with             id_app := (GenStx (Sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
       Θ_1 Σ)
   ex-app]

  ;; primitive application
  [(ζ (Stxξ (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (not (Id? stx_fun))
   #:with             id_app := (GenStx (Sym '#%app) ctx)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ) '∘
       (κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
       Θ_1 Σ)
   ex-prim-app]

  ;; reference
  [(ζ (Stxξ (and id (GenStx (Sym nam) ctx)) ξ) '∘ κ Θ Σ)
   #:with           nam :=<1> (resolve id Σ)
   #:with all-transform :=    (lookup-ξ ξ nam)
   (match all-transform
     [(TVar id_new) (ζ id_new '• κ Θ Σ)]
     [_ (error '==>c "unbound identifier: ~a" nam)])
   ex-var]
  
  ;; literal
  [(ζ (Stxξ (GenStx (? Atom? atom) ctx) ξ) '∘ κ Θ Σ)
   #:when (not (Id? (GenStx atom ctx)))
   (ζ (GenStx `(,(GenStx (Sym 'quote) ctx) ,(GenStx atom ctx)) ctx) '• κ Θ Σ)
   ex-lit]

  ;; pop κ
  [(ζ stx '• (κ stx_c ex? 𝓁) Θ Σ)
   #:with κ0 := (lookup-κ Θ 𝓁)
   (ζ (in-hole stx_c stx) ex? κ0 Θ Σ)
   ex-pop-κ]

  ;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ (GenStx `(,(? Id? id_seq)
                      ,(GenStx (? ProperStl? val_dones) _)
                      ,stx_exp0 ,stl_exps ...) ctx) ξ) '∘ κ0 Θ Σ)
   #:when (id=? id_seq '#%seq Σ)
   #:with (values 𝓁_new Θ_1) := (push-κ Θ κ0)
   (ζ (Stxξ stx_exp0 ξ) '∘
      (κ
       (GenStx
        `(,(Stxξ id_seq ξ)
          ,(GenStx `(,id-snoc ,(GenStx val_dones (empty-ctx)) ,(Hole))
                   (empty-ctx))
          ,@stl_exps) ctx) '∘ 𝓁_new)
      Θ_1 Σ)
   ex-seq-cons]

  [(ζ (GenStx `(,(Stxξ (? Id? id_seq) ξ)
                ,(GenStx `(,(? Id? id_snoc)
                           ,(GenStx (? ProperStl? val_dones) ctx_1)
                           ,(? Stx? stx_done)) _)
                ,stl_exps ...) ctx) '∘ κ Θ Σ)
   #:when (and (id=? id_seq  '#%seq  Σ) (id=? id_snoc '#%snoc Σ))
   #:with val_dones2 := (snoc val_dones stx_done)
   (ζ (Stxξ (GenStx `(,id_seq ,(GenStx val_dones2 ctx_1)
                              ,@stl_exps) ctx) ξ) '∘ κ Θ Σ)
   ex-seq-snoc]
  
  ;; (#%seq (done ...)) --> (done ...)
  [(ζ (Stxξ (GenStx `(,(? Id? id_seq)
                      ,(GenStx (? ProperStl? val_dones) _)) ctx) ξ) '∘ κ Θ Σ)
   #:when (id=? id_seq '#%seq Σ)
   (ζ (GenStx val_dones ctx) '• κ Θ Σ)
   ex-seq-nil]

  ;; in-eval
  [(InEval s1 ζ0)
   #:with s2 <- (lift (-->c s1))
   (InEval s2 ζ0)
   ex-in-eval])

(define ==>c ((reducer-of ==>c/Σ) :=))

;(: expand : Stx ξ Σ -> (Cons Stx Σ))
(define ((expand/==> ==>) stx ξ Σ)
  (let ([init-ζ (ζ (Stxξ stx ξ) '∘ '• (init-Θ) Σ)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ_new))
                 (apply-reduction-relation* ==> init-ζ)])
      (cons stx_new Σ_new))))

(define expand (expand/==> ==>c))
