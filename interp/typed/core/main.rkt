#lang typed/racket
(require "../reduction.rkt"
         "types.rkt"
         "misc.rkt"
         (only-in "../example.rkt" core:examples)
         (for-syntax racket))

;; ----------------------------------------
;; Implementation of primitives:

(include "../delta.rktl")

;; ----------------------------------------
;; Evaluating AST:

(include "eval.rktl")

;; ----------------------------------------
;; Parsing:

(include "parsing.rktl")

;; ----------------------------------------
;; Syntax-object operations:

;; Simply pushes scopes down through a syntax object
(: add : Stx Scp -> Stx)
(define (add stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (cast (add stx scp) Stx)
                   (add-stl stl scp))
             (set-add ctx scp))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (set-add ctx scp))]))

(: add-stl : Stl Scp -> Stl)
(define (add-stl stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (cast (add stx scp) Stx) (add-stl stl scp))
             (set-add ctx scp))]
    [(GenStx (? Atom? atom) ctx) (GenStx atom (set-add ctx scp))]
    [(cons stx stl) (cons (cast (add stx scp) Stx)
                          (add-stl stl scp))]))

;; Pushes flipping a scope down through a syntax object
(: flip : Stx Scp -> Stx)
(define (flip stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip stx scp) (flip-stl stl scp))
             (addremove scp ctx))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (addremove scp ctx))]))

(: flip-stl : Stl Scp -> Stl)
(define (flip-stl stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip stx scp) (flip-stl stl scp))
             (addremove scp ctx))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (addremove scp ctx))]
    [(cons stx stl) (cons (flip stx scp) (flip-stl stl scp))]))

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
(: bind : Σ Id Nam -> Σ)
(define (bind Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(GenStx (Sym nam_1) ctx_1) id])
    (Σ size (hash-update tbl nam_1
                          (λ ([sbs : (U (Setof StoBind) Val ξ)])
                            (set-add (cast sbs (Setof StoBind))
                                     (StoBind ctx_1 nam)))
                          (λ () (ann (set) (Setof StoBind)))))))

(: resolve : Id Σ -> Nam)
(define (resolve id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbs (cast (lookup-Σ Σ0 nam) (Setof StoBind))]
           [scpss (map (λ ([sb : StoBind]) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset ctx scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))


;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

(: parse : Stx Σ -> Ast)
(define (parse stx Σ)
  (define (id=? [nam : Nam]) : (-> Id Boolean)
    (λ (id) (eq? (resolve id Σ) nam)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'lambda)))
               ,(GenStx stl_ids _) ,stx_body) _)
     (Fun (map (λ ([id : Id]) (Var (resolve id Σ)))
               (cast stl_ids (Listof Id)))
          (parse stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'let)))
               ,(GenStx (? ProperStl?  stl_binds) _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (App (Fun (map (λ ([id : Id]) (Var (resolve id Σ)))
                      (cast stl_ids (Listof Id)))
                 (parse stx_body Σ))
            (map (λ ([stx_rhs : Stx]) (parse stx_rhs Σ))
                 (cast stl_rhs (Listof Stx)))))]
    ; (quote stx)
    [(GenStx `(,(? Id? (? (id=? 'quote))) ,stx) _)
     (strip stx)]
    ; (syntax stx)
    [(GenStx `(,(? Id? (? (id=? 'syntax))) ,stx) _)
     stx]
    ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
    [(GenStx (cons (? Id? (? (id=? '#%app)))
                   (GenStx (cons stx_fun stl_args) _)) _)
     (App (parse stx_fun Σ) (parse* stl_args Σ))]
    ; (if stx stx stx)
    [(GenStx `(,(? Id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
     (If (parse stx_test Σ) (parse stx_then Σ) (parse stx_else Σ))]
    ; reference
    [(? Id? id) (Var (resolve id Σ))]
    ; literal
    [(GenStx (? Atom? atom) _) atom]))

(: parse* : Stl Σ -> (Listof Ast))
(define (parse* stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons (parse stx Σ) (parse* stl Σ))]
    [stx (list (parse (cast stx Stx) Σ))]))

;; ----------------------------------------
;; The expander:

(define (empty-ctx) : Ctx (ann (set) Ctx))

(include "expand.rktl")

(: regist-vars : Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define (regist-vars scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) (cast stx Id)) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (cast (add id scp) Id)]
                   [(Σ_3) (bind Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))


;(: ==>c : (-> ζ (Setof ζ)))
(define-reduction-relation ==>c ζ State

  ;; lambda
  [(ζ (Stxξ (GenStx `(,(? Id? id_lam)
                        ,(GenStx (? ProperStl? stl_args) ctx_0)
                        ,stx_body) ctx)
              ξ) '∘ κ Θ Σ)
   #:when (eq? 'lambda (resolve id_lam Σ))
   (let*-values ([(scp_new Σ_1) (alloc-scope Σ)]
                 [(stl_args2 ξ_new Σ_2)
                  (regist-vars scp_new stl_args ξ Σ_1)]
                 [(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ (add stx_body scp_new) ξ_new)
         '∘
         (Mk-κ (GenStx `(,id_lam
                          ,(GenStx stl_args2 ctx_0)
                          ,(Hole)) ctx)
                '• 𝓁_new)
         Θ_1 Σ_2))
   ex-lam-body]

  ;; let
  [(ζ (Stxξ (GenStx `(,(? Id? id_let)
                        ,(GenStx (? ProperStl? stl_binds) ctx_1)
                        ,stx_body) ctx) ξ) '∘ κ Θ Σ)
   #:when (eq? 'let (resolve id_let Σ))
   (let*-values ([(stl_vars stl_rhs) (unzip stl_binds)]
                 [(scp_new Σ_1) (alloc-scope Σ)]
                 [(stl_vars2 ξ_new Σ_2) (regist-vars scp_new stl_vars ξ Σ_1)]
                 [(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ (add stx_body scp_new) ξ_new)
         '∘
         (Mk-κ (GenStx `(,id-kont
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
                 ,stx_body) ctx) '∘ κ Θ Σ)
   #:when (and (eq? '#%kont (resolve id_kont Σ))
               (eq? 'let (resolve id_let Σ)))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ)
         '∘
         (Mk-κ (GenStx `(,id_kont
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
                 ,(Stxξ stx_body ξ)) ctx) '∘ κ Θ Σ)
   #:when (eq? 'let-syntax (resolve id_ls Σ))
   (let*-values ([(nam_new Σ_1) (alloc-name id Σ)]
                 [(scp_new Σ_2) (alloc-scope Σ_1)]
                 [(id_new) (cast (add id scp_new) Id)]
                 [(Σ_3) (bind Σ_2 id_new nam_new)]
                 [(𝓁_new Θ_1) (push-κ Θ κ)]
                 [(stx_body2) (add stx_body scp_new)])
     (ζ (Stxξ stx_rhs (init-ξ))
         '∘
         (Mk-κ (GenStx `(,id-kont
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
                 [(scp_u Σ_1) (alloc-scope Σ)]
                 [(scp_i Σ_2) (alloc-scope Σ_1)])
     (InEval
      `(,(AstEnv (App (cast val Val)
                      (list (flip (add stx_macapp scp_u) scp_i))) (init-env))
        • ,(init-store))
      (ζ (Stxξ (GenStx #f (set scp_i)) ξ) '∘ κ Θ Σ_2)))
   ex-macapp-eval]

  [(InEval `(,(? Stx? stx_exp) • ,store_0)
           (ζ (Stxξ (GenStx #f scps) ξ) '∘ κ Θ Σ))
   (ζ (Stxξ (flip stx_exp (car (set->list scps))) ξ) '∘ κ Θ Σ)
   ex-macapp-flip]

  ;; if
  [(ζ (Stxξ (GenStx `(,(? Id? id_if) ,stl_exps ...) ctx) ξ) '∘ κ Θ Σ)
   #:when (eq? 'if (resolve id_if Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ)
         '∘
         (Mk-κ (GenStx `(,id-kont ,id_if ,(Hole)) ctx) '∘ 𝓁_new)
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
                        ,stx_fun ,stl_args ...) ctx) ξ) '∘ κ Θ Σ)
   #:when (eq? '#%app (resolve id_app Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (Mk-κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-#%app]

  ;; application (canonical #%app version)
  [(ζ (Stxξ (GenStx (cons (? Id? id_app)
                            (GenStx `(,stx_fun ,stl_args ...) _)) ctx) ξ)
       '∘ κ Θ Σ)
   #:when (eq? '#%app (resolve id_app Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (Mk-κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
         Θ_1 Σ))
   ex-#%app2]

  ;; application
  [(ζ (Stxξ (GenStx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ Θ Σ)
   #:when (or (not (Id? stx_fun))
              (let ([name (resolve stx_fun Σ)])
                (and (eq? 'not-found (lookup-ξ ξ name))
                     (not (member name
                                  '(lambda let quote syntax let-syntax if
                                     #%app #%kont #%seq #%ls-kont #%snoc))))))
   (let-values ([(id_app) (GenStx (Sym '#%app) ctx)]
                [(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ (GenStx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
         '∘
         (Mk-κ (GenStx (cons id_app (Hole)) ctx) '• 𝓁_new)
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
  [(ζ stx '• (Mk-κ stx_c ex? 𝓁) Θ Σ)
   (let ([κ (lookup-κ Θ 𝓁)])
     (ζ (in-hole stx_c stx) ex? κ Θ Σ))
   ex-pop-κ]

  ;; expression sequence

  ;; (#%seq (done ...) exp0 exp ...) -->
  ;;   (#%seq (done ... (expand exp0)) exp ...)
  [(ζ (Stxξ (GenStx `(,(? Id? id_seq)
                        ,(GenStx (? ProperStl? val_dones) _)
                        ,stx_exp0 ,stl_exps ...) ctx) ξ) '∘ κ Θ Σ)
   #:when (eq? '#%seq (resolve id_seq Σ))
   (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
     (ζ (Stxξ stx_exp0 ξ) '∘
         (Mk-κ
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
   #:with ((reducer-of -->c) s1)
   (λ ([s2 : State]) (InEval s2 ζ0))
   ex-in-eval])


(: expand : Stx ξ Σ -> (Values Stx Σ))
(define (expand stx ξ Σ)
  (let ([init-ζ (ζ (Stxξ stx ξ) '∘ '• (init-Θ) Σ)])
    (match-let ([(list (ζ stx_new '• '• Θ_new Σ_new))
                 (apply-reduction-relation* (reducer-of ==>c) init-ζ)])
      (values stx_new Σ_new))))

;; for debug

(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  ((reducer-of ==>c)
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))))

(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Listof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   (reducer-of ==>c)
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))


;; ----------------------------------------
;; Drivers

(include "drivers.rktl")

(: expander : Stx -> (Values Stx Σ))
(define (expander stx)
  (expand stx (init-ξ) (init-Σ)))

(define-runner run
  reader
  expander
  stripper printer
  eval
  parse)

(define (main [mode : Symbol 'check])
  (run-examples run core:examples mode))
