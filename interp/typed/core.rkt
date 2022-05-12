#lang typed/racket
(require "types.rkt"
         "common.rkt"
         "reduction.rkt"
         (for-syntax racket))


;; ----------------------------------------
;; Implementation of primitives:

(define (plus . [ns : Real *]) : Real (apply + ns))
(define (minus [n : Real] . [ns : Real *]) : Real (apply - n ns))
(define (times . [ns : Real *]) : Real (apply * ns))
(define (div [n : Real] . [ns : Real *]) : Real (apply / n ns))
(define (less-than [n1 : Real] [n2 : Real] . [ns : Real *])
  (apply < n1 n2 ns))
(define (num-eq [n1 : Real] [n2 : Real] . [ns : Real *]) : Boolean
  (apply = n1 n2 ns))
(define (sym-eq [sym1 : Sym] [sym2 : Sym]) : Boolean
  (match* (sym1 sym2)
    [((Sym nam1) (Sym nam2)) (eq? nam1 nam2)]))

(: δ : Prim (Listof Val) -> Val)
(define (δ p vs)
  (match (cons p vs)
    [`(+ ,(? real? #{ns : (Listof Real)}) ...)
     (apply plus ns)]
    [`(- ,(? real? #{n : Real}) ,(? real? #{ns : (Listof Real)}) ...)
     (apply minus n ns)]
    [`(* ,(? real? #{ns : (Listof Real)}) ...)
     (apply times ns)]
    [`(/ ,(? real? #{n : Real}) ,(? real? #{ns : (Listof Real)}) ...)
     (apply div n ns)]
    [`(< ,(? real? #{n1 : Real}) ,(? real? #{n2 : Real})
         ,(? real? #{ns : (Listof Real)}) ...)
     (apply less-than n1 n2 ns)]
    [`(= ,(? real? #{n1 : Real}) ,(? real? #{n2 : Real})
         ,(? real? #{ns : (Listof Real)}) ...)
     (apply num-eq n1 n2 ns)]

    [`(eq? ,(? Sym? s1) ,(? Sym? s2)) (sym-eq s1 s2)]

    [`(cons ,v1 ,v2) (cons v1 v2)]
    [`(car ,(cons v1 _)) v1]
    [`(cdr ,(cons _ v2)) v2]

    [`(list) '()]
    [`(list ,v1 ,vs ...) (δ 'cons (list v1 (δ 'list vs)))]
    [`(second (,_ ,v2 ,_ ...)) v2]
    [`(third  (,_ ,_ ,v3 ,_ ...)) v3]
    [`(fourth (,_ ,_ ,_ ,v4 ,_ ...)) v4]

    [`(syntax-e ,(GenStx e _)) e]
    [`(datum->syntax ,_ ,(? Stx? stx)) stx]
    [`(datum->syntax ,(and stx0 (GenStx _ ctx_0)) (,v1 ,vs ...))
     (GenStx `(,(cast (δ 'datum->syntax `(,stx0 ,v1)) Stx)
               ,@(cast (δ 'syntax-e `(,(δ 'datum->syntax `(,stx0 ,vs))))
                       Stl))
             ctx_0)]
    [`(datum->syntax ,(GenStx _ ctx) ,(? Atom? atom))
     (GenStx atom ctx)]))

;; ----------------------------------------
;; Evaluating AST:

(: lookup-env : Env Var -> Loc)
(define (lookup-env env var) (hash-ref env var))

(: update-env : Env (Listof Var) (Listof Loc) -> Env)
(define (update-env env vars locs)
  (foldl (λ ([v : Var] [l : Loc] [e : Env]) (hash-set e v l))
         env vars locs))

(: lookup-store : Store Loc -> (U Val Cont))
(define (lookup-store store loc)
  (hash-ref (Store-tbl store) loc))

(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store store loc u)
  (Store (Store-size store)
         (hash-set (Store-tbl store) loc u)))

(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* store locs us)
  (Store (Store-size store)
         (foldl (λ ([l : Loc]
                     [u : (U Val Cont)]
                     [t : (HashTable Loc (U Val Cont))])
                  (hash-set t l u))
                (Store-tbl store) locs us)))

(: alloc-loc : Store -> (Values Loc Store))
(define (alloc-loc store)
  (let ([size (Store-size store)])
    (values (string->symbol (format "l~a" size))
            (Store (add1 size) (Store-tbl store)))))

;; for eval-time value binding
(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
(define (alloc-loc* nams store)
  (match nams
    ['() (values '() store)]
    [(list nam1 nams ...)
     (let* ([size (Store-size store)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (Store (add1 size) (Store-tbl store)))])
         (values (cons loc_0 locs_new) store_new)))]))

(: push-cont : Store Cont -> (Values Loc Store))
(define (push-cont store cont)
  (let-values ([(loc store_1) (alloc-loc store)])
    (let ([store_2 (update-store store_1 loc cont)])
      (values loc store_2))))

;(: -->c : (-> State (Setof State)))
(define-reduction-relation -->c State ζ
  ;; propagate env into subterms
  [`(,(AstEnv (If ast_test ast_then ast_else) env) ,cont ,store)
   `(,(SIf (AstEnv ast_test env)
           (AstEnv ast_then env)
           (AstEnv ast_else env)) ,cont ,store)
   ev-env-if]

  [`(,(AstEnv (App ast_fun ast_args) env) ,cont ,store)
   `(,(SApp '()
            (cons (AstEnv ast_fun env)
                  (map (λ ([arg : Ast]) (AstEnv arg env)) ast_args)))
     ,cont ,store)
   ev-env-app]

  ;; value
  [`(,(AstEnv (? Val? val) _) ,cont ,store)
   `(,val ,cont ,store)
   ev-val]

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cont ,store)
   `(,(AstEnv (cast (lookup-store store (lookup-env env var)) Val) env)
     ,cont ,store)
   ev-x]

  ;; lambda
  [`(,(AstEnv (Fun vars ast) env) ,cont ,store)
   `(,(AstEnv (VFun vars ast env) env) ,cont ,store)
   ev-lam]

  ;; application
  [`(,(SApp `(,vals ...) `(,clo ,clos ...)) ,cont ,store)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,clo ,(KApp vals  clos loc_new) ,store_1))
   ev-push-app]

  [`(,(? Val? val) ,(KApp vals clos loc_cont) ,store)
   `(,(SApp (append vals (list val)) clos)
     ,(cast (lookup-store store loc_cont) Cont) ,store)
   ev-pop-app]

  ;; β
  [`(,(SApp vals '()) ,cont ,store)
   #:when (and (pair? vals)
               (VFun? (car vals)))
   (let*-values ([(vars ast env vals) (let ([f(car vals)])
                                        (values (VFun-vars f)
                                                (VFun-ast f)
                                                (VFun-env f)
                                                (cdr vals)))]
                 [(nams) (map Var-nam vars)]
                 [(locs store_1) (alloc-loc* nams store)]
                 [(env_new) (update-env env vars locs)]
                 [(store_2) (update-store* store_1 locs vals)])
     `(,(AstEnv ast env_new) ,cont ,store_2))
   ev-β]

  ;; primitive application
  [`(,(SApp vals '()) ,cont ,store)
   #:when (and (pair? vals)
               (Prim? (car vals)))
   `(,(δ (car vals) (cdr vals)) ,cont ,store)
   ev-δ]

  ;; if
  [`(,(SIf (? (λ (x) (not (Val? x))) ser_test) clo_then clo_else) ,cont ,store)
   (let-values ([(loc_new store_1) (push-cont store cont)])
     `(,ser_test ,(KIf clo_then clo_else loc_new) ,store_1))
   ev-push-if]

  [`(,(? Val? val) ,(KIf clo_then clo_else loc_cont) ,store)
   `(,(SIf val clo_then clo_else)
     ,(cast (lookup-store store loc_cont) Cont) ,store)
   ev-pop-if]

  [`(,(SIf #f _ clo_else) ,cont ,store)
   `(,clo_else ,cont ,store)
   ev-if-#f]

  [`(,(SIf (? Val? val) clo_then _) ,cont ,store)
   #:when (not (equal? val #f))
   `(,clo_then ,cont ,store)
   ev-if-#t])

(: eval : Ast -> Val)
(define (eval ast)
  (match-let ([`((,(? Val? val) • ,_store))
               (apply-reduction-relation*
                (reducer-of -->c)
                `(,(AstEnv ast (init-env)) • ,(init-store)))])
    val))

;; for debug

(: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  ((reducer-of -->c)
   `(,(AstEnv (cast (run form 'parse) Ast) (init-env)) • ,(init-store))))

(: eval-->* : Sexp -> (Listof State))
(define (eval-->* form)
  (apply-reduction-relation*
   (reducer-of -->c)
   `(,(AstEnv (cast (run form 'parse) Ast) (init-env)) • ,(init-store))))


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

;; Adds or cancels a scope
(: addremove : Scp Scps -> Scps)
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

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

;; Recursively strips lexical context from a syntax object
(: strip : Stl -> Val)
(define (strip stl)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) _) (cons (strip stx) (strip stl))]
    [(GenStx (? Atom? atom) _) atom]
    [(cons stx stl) (cons (strip stx) (strip stl))]))

(: subtract : Scps Scps -> Scps)
(define (subtract scps1 scps2) (set-subtract scps1 scps2))

(: union : Scps Scps -> Scps)
(define (union scps1 scps2) (set-union scps1 scps2))

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

(: lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ))
(define (lookup-Σ Σ0 nam)
  (hash-ref (Σ-tbl Σ0) nam (λ () (ann (set) (Setof StoBind)))))

(: binding-lookup : (Setof StoBind) Scps -> (Option Nam))
(define (binding-lookup sbs scps)
  (let ([r (member scps (set->list sbs)
                   (λ ([scps : Scps] [sb : StoBind])
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))

(: biggest-subset : Scps (Listof Scps) -> Scps)
(define (biggest-subset scps_ref scpss)
  (let* ([matching : (Listof Scps)
                   (filter (λ ([scps_bind : Scps])
                             (subset? scps_bind scps_ref))
                           scpss)]
         [sorted : (Listof Scps)
                 ((inst sort Scps Index)
                  matching > #:key set-count)])
    ;; The binding is ambiguous if the first scps in
    ;; `sorted` is not bigger than the others, or if
    ;; some scps in `sorted` is not a subset of the
    ;; first one.
    (if (or (empty? sorted)
            (and (pair? (rest sorted))
                 (= (set-count (first sorted))
                    (set-count (second sorted))))
            (ormap (λ ([b : Scps]) (not (subset? b (first sorted))))
                   (rest sorted)))
        (set)
        (first sorted))))

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
;; Expand-time environment operations:

(: lookup-ξ : ξ Nam -> AllTransform)
(define (lookup-ξ ξ nam) (hash-ref ξ nam (λ () 'not-found)))

(: extend-ξ : ξ Nam AllTransform -> ξ)
(define (extend-ξ ξ nam all-transform) (hash-set ξ nam all-transform))

;; ----------------------------------------
;; Expand-time stack operations:

(: alloc-κ : Θ -> (Values 𝓁 Θ))
(define (alloc-κ θ)
  (match-let ([(Θ size tbl) θ])
    (values (𝓁 (string->symbol (format "k~a" size)))
            (Θ (add1 size) tbl))))

(: lookup-κ : Θ 𝓁 -> κ)
(define (lookup-κ θ 𝓁) (hash-ref (Θ-tbl θ) 𝓁))

(: update-κ : Θ 𝓁 κ -> Θ)
(define (update-κ θ 𝓁 κ)
  (match-let ([(Θ size tbl) θ])
    (Θ size (hash-set tbl 𝓁 κ))))

(: push-κ : Θ κ -> (Values 𝓁 Θ))
(define (push-κ θ κ)
  (let-values ([(𝓁 θ_1) (alloc-κ θ)])
    (values 𝓁 (update-κ θ_1 𝓁 κ))))

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

(: alloc-name : Id Σ -> (Values Nam Σ))
(define (alloc-name id Σ0)
  (match-let ([(GenStx (Sym nam) _) id]
              [(Σ size tbl) Σ0])
    (values (string->symbol (format "~a:~a" nam size))
            (Σ (add1 size) tbl))))

(: alloc-scope : Σ -> (Values Scp Σ))
(define (alloc-scope Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (string->symbol (format "scp:~a" size))
            (Σ (add1 size) tbl))))

;(: regist-vars : Scp Stl ξ Σ -> (Values Stl ξ Σ))
(: regist-vars : Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define (regist-vars  scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) (cast stx Id)) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (cast (add id scp) Id)]
                   [(Σ_3) (bind Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (TVar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))

;; ----------------------------------------
;; The expander:

(define (empty-ctx) : Ctx (ann (set) Ctx))

(define id-kont : Id (GenStx (Sym '#%kont) (empty-ctx)))
(define id-seq  : Id (GenStx (Sym '#%seq)  (empty-ctx)))
(define id-snoc : Id (GenStx (Sym '#%snoc) (empty-ctx)))
(define stx-nil (GenStx '() (empty-ctx)))

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


(: init-env : -> Env)
(define (init-env) (make-immutable-hash))

(: init-store : -> Store)
(define (init-store) (Store 0 (make-immutable-hash)))

(: init-ξ : -> ξ)
(define (init-ξ) (make-immutable-hash))

(: init-Σ : -> Σ)
(define (init-Σ) (Σ 0 (make-immutable-hash)))

(: init-Θ : -> Θ)
(define (init-Θ) (Θ 0 (make-immutable-hash)))

(define-helpers (empty-ctx) reader printer)

(: stripper : Stx Σ -> Val)
(define (stripper stx Σ) (strip stx))

(: expander : Stx -> (Values Stx Σ))
(define (expander stx)
  (expand stx (init-ξ) (init-Σ)))

(define-runner run
  reader
  expander
  stripper printer
  eval
  parse)


;; ----------------------------------------
;; Examples:

(define ex-<
  '[<
    (< 3 5)])

(define ex-eq?
  '[eq?
    (eq? 'a 'a)])

(define ex-let
  '[let-x
    (let ([x 1]) (+ x 2))])

(define ex-if-#t
  '[if-#t
    (if (< 0 1) 'foo 'bar)])

(define ex-if-#f
  '[if-#f
    (let ([x 3] [y 2])
      (if (< x y) (+ x y) (* x y)))])

(define ex-simple
  '[simple
    (let-syntax ([x (lambda (stx) #'2)])
      (x 1))])
(define (raw-simple)
  (let-syntax ([x (lambda (stx) #'2)])
    (x 1)))

(define ex-reftrans
  '[reftrans
    (let ([z 1])
      ((let-syntax ([x (lambda (stx) #'z)])
         (lambda (z) (x))) 2))])
(define (raw-reftrans)
  (lambda (z)
    (let-syntax ([x (lambda (stx) #'z)])
      (lambda (z) (x)))))

(define ex-hyg
  '[hyg
    (let ([z 1])
      ((let-syntax
           ([x (lambda (stx)
                 (#%app datum->syntax
                        #'here
                        (#%app list #'lambda
                               (#%app datum->syntax #'here (#%app list #'z))
                               (#%app second (#%app syntax-e stx)))))])
         (x z)) 2))])
(define (raw-hyg)
  (lambda (z)
    (let-syntax ([x (lambda (stx)
                      #`(lambda (z) #,(second (syntax-e stx))))])
      (x z))))


(define ex-thunk
  '[thunk
    (let-syntax
        ([thunk (lambda (stx)
                  (#%app datum->syntax
                         stx
                         (#%app list #'lambda
                                (#%app datum->syntax stx (#%app list #'a)) 
                                (#%app second (#%app syntax-e stx)) ;; #'(+ a 1)
                                )))])
      ((let ([a 5])
         (thunk (+ a 1))) 0))])
(define (raw-thunk)
  (let-syntax ([thunk (lambda (stx)
                        #`(lambda (a)
                            #,(second (syntax-e stx)) ;; #'(+ a 1)
                            ))])
    (((lambda (a) (thunk (+ a 1))) 5) 0)))


(define ex-get-identity
  '[get-identity
    (let-syntax
        ([get-identity
          (lambda (stx)
            (#%app datum->syntax
                   stx
                   (#%app list #'lambda
                          (#%app datum->syntax stx (#%app list #'a))
                          (#%app datum->syntax
                                 stx
                                 (#%app list #'lambda
                                        (#%app
                                         datum->syntax
                                         stx
                                         (#%app list
                                                (#%app
                                                 second
                                                 (#%app syntax-e stx)) ;; #'a
                                                ))
                                        #'a)))))])
      (((get-identity a) 1) 2))])
(define (raw-get-identity)
  (let-syntax ([get-identity (lambda (stx)
                               #`(lambda (a)
                                   (lambda (#,(second (syntax-e stx))) ;; #'a
                                     a)))])
    (get-identity a)))

(define core:examples
  (list ex-<
        ex-eq?
        ex-let
        ex-if-#t ex-if-#f
        ex-simple
        ex-reftrans
        ex-hyg
        ex-thunk
        ex-get-identity))

(define (main [mode : Symbol 'check])
  (run-examples run core:examples mode))
