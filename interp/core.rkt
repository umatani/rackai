#lang racket
(require racket/struct
         "common.rkt"
         (for-syntax racket/list))


;; ----------------------------------------
;; Implementation of primitives:

(define (plus . numbers) (apply + numbers))
(define (minus . numbers) (apply - numbers))
(define (times . numbers) (apply * numbers))
(define (div . numbers) (apply / numbers))
(define (less-than . numbers) (apply < numbers))
(define (num-eq . numbers) (apply = numbers))
(define (sym-eq sym1 sym2)
  (match* (sym1 sym2)
    [((Sym nam1) (Sym nam2)) (eq? nam1 nam2)]))

;; δ : prim (val ...) -> val
(define (δ prim vals)
  (match (cons prim vals)
    [`(+ ,numbers ...) (apply plus      numbers)]
    [`(- ,numbers ...) (apply minus     numbers)]
    [`(* ,numbers ...) (apply times     numbers)]
    [`(/ ,numbers ...) (apply div       numbers)]
    [`(< ,numbers ...) (apply less-than numbers)]
    [`(= ,numbers ...) (apply num-eq    numbers)]
    [`(eq? ,sym_1 ,sym_2) (sym-eq sym_1 sym_2)]

    [`(cons ,val_1 ,val_2) (cons val_1 val_2)]
    [`(car ,(cons val_1 _)) val_1]
    [`(cdr ,(cons _ val_2)) val_2]

    [`(list) '()]
    [`(list ,val_1 ,vals ...) (δ 'cons (list val_1 (δ 'list vals)))]
    [`(second (,_ ,val_2 ,_ ...)) val_2]
    [`(third  (,_ ,_ ,val_3 ,_ ...)) val_3]
    [`(fourth (,_ ,_ ,_ ,val_4 ,_ ...)) val_4]

    [`(syntax-e ,(Stx e _)) e]
    [`(datum->syntax ,_ ,(and stx (Stx _ _))) stx]
    [`(datum->syntax ,(and stx0 (Stx _ ctx_0)) (,val_1 ,vals ...))
     (Stx `(,(δ 'datum->syntax `(,stx0 ,val_1))
            ,@(δ 'syntax-e `(,(δ 'datum->syntax `(,stx0 ,vals)))))
           ctx_0)]
    [`(datum->syntax ,(Stx _ ctx) ,(? atom? atom))
     (Stx atom ctx)]))

;; ----------------------------------------
;; Evaluating AST:

;; lookup-env : env nam -> loc
(define (lookup-env env nam) (hash-ref env nam))

;; update-env : env (nam ...) (loc ...) -> env
(define (update-env env nams locs)
  (foldl (λ (n l e) (hash-set e n l)) env nams locs))

;; lookup-store : store loc -> u
(define (lookup-store store loc)
  (hash-ref (Heap-table store) loc))

;; update-store : store loc u -> store
(define (update-store store loc u)
  (Heap (Heap-size store)
        (hash-set (Heap-table store) loc u)))

;; update-store* : store (loc ...) (u ...) -> store
(define (update-store* store locs us)
  (Heap (Heap-size store)
        (foldl (λ (l u s) (hash-set s l u)) (Heap-table store) locs us)))

;; alloc-loc : store -> (values loc store)
(define (alloc-loc store)
  (let ([size (Heap-size store)])
    (values (string->symbol (format "l~a" size))
            (Heap (add1 size) (Heap-table store)))))

;; alloc-loc* : (nam ...) store -> (values (loc ...) store)
;;     for eval-time value binding
(define (alloc-loc* nams store)
  (match nams
    ['() (values '() store)]
    [(list nam1 nams ...)
     (let* ([size (Heap-size store)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (Heap (add1 size) (Heap-table store)))])
         (values (cons loc_0 locs_new) store_new)))]))

;; push-cont : store cont -> (values loc store)
(define (push-cont store cont)
  (let-values ([(loc store_1) (alloc-loc store)])
    (let ([store_2 (update-store store_1 loc cont)])
      (values loc store_2))))

(define -->c
  (reduction-relation
   #; template #;
   [`(,clo ,cont ,store)
    `(,clo ,cont ,store)]

   ;; propagate env into subterms
   [`(,(Clo `(If ,ast_test ,ast_then ,ast_else) env) ,cont ,store)
    `((If ,(Clo ast_test env)
          ,(Clo ast_then env)
          ,(Clo ast_else env)) ,cont ,store)]

   [`(,(Clo `(App ,ast_fun ,ast_args ...) env) ,cont ,store)
    `((App ,(Clo ast_fun env) ,@(map (λ (ast_arg) (Clo ast_arg env))
                                     ast_args))
      ,cont ,store)]

   ;; value
   [`(,(Clo (? val? val) env) ,cont ,store)
    `(,val ,cont ,store)]

   ;; reference
   [`(,(Clo `(Var ,nam) env) ,cont ,store)
    `(,(Clo (lookup-store store (lookup-env env nam)) env) ,cont ,store)]

   ;; lambda
   [`(,(Clo `(Fun (,vars ...) ,ast) env) ,cont ,store)
    `(,(Clo `(Fun (,@vars) ,ast ,env) env) ,cont ,store)]

   ;; application
   [`((App ,(? val? vals) ... ,(? ser? ser) ,clos ...) ,cont ,store)
    (let-values ([(loc_new store_1) (push-cont store cont)])
      `(,ser (App ,@vals ,hole ,@clos ,loc_new) ,store_1))]

   [`(,(? val? val)
      (App ,(? val? vals) ... ,(Hole) ,clos ... ,loc_cont)
      ,store)
    `((App ,@vals ,val ,@clos) ,(lookup-store store loc_cont) ,store)]

   ;; β
   [`((App (Fun ((Var ,nams) ...) ,ast ,env) ,(? val? vals) ...)
      ,cont ,store)
    (let*-values ([(locs store_1) (alloc-loc* nams store)]
                  [(env_new) (update-env env nams locs)]
                  [(store_2) (update-store* store_1 locs vals)])
      `(,(Clo ast env_new) ,cont ,store_2))]

   ;; primitive application
   [`((App ,(? prim? prim) ,(? val? vals) ...) ,cont ,store)
    `(,(δ prim vals) ,cont ,store)]

   ;; if
   [`((If ,(? ser? ser_test) ,clo_then ,clo_else) ,cont ,store)
    (let-values ([(loc_new store_1) (push-cont store cont)])
      `(,ser_test (If ,hole ,clo_then ,clo_else ,loc_new) ,store_1))]

   [`(,(? val? val) (If ,(Hole) ,clo_then ,clo_else ,loc_cont) ,store)
    `((If ,val ,clo_then ,clo_else) ,(lookup-store store loc_cont) ,store)]

   [`((If #f ,_ ,clo_else) ,cont ,store)
    `(,clo_else ,cont ,store)]

   [`((If ,(? val? val) ,clo_then ,_) ,cont ,store)
    #:when (not (equal? val #f))
    `(,clo_then ,cont ,store)]))

;; eval : ast -> val
(define (eval ast)
  (match-let ([`((,(? val? val) • ,_store))
               (apply-reduction-relation*
                -->c `(,(Clo ast (init-env)) • ,(init-store)))])
    val))

;; for debug

(define (eval--> form)
  (-->c `(,(Clo (run form 'parse) (init-env))
          • ,(init-store))))

(define (eval-->* form)
  (apply-reduction-relation* -->c `(,(Clo (run form 'parse) (init-env))
                                    • ,(init-store))))

;; ----------------------------------------
;; Syntax-object operations:

;; add : stl scp -> stl
;;     Simply pushes scopes down through a syntax object
(define (add stl scp)
  (match stl
    ['() '()]
    [(Stx (cons stx stl) ctx)
     (Stx (cons (add stx scp) (add stl scp)) (set-add ctx scp))]
    [(Stx (? atom? atom) ctx) (Stx atom (set-add ctx scp))]
    [(cons stx stl) (cons (add stx scp) (add stl scp))]))

;; addremove : scp scps -> scps
;;     Adds or cancels a scope
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

;; flip : stl scp -> stl
;;     Pushes flipping a scope down through a syntax object
(define (flip stl scp)
  (match stl
    ['() '()]
    [(Stx (cons stx stl) ctx)
     (Stx (cons (flip stx scp) (flip stl scp)) (addremove scp ctx))]
    [(Stx (? atom? atom) ctx) (Stx atom (addremove scp ctx))]
    [(cons stx stl) (cons (flip stx scp) (flip stl scp))]))

;; strip : (stl ∪ ζ) -> val
;;     Recursively strips lexical context from a syntax object
(define (strip stl)
  (match stl
    ['() '()]
    [(Stx (cons stx stl) _) (cons (strip stx) (strip stl))]
    [(Stx (? atom? atom) _) atom]
    [(cons stx stl) (cons (strip stx) (strip stl))]
    ;; for strip-ζ
    [(ζ stx∘ ex? (κ STX ex2? 𝓁) Θ Σ)
     (ζ (strip stx∘) ex? (mk-κ (strip STX) ex2? 𝓁) Θ Σ)]
    [any any]))

;; subtract : scps scps -> scps
(define (subtract scps1 scps2) (set-subtract scps1 scps2))

;; union : scps scps -> scps
(define (union scps1 scps2) (set-union scps1 scps2))

;; bind : Σ id nam -> Σ
;;     Add a binding using the name and scopes of an identifier, mapping
;;     them in the store to a given name
(define (bind Σ id nam)
  (match-let ([(Sto size binds) Σ]
              [(Stx (Sym nam_1) ctx_1) id])
    (Sto size (hash-update binds nam_1
                           (λ (sbs) (set-add sbs (StoBind ctx_1 nam)))
                           (set)))))


;; lookup-Σ : Σ nam -> (Set (StoBind scps nam) ...)
(define (lookup-Σ Σ nam)
  (hash-ref (Sto-binds Σ) nam (set)))

;; binding-lookup : (Set (StoBind scps nam) ...) scps -> nam ∪ #f
(define (binding-lookup sbs scps)
  (for/first ([sb (in-set sbs)]
              #:when (set=? (StoBind-scps sb) scps))
    (StoBind-nam sb)))

;; biggest-subset : scps (Set scps ...) -> scps
(define (biggest-subset scps_ref scpss)
  (let* ([matching
          (filter (λ (scps_bind)
                    (subset? scps_bind scps_ref))
                  (set->list scpss))]
         [sorted
          (sort matching > #:key set-count)])
    ;; The binding is ambiguous if the first scps in
    ;; `sorted` is not bigger than the others, or if
    ;; some scps in `sorted` is not a subset of the
    ;; first one.
    (if (or (empty? sorted)
            (and (pair? (rest sorted))
                 (= (set-count (first sorted))
                    (set-count (second sorted))))
            (ormap (λ (b) (not (subset? b (first sorted))))
                   (rest sorted)))
        (set)
        (first sorted))))

;; resolve : id Σ -> nam
(define (resolve id Σ)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ nam)]
           [scpss (for/set ([sb (in-set sbs)]) (StoBind-scps sb))]
           [scps_biggest (biggest-subset ctx scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))


;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):


;; parse : stx Σ -> ast
(define (parse stx Σ)
  (define (id=? nam) (λ (id) (eq? (resolve id Σ) nam)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(Stx `(,(? (id=? 'lambda))
            ,(Stx stl_ids _) ,stx_body) _)
     `(Fun ,(map (λ (id) `(Var ,(resolve id Σ))) stl_ids)
           ,(parse stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx `(,(? (id=? 'let))
            ,(Stx stl_binds _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       `(App (Fun ,(map (λ (id) `(Var ,(resolve id Σ))) stl_ids)
                  ,(parse stx_body Σ))
             ,@(map (λ (stx_rhs) (parse stx_rhs Σ)) stl_rhs)))]
    ; (quote stx)
    [(Stx `(,(? (id=? 'quote)) ,stx) _)
     (strip stx)]
    ; (syntax stx)
    [(Stx `(,(? (id=? 'syntax)) ,stx) _)
     stx]
    ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
    [(Stx (cons (? (id=? '#%app))
                (Stx (cons stx_fun stl_args) _)) _)
     `(App ,(parse stx_fun Σ) ,@(parse* stl_args Σ))]
    ; (if stx stx stx)
    [(Stx `(,(? (id=? 'if)) ,stx_test ,stx_then ,stx_else) _)
     `(If ,(parse stx_test Σ) ,(parse stx_then Σ) ,(parse stx_else Σ))]
    ; reference
    [(? id? id) `(Var ,(resolve id Σ))]
    ; literal
    [(Stx (? atom? atom) _) atom]))

;; parse* : stl Σ -> (ast ...)
(define (parse* stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons (parse stx Σ) (parse* stl Σ))]
    [stx (parse stx Σ)]))


;; ----------------------------------------
;; Expand-time environment operations:

;; lookup-ξ : ξ nam -> all-transform
(define (lookup-ξ ξ nam) (hash-ref ξ nam 'not-found))

;; extend-ξ : ξ nam all-transform -> ξ
(define (extend-ξ ξ nam all-transform) (hash-set ξ nam all-transform))

;; ----------------------------------------
;; Expand-time stack operations:

;; alloc-κ : Θ -> (values 𝓁 Θ)
(define (alloc-κ Θ)
  (match-let ([(Stk size frames) Θ])
    (values (𝓁 (string->symbol (format "k~a" size)))
            (Stk (add1 size) frames))))

;; lookup-κ : Θ 𝓁 -> κ
(define (lookup-κ Θ 𝓁) (hash-ref (Stk-frames Θ) 𝓁))

;; update-κ : Θ 𝓁 κ -> Θ
(define (update-κ Θ 𝓁 κ)
  (match-let ([(Stk size frames) Θ])
    (Stk size (hash-set frames 𝓁 κ))))

;; push-κ : Θ κ -> (values 𝓁 Θ)
(define (push-κ Θ κ)
  (let-values ([(𝓁 Θ_1) (alloc-κ Θ)])
    (values 𝓁 (update-κ Θ_1 𝓁 κ))))


;; ----------------------------------------
;; Alloc name & scope helpers for expander:

;; alloc-name : id Σ -> (values nam Σ)
(define (alloc-name id Σ)
  (match-let ([(Stx (Sym nam) _) id]
              [(Sto size binds) Σ])
    (values (string->symbol (format "~a:~a" nam size))
            (Sto (add1 size) binds))))

;; alloc-scope : Σ -> (values scp Σ)
(define (alloc-scope Σ)
  (match-let ([(Sto size binds) Σ])
    (values (string->symbol (format "scp:~a" size))
            (Sto (add1 size) binds))))

;; regist-vars : scp stl ξ Σ -> (values stl ξ Σ)
(define (regist-vars  scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons id stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (add id scp)]
                   [(Σ_3) (bind Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new `(TVar ,id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))

;; ----------------------------------------
;; The expander:

(define (empty-ctx) (set))

(define id-kont (Stx (Sym '#%kont) (empty-ctx)))
(define id-seq  (Stx (Sym '#%seq)  (empty-ctx)))
(define id-snoc (Stx (Sym '#%snoc) (empty-ctx)))
(define stx-nil (Stx '() (empty-ctx)))

;; ζ ::= (ζ stx∘ ex? κ Θ Σ) | (in-eval state ζ)
(struct ζ (stx∘ ex? κ Θ Σ)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'ζ)
      (lambda (obj) (list (ζ-stx∘ obj) (ζ-ex? obj) (ζ-κ obj)
                          (ζ-Θ obj) (ζ-Σ obj)))))])
(struct in-eval (state ζ)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'in-eval)
      (lambda (obj) (list (in-eval-state obj) (in-eval-ζ obj)))))])

;; ζ ==>c ζ
(define ==>c
  (reduction-relation
   #; template #;
   [(ζ (cons stx ξ) ex? κ Θ Σ)
    (ζ stx ex? κ Θ Σ)]

   ;; lambda
   [(ζ (cons (Stx `(,(? id? id_lam) ,(Stx stl_args ctx_0) ,stx_body) ctx)
              ξ) '∘ κ Θ Σ)
    #:when (eq? 'lambda (resolve id_lam Σ))
    (let*-values ([(scp_new Σ_1) (alloc-scope Σ)]
                  [(stl_args2 ξ_new Σ_2) (regist-vars scp_new stl_args ξ Σ_1)]
                  [(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons (add stx_body scp_new) ξ_new)
          '∘
          (mk-κ (Stx `(,id_lam
                        ,(Stx stl_args2 ctx_0)
                        ,hole) ctx)
                 '• 𝓁_new)
          Θ_1 Σ_2))]

   ;; let
   [(ζ (cons (Stx `(,(? id? id_let)
                     ,(Stx stl_binds ctx_1)
                     ,stx_body) ctx) ξ) '∘ κ Θ Σ)
    #:when (eq? 'let (resolve id_let Σ))
    (let*-values ([(stl_vars stl_rhs) (unzip stl_binds)]
                  [(scp_new Σ_1) (alloc-scope Σ)]
                  [(stl_vars2 ξ_new Σ_2) (regist-vars scp_new stl_vars ξ Σ_1)]
                  [(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons (add stx_body scp_new) ξ_new)
          '∘
          (mk-κ (Stx `(,id-kont
                        ,id_let
                        ,(Stx `(,(Stx stl_vars2 ctx_1)
                                ,(Stx stl_rhs ctx_1)
                                ,ξ) ctx_1)
                        ,hole) ctx) '∘ 𝓁_new)
          Θ_1 Σ_2))]

   [(ζ (Stx `(,(? id? id_kont)
               ,(? id? id_let)
               ,(Stx `(,(Stx stl_vars _)
                       ,(Stx stl_rhs _) ,ξ) ctx_1)
               ,stx_body) ctx) '∘ κ Θ Σ)
    #:when (and (eq? '#%kont (resolve id_kont Σ))
                (eq? 'let (resolve id_let Σ)))
    (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons (Stx `(,id-seq ,stx-nil ,@stl_rhs) ctx_1) ξ)
          '∘
          (mk-κ (Stx `(,id_kont
                        ,id_let
                        ,(Stx `(,(Stx stl_vars ctx_1) . ,hole) ctx_1)
                        ,stx_body) ctx) '∘ 𝓁_new)
          Θ_1 Σ))]

   [(ζ (Stx `(,(? id? id_kont)
               ,(? id? id_let)
               ,(Stx `(,(Stx stl_vars _) . ,(Stx val_rhs _)) ctx_1)
               ,stx_body) ctx) '∘ κ Θ Σ)
    #:when (and (eq? '#%kont (resolve id_kont Σ))
                (eq? 'let (resolve id_let Σ)))
    (ζ (Stx `(,id_let ,(Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                       ,stx_body) ctx)
        '• κ Θ Σ)]

   ;; quote
   [(ζ (cons (and stx (Stx `(,(? id? id_quote) ,_) _)) ξ) '∘ κ Θ Σ)
    #:when (eq? 'quote (resolve id_quote Σ))
    (ζ stx '• κ Θ Σ)]

   ;; syntax
   [(ζ (cons (and stx (Stx `(,(? id? id_syntax) ,_) _)) ξ) '∘ κ Θ Σ)
    #:when (eq? 'syntax (resolve id_syntax Σ))
    (ζ stx '• κ Θ Σ)]

   ;; macro creation
   [(ζ (cons (Stx `(,(? id? id_ls)
                     ,(Stx `(,(Stx `(,id ,stx_rhs) ctx_0)) ctx_1)
                     ,stx_body) ctx) ξ) '∘ κ Θ Σ)
    #:when (eq? 'let-syntax (resolve id_ls Σ))
    (ζ (Stx `(,id_ls
               ,(Stx `(,(Stx `(,id ,stx_rhs) ctx_0)) ctx_1)
               (,stx_body ,ξ)) ctx)
        '∘ κ Θ Σ)]

   [(ζ (Stx `(,(? id? id_ls)
               ,(Stx `(,(Stx `(,id ,stx_rhs) ctx_0)) ctx_1)
               (,stx_body ,ξ)) ctx) '∘ κ Θ Σ)
    #:when (eq? 'let-syntax (resolve id_ls Σ))
    (let*-values ([(nam_new Σ_1) (alloc-name id Σ)]
                  [(scp_new Σ_2) (alloc-scope Σ_1)]
                  [(id_new) (add id scp_new)]
                  [(Σ_3) (bind Σ_2 id_new nam_new)]
                  [(𝓁_new Θ_1) (push-κ Θ κ)]
                  [(stx_body2) (add stx_body scp_new)])
      (ζ (cons stx_rhs (init-ξ))
          '∘
          (mk-κ (Stx `(,id-kont
                        ,id_ls
                        ,(Stx `(,(Stx `(,id_new ,hole) ctx_0)) ctx_1)
                        (,stx_body2 ,ξ)) ctx) '∘ 𝓁_new)
          Θ_1 Σ_3))]

   [(ζ (Stx `(,(? id? id_kont)
               ,(? id? id_ls)
               ,(Stx `(,(Stx `(,id_new ,stx_exp) ctx_0)) ctx_1)
               (,stx_body2 ,ξ)) ctx) '∘ κ Θ Σ)
    #:when (and (eq? '#%kont     (resolve id_kont Σ))
                (eq? 'let-syntax (resolve id_ls Σ)))
    (let ([nam_new (resolve id_new Σ)])
      (in-eval `(,(Clo (parse stx_exp Σ) (init-env)) • ,(init-store))
               (ζ (Stx `(,(Stx (Sym nam_new) (empty-ctx))
                          (,stx_body2 ,ξ)) (empty-ctx))
                   '∘ κ Θ Σ)))]

   [(in-eval `(,(? val? val) • ,_)
             (ζ (Stx `(,(Stx (Sym nam_new) _) (,stx_body2 ,ξ)) _) '∘ κ Θ Σ))
    (let ([ξ_new (extend-ξ ξ nam_new val)])
      (ζ (cons stx_body2 ξ_new) '∘ κ Θ Σ))]

   ;; macro invocation
   [(ζ (cons (and stx_macapp (Stx `(,(? id? id_mac) ,_ ...) ctx)) ξ)
        '∘ κ Θ Σ)
    #:when (val? (lookup-ξ ξ (resolve id_mac Σ)))
    (let*-values ([(val) (lookup-ξ ξ (resolve id_mac Σ))]
                  [(scp_u Σ_1) (alloc-scope Σ)]
                  [(scp_i Σ_2) (alloc-scope Σ_1)])
      (in-eval
       `(,(Clo `(App ,val ,(flip (add stx_macapp scp_u) scp_i)) (init-env))
         • ,(init-store))
       (ζ (cons (Stx #f scp_i) ξ) '∘ κ Θ Σ_2)))]

   [(in-eval `(,(? Stx? stx_exp) • ,store_0)
             (ζ (cons (Stx #f scp_i) ξ) '∘ κ Θ Σ))
    (ζ (cons (flip stx_exp scp_i)  ξ) '∘ κ Θ Σ)]

   ;; if
   [(ζ (cons (Stx `(,(? id? id_if) ,stl_exps ...) ctx) ξ) '∘ κ Θ Σ)
    #:when (eq? 'if (resolve id_if Σ))
    (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons (Stx `(,id-seq ,stx-nil ,@stl_exps) ctx) ξ)
          '∘
          (mk-κ (Stx `(,id-kont ,id_if . ,hole) ctx) '∘ 𝓁_new)
          Θ_1 Σ))]

   [(ζ (Stx `(,(? id? id_kont)
               ,(? id? id_if) . ,(Stx val_exps ctx)) _) '∘ κ Θ Σ)
    #:when (and (eq? '#%kont (resolve id_kont Σ))
                (eq? 'if     (resolve id_if Σ)))
    (ζ (Stx `(,id_if ,@val_exps) ctx) '• κ Θ Σ)]

   ;; application (non-canonical #%app version)
   [(ζ (cons (Stx `(,(? id? id_app)
                     ,stx_fun ,stl_args ...) ctx) ξ) '∘ κ Θ Σ)
    #:when (eq? '#%app (resolve id_app Σ))
    (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons (Stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
          '∘
          (mk-κ (Stx (cons id_app hole) ctx) '• 𝓁_new)
          Θ_1 Σ))]

   ;; application (canonical #%app version)
   [(ζ (cons (Stx (cons (? id? id_app)
                         (Stx `(,stx_fun ,stl_args ...) _)) ctx) ξ)
        '∘ κ Θ Σ)
    #:when (eq? '#%app (resolve id_app Σ))
    (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons (Stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
          '∘
          (mk-κ (Stx (cons id_app hole) ctx) '• 𝓁_new)
          Θ_1 Σ))]

   ;; application
   [(ζ (cons (Stx `(,stx_fun ,stl_args ...) ctx) ξ) '∘ κ Θ Σ)
    #:when (or (not (id? stx_fun))
               (let ([name (resolve stx_fun Σ)])
                 (and (eq? 'not-found (lookup-ξ ξ name))
                      (not (member name
                                   '(lambda let quote syntax let-syntax if
                                      #%app #%kont #%seq #%ls-kont #%snoc))))))
    (let-values ([(id_app) (Stx (Sym '#%app) ctx)]
                 [(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons (Stx `(,id-seq ,stx-nil ,stx_fun ,@stl_args) ctx) ξ)
          '∘
          (mk-κ (Stx (cons id_app hole) ctx) '• 𝓁_new)
          Θ_1 Σ))]

   ;; reference
   [(ζ (cons (and id (Stx (Sym nam) ctx)) ξ) '∘ κ Θ Σ)
    (let ([all-transform (lookup-ξ ξ (resolve id Σ))])
      (match all-transform
        [`(TVar ,id_new) (ζ id_new '• κ Θ Σ)]
        [_ (error '==>c "unbound identifier: ~a" nam)]))]

   ;; literal
   [(ζ (cons (Stx (? atom? atom) ctx) ξ) '∘ κ Θ Σ)
    #:when (not (id? (Stx atom ctx)))
    (ζ (Stx `(,(Stx (Sym 'quote) ctx) ,(Stx atom ctx)) ctx)
        '• κ Θ Σ)]

   ;; pop κ
   [(ζ stx '• (κ STX ex? 𝓁) Θ Σ)
    (let ([κ (lookup-κ Θ 𝓁)])
      (ζ (in-hole STX stx) ex? κ Θ Σ))]


   ;; (#%seq (done ...) exp0 exp ...) -->
   ;;   (#%seq (done ... (expand exp0)) exp ...)
   [(ζ (cons (Stx `(,(? id? id_seq)
                     ,(Stx val_dones _)
                     ,stx_exp0 ,stl_exps ...) ctx) ξ) '∘ κ Θ Σ)
    #:when (eq? '#%seq (resolve id_seq Σ))
    (let-values ([(𝓁_new Θ_1) (push-κ Θ κ)])
      (ζ (cons stx_exp0 ξ) '∘
          (mk-κ (Stx `((,id_seq ,ξ)
                        ,(Stx `(,id-snoc ,(Stx val_dones (empty-ctx)) . ,hole)
                              (empty-ctx))
                        ,@stl_exps) ctx) '∘ 𝓁_new)
          Θ_1 Σ))]

   [(ζ (Stx `((,(? id? id_seq) ,ξ)
               ,(Stx `(,(? id? id_snoc)
                       ,(Stx val_dones ctx_1)
                       . ,(Stx val_done ctx_2)) _)
               ,stl_exps ...) ctx) '∘ κ Θ Σ)
    #:when (and (eq? '#%seq  (resolve id_seq Σ))
                (eq? '#%snoc (resolve id_snoc Σ)))
    (let ([val_dones2 (snoc val_dones (Stx val_done ctx_2))])
      (ζ (cons (Stx `(,id_seq ,(Stx val_dones2 ctx_1) ,@stl_exps)  ctx) ξ)
          '∘ κ Θ Σ))]

   ;; (#%seq (done ...)) --> (done ...)
   [(ζ (cons (Stx `(,(? id? id_seq) ,(Stx val_dones _)) ctx) ξ) '∘ κ Θ Σ)
    #:when (eq? '#%seq (resolve id_seq Σ))
    (ζ (Stx val_dones ctx) '• κ Θ Σ)]

   ;; in-eval
   [(in-eval s1 ζ)
    #:with (-->c s1)
    (λ (s2) (in-eval s2 ζ))]))

;; expand : stx ξ Σ -> (values stx Σ)
(define (expand stx ξ Σ)
  (let ([init-ζ (ζ (cons stx ξ) '∘ '• (init-Θ) Σ)])
    (match-let ([(list (ζ stx_new '• '• Θ_new Σ_new))
                 (apply-reduction-relation* ==>c init-ζ)])
      (values stx_new Σ_new))))

;; for debug

(define (expand==> form)
  (==>c (ζ (cons (run form 'read) (init-ξ)) '∘ '• (init-Θ) (init-Σ))))

(define (expand==>* form #:steps [steps #f])
  (print-as-expression #f)
  (apply-reduction-relation*
    ==>c
    (ζ (cons (run form 'read) (init-ξ)) '∘ '• (init-Θ) (init-Σ))
    #:steps steps))

(define (expand&parse form)
  (print-as-expression #f)
  (let ([r (expand==>* form)])
    (and (= (length r) 1)
         (match-let ([(ζ (and stx (Stx _ _)) '• '• Θ Σ) (car r)])
           (parse stx Σ)))))

;; ----------------------------------------
;; Drivers

;; init-env : -> env
(define (init-env) (make-immutable-hash))

;; init-store : -> store
(define (init-store) (Heap 0 (make-immutable-hash)))

;; init-ξ : -> ξ
(define (init-ξ) (make-immutable-hash))

;; init-Σ : -> Σ
(define (init-Σ) (Sto 0 (make-immutable-hash)))

;; init-Θ : -> Θ
(define (init-Θ) (Stk 0 (make-immutable-hash)))

(define-helpers (empty-ctx) reader printer)

;; stripper : (values stx Σ) -> val
(define (stripper stx Σ) (strip stx))

;; expander : stx -> (values stx Σ)
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
        ex-get-identity
        ))

(define (main [mode 'check])
  (run-examples run core:examples mode))
