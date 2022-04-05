#lang racket
(require redex
         "common.rkt"
         slideshow/pict
         (for-syntax racket/list))

(provide L stl->seq unzip zip snoc
         lookup-σ update-σ update-σ* push-cont alloc-loc alloc-loc*
         ;eval expand
         plus minus
         δ/stx δ
         addremove
         flip add strip
         subtract union
         bind resolve
         lookup-Σ binding-lookup biggest-subset
         parse lookup-env extend-env
         alloc-name alloc-scope
         primitives-env init-Σ
         run core:examples)

;; TODO
;;   (1) use redex parameter
;;
;;   (2) toy benchmark
;;   loopするメタレベル関数コード&loopする展開先コード&ループで無限にフェーズが大きくなる
;;   letrec, letrec-syntaxes? --> とりあえず μ を入れておく
;;
;;   (3) 抽象インナプリタ化(AAMでやる)
;;       まずはRedexに頼らずにworklist方式でnondeterministicな実行を可能にしておく．
;;       できればyieldを呼び出したときだけスイッチする方が効率良い？
;;   (4) 構文オブジェクトの抽象化(abstraction function)
;;     * コード部分
;;       - あまりおおざっぱだとevalやexpandが回らなくなりそう
;;       - 方針1: シンボルとリストは正確なまま．それ以外は近似．
;;           コードの大きさ(ペア数)がある程度を超えたら解析時エラーで落とす．
;;       - 方針2: コアフォームを「ある程度」認識できる程度にフォームの先頭部分の
;;           いくらかを正確に覚えておく．それだけの知識で「ある程度」ざっくりと
;;           動作できるagnosticなevalとexpandも抱き合わせで提案．
;;     * スコープ部分
;;       - use/def相当の情報をidにつけるにはどんなデータ構造が良い？
;;         それとも，単にスコープセットのセットにしてしまってぼやけてしまうことを
;;         問題提起しておわりにするか．．．
;;
;;   (5) Racketへの移植(大きめのベンチマークに必要)
;;

;;   eval, expand の small-step reduction化をとりあえずphases-machineまで
;;     * local-expand以降は，evalとexpandの相互再帰．
;;       それを有限回に抑える方法は？ということを
;;       local-expandを考慮すべきことや抽象化の工夫点として論文に書く．
;;       core, phasesでは expand から eval を呼び出すだけなので個々を
;;       有限にすれば十分ということも書く．
;;       --> coreでは無理だけどphasesでは相互再帰できそうな気がしてきた．
;;           let-syntaxのrhsをexpandする中でまたlet-syntaxすればいい？
;;       - 方針1: あらゆるeval, expand呼び出しで同じ探索空間を共有
;;       - 方針2: ADIの方法を流用．(call x 探索空間)でキャッシュ．
;;         呼出しも有限なのでキャッシュサイズも有限．方針1よりは精度が良さそう．


(define-language L
  ;; Executable AST and values:
  [ast ::=
       var
       val
       (Fun (var ...) ast)
       (App ast ast ...)
       (If ast ast ast)]
  [var ::= (Var nam)]
  [val ::=
       (Fun (var ...) ast ρ)
       atom
       (Cons val val)
       stx]

  ;; Syntax objects (a subset of values):
  [stx ::=
       (Stx atom ctx)
       (Stx (Cons stx stl) ctx)]
  [id ::= (Stx sym ctx)]
  [ctx ::= scps]
  [scps ::= (Set scp ...)]
  [stl ::= ;; syntax tail
       stx
       ()
       (Cons stx stl)]

  ;; Literal values:
  [atom ::=
        ()
        sym
        prim
        desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom ::=
                   number
                   boolean
                   ;; Not used until definition-context model:
                   addr
                   (Defs scp addr)]
  [sym ::= (Sym nam)]
  [prim ::=
        syntax-e
        datum->syntax
        desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim ::=
                   + - * / < = eq?
                   cons car cdr list second third fourth
                   ;; Not implemented at first, but it's simplest
                   ;; to include these in the grammar from the start:
                   stx-prim]
  [stx-prim ::=
            syntax-local-value local-expand
            syntax-local-identifier-as-binding
            box unbox set-box!
            syntax-local-make-definition-context
            syntax-local-bind-syntaxes]

  ;; Eval-time continuation and environment
  [cont ::=
        •
        (App val ... [] clo ... loc)
        (If [] clo clo loc)]
  [ρ ::= ([var loc] ...)]
  [clo ::=
       val
       ser]
  [ser ::= ;; serious (i.e., reducible) closure
       (ast ρ)
       (App clo ...)
       (If clo clo clo)]
  [state ::= (clo cont σ)]

  ;; Heap (also used for κ (continuation) at expand-time

  [σ ::= (Heap number [loc u] ...)]
  [u ::=
     val
     cont
     κ]
  [loc ::= nam]

  ;; Expand-time environment:
  [env ::= desc-env] ; `desc-env' typesets as prose
  [desc-env ::= ((nam all-transform) ...)]
  [transform ::=
             (TVar id)
             val
             κ]
  ;; The `TStop' transform type is not used at first:
  [all-transform ::=
                 transform
                 (TStop all-transform)
                 not-found]

  ;; Expand-time store:
  [Σ ::= desc-store] ; `desc-store' typesets as prose
  [desc-store ::= (Sto number      ; for alloc
                       (binds ...) ; binding store
                       boxes       ; for later model
                       def-envs)]  ; for later model
  [binds ::= [nam (StoBind scps nam) ...]]

  ;; Expand-time continuation:
  [stx∘ ::=
        stx
        (stx env) ;; to be expanded
        (Stx (Cons stx∘ stl∘) ctx)]
  [stl∘ ::=
        stx∘
        ()
        (Cons stx∘ stl∘)]
  [STX ::=
       hole
       (STX env)
       (Stx (Cons STX stl∘) ctx)
       (Stx (Cons stx∘ STL) ctx)]
  [STL ::=
       STX
       (Cons STX stl∘)
       (Cons stx∘ STL)]
  [κ ::=
     •
     (STX ex? loc)]

  [ex? ::=
       ∘ ;; to be expanded
       • ;; done
       ]

  ;; Expand-time state (configuration):
  [cfg ::=
       (stx∘ ex? κ σ Σ)
       (in-eval state cfg)]

  ;; for signatures only
  [continuation ::= cont κ]
  [configuration ::= state cfg]

  ;; Use names for vars, addrs, and scopes
  [nam ::= desc-name] ; `desc-name' typesets as prose
  [desc-name ::= variable-not-otherwise-mentioned]
  [scp ::= desc-scope] ; `desc-scope' typesets as prose
  [desc-scope ::= nam]

  [boxes ::= any] ; refined in a later model
  [def-envs ::= any]  ; refined in a later model
  [addr ::= nam]  ; used in a later model
  )


;; ----------------------------------------
;; stx utils

(define-metafunction L
  stl->seq : stl -> (stx ...)
  [(stl->seq ()) ()]
  [(stl->seq (Cons stx_0 stl))
   (stx_0 stx ...)
   (where (stx ...) (stl->seq stl))])

(define-metafunction L
  unzip : stl -> (values stl stl)
  [(unzip ()) (values () ())]
  [(unzip (Cons (Stx (Cons stx_left (Cons stx_right ())) ctx)
                stl_rest))
   (values (Cons stx_left stl_lefts) (Cons stx_right stl_rights))
   (where (values stl_lefts stl_rights) (unzip stl_rest))])

(define-metafunction L
  zip : stl stl ctx -> stl
  [(zip () () ctx) ()]
  [(zip (Cons stx_left stl_lefts) (Cons stx_right stl_rights) ctx)
   (Cons (Stx (Cons stx_left (Cons stx_right ())) ctx)
         (zip stl_lefts stl_rights ctx))])

(define-metafunction L
  snoc : stl stx -> stl
  [(snoc () stx) (Cons stx ())]
  [(snoc (Cons stx_1 stl) stx) (Cons stx_1 (snoc stl stx))])

;; ----------------------------------------
;; Implementation of primitives:

(define-metafunction L
  [(plus number ...) ,(apply + (term (number ...)))])
(define-metafunction L
  [(minus number ...) ,(apply - (term (number ...)))])
(define-metafunction L
  [(times number ...) ,(apply * (term (number ...)))])
(define-metafunction L
  [(div number ...) ,(apply / (term (number ...)))])
(define-metafunction L
  [(less-than number ...) ,(apply < (term (number ...)))])
(define-metafunction L
  [(num-eq number ...) ,(apply = (term (number ...)))])
(define-metafunction L
  [(sym-eq (Sym nam_1) (Sym nam_2)) ,(eq? (term nam_1) (term nam_2))])

(define-metafunction L
  δ/stx : prim (val ...) -> val
  [(δ/stx syntax-e ((Stx atom ctx))) atom]
  [(δ/stx syntax-e ((Stx (Cons stx stl) ctx))) (Cons stx stl)]

  [(δ/stx datum->syntax ((Stx val_0 ctx) stx)) stx]
  [(δ/stx datum->syntax ((Stx val_0 ctx) atom)) (Stx atom ctx)]
  [(δ/stx datum->syntax ((Stx val_0 ctx) (Cons val_1 val_2)))
   (Stx (Cons (δ/stx datum->syntax ((Stx val_0 ctx) val_1)) stl) ctx)
   (where (Stx stl ctx_2) (δ/stx datum->syntax ((Stx val_0 ctx) val_2)))])

(define-metafunction/extension δ/stx L
  δ : prim (val ...) -> val
  [(δ + (number ...)) (plus  number ...)]
  [(δ - (number ...)) (minus number ...)]
  [(δ * (number ...)) (times number ...)]
  [(δ / (number ...)) (div   number ...)]
  [(δ < (number ...)) (less-than number ...)]
  [(δ = (number ...)) (num-eq number ...)]
  [(δ eq? (sym_1 sym_2)) (sym-eq sym_1 sym_2)]

  [(δ cons (val_1 val_2)) (Cons val_1 val_2)]
  [(δ car ((Cons val_1 val_2))) val_1]
  [(δ cdr ((Cons val_1 val_2))) val_2]

  [(δ list ()) ()]
  [(δ list (val_1 val_2 ...)) (δ cons (val_1 (δ list (val_2 ...))))]
  [(δ second ((Cons val_1 (Cons val_2 val_3)))) val_2]
  [(δ third  ((Cons val_1 (Cons val_2 (Cons val_3 val_4))))) val_3]
  [(δ fourth ((Cons val_1 (Cons val_2 (Cons val_3 (Cons val_4 val_5))))))
   val_4])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction L
  lookup-σ : σ loc -> u
  [(lookup-σ (Heap number _ ... [loc u] _ ...) loc) u])

(define-metafunction L
  update-σ : σ loc u -> σ
  [(update-σ (Heap number
                   [loc_0 u_0] ...
                   [loc u_old]
                   [loc_1 u_1] ...) loc u_new)
   (Heap number
         [loc_0 u_0] ...
         [loc u_new]
         [loc_1 u_1] ...)]
  [(update-σ (Heap number [loc_0 u_0] ...) loc u_new)
   (Heap number [loc u_new] [loc_0 u_0] ...)])

(define-metafunction L
  update-σ* : σ (loc u) ... -> σ
  [(update-σ* σ) σ]
  [(update-σ* σ (loc_0 u_0) (loc u) ...)
   (update-σ* (update-σ σ loc_0 u_0) (loc u) ...)])

(define-metafunction L
  push-cont : configuration continuation -> (values loc σ)
  [(push-cont configuration continuation)
   (values loc σ_1)
   (where (values loc σ) (alloc-loc configuration))
   (where σ_1 (update-σ σ loc continuation))])

(define-metafunction L
  alloc-loc : configuration -> (values loc σ)
  ;; for eval-time continuation
  [(alloc-loc ((App val ... clo_0 clo ...) cont (Heap number [loc u] ...)))
   (values ,(string->symbol (format "vapp:~a" (term number)))
           (Heap ,(add1 (term number)) [loc u] ...))]
  [(alloc-loc ((If clo_test clo_then clo_else) cont (Heap number [loc u] ...)))
   (values ,(string->symbol (format "vif:~a" (term number)))
           (Heap ,(add1 (term number)) [loc u] ...))]

  ;; for expand-time continuation
  [(alloc-loc (stx∘ ex? κ (Heap number [loc u] ...) Σ))
   (values ,(string->symbol (format "exp:~a" (term number)))
           (Heap ,(add1 (term number)) [loc u] ...))])

;; for eval-time value binding
#;
(define-metafunction L
  alloc-loc* : (loc ...) (nam ...) σ -> (values (loc ...) σ)
  [(alloc-loc* (loc ...) () σ) (values (loc ...) σ)]
  [(alloc-loc* (loc_new ...) (nam_0 nam ...) (Heap number [loc u] ...))
   (alloc-loc* (loc_new ... loc_0) (nam ...)
               (Heap ,(add1 (term number)) [loc u] ...))
   (where loc_0 ,(string->symbol (format "~a:~a" (term nam_0) (term number))))])

(define-metafunction L
  alloc-loc* : (nam ...) σ -> (values (loc ...) σ)
  [(alloc-loc* () σ) (values () σ)]
  [(alloc-loc* (nam_0 nam ...) (Heap number [loc u] ...))
   (values (loc_0 loc_new ...) σ_new)
   (where loc_0 ,(string->symbol (format "~a:~a" (term nam_0) (term number))))
   (where (values (loc_new ...) σ_new)
          (alloc-loc* (nam ...) (Heap ,(add1 (term number)) [loc u] ...)))])


(define -->c
  (reduction-relation
   L #:domain state

   ;; propagate ρ into subterms
   (--> (((If ast_test ast_then ast_else) ρ) cont σ)
        ((If (ast_test ρ) (ast_then ρ) (ast_else ρ)) cont σ)
        ev-ρ-if)

   (--> (((App ast_fun ast_arg ...) ρ) cont σ)
        ((App (ast_fun ρ) (ast_arg ρ) ...) cont σ)
        ev-ρ-app)

   ;; value
   (--> ((val ρ) cont σ) (val cont σ) ev-val)

   ;; reference
   (--> ((var ρ) cont σ) (((lookup-σ σ (find ρ var)) ρ) cont σ) ev-x)

   ;; lambda
   (--> (((Fun (var ...) ast)   ρ) cont σ)
        (((Fun (var ...) ast ρ) ρ) cont σ)
        ev-lam)

   ;; application
   (--> (name s ((App val ... ser clo ...) cont σ))
        (ser (App val ... [] clo ... loc_new) σ_1)

        (where (values loc_new σ_1) (push-cont s cont))
        ev-push-app)

   (--> (val_0 (App val ... [] clo ... loc_cont) σ)
        ((App val ... val_0 clo ...) (lookup-σ σ loc_cont) σ)
        ev-pop-app)

   ;; β
   (--> ((App (Fun ((Var nam) ...) ast ρ) val ...) cont σ)
        ((ast ρ_new) cont σ_2)

        (where (values (loc ...) σ_1) (alloc-loc* (nam ...) σ))
        (where ρ_new (ext ρ ((Var nam) loc) ...))
        (where σ_2 (update-σ* σ_1 (loc val) ...))
        ev-β)

   ;; primitive application
   (--> ((App prim val ...) cont σ)
        ((δ prim (val ...)) cont σ)
        ev-δ)

   ;; if
   (--> (name s ((If ser_test clo_then clo_else) cont σ))
        (ser_test (If [] clo_then clo_else loc_new) σ_1)

        (where (values loc_new σ_1) (push-cont s cont))
        ev-push-if)

   (--> (val (If [] clo_then clo_else loc_cont) σ)
        ((If val clo_then clo_else) (lookup-σ σ loc_cont) σ)
        ev-pop-if)

   (--> ((If #f clo_then clo_else) cont σ)
        (clo_else cont σ)
        ev-if-#f)

   (--> ((If val clo_then clo_else) cont σ)
        (clo_then cont σ)

        (side-condition (not (equal? (term val) #f)))
        ev-if-#t)))

(define-metafunction L
  eval : ast -> val
  [(eval ast)
   val
   (where ((val • σ))
          ,(apply-reduction-relation* -->c (term ((ast ()) • (Heap 0)))))])

;; for debug

(define (trace--> form)
  (traces -->c (term ((,(run form 'parse) ()) • (Heap 0)))))

(define (eval--> form)
  (apply-reduction-relation* -->c (term ((,(run form 'parse) ()) • (Heap 0)))))


;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction L
  add : stl scp -> stl
  ;; Simply pushes scopes down through a syntax object
  [(add (Stx atom ctx) scp)
   (Stx atom (union (Set scp) ctx))]
  [(add (Stx (Cons stx stl) ctx) scp)
   (Stx (Cons (add stx scp) (add stl scp)) (union (Set scp) ctx))]
  [(add () scp) ()]
  [(add (Cons stx stl) scp) (Cons (add stx scp) (add stl scp))])

(define-metafunction L
  ;; Adds or cancels a scope
  addremove : scp scps -> scps
  [(addremove scp_2 (Set scp_1 ... scp_2 scp_3 ...)) (Set scp_1 ... scp_3 ...)]
  [(addremove scp_1 (Set scp_2 ...)) (Set scp_1 scp_2 ...)])

(define-metafunction L
  flip : stl scp -> stl
  ;; Pushes flipping a scope down through a syntax object
  [(flip (Stx atom ctx) scp)
   (Stx atom (addremove scp ctx))]
  [(flip (Stx (Cons stx stl) ctx) scp)
   (Stx (Cons (flip stx scp) (flip stl scp)) (addremove scp ctx))]
  [(flip () scp) ()]
  [(flip (Cons stx stl) scp) (Cons (flip stx scp) (flip stl scp))])

(define-metafunction L
  strip : stl -> val
  ;; Recursively strips lexical context from a syntax object
  [(strip (Stx atom ctx))
   atom]
  [(strip (Stx (Cons stx stl) ctx))
   (Cons (strip stx) (strip stl))]
  [(strip ()) ()]
  [(strip (Cons stx stl)) (Cons (strip stx) (strip stl))])

(define-metafunction L
  subtract : scps scps -> scps
  [(subtract scps (Set)) scps]
  [(subtract (Set scp_1 ... scp scp_2 ...) (Set scp scp_3 ...))
   (subtract (Set scp_1 ... scp_2 ...) (Set scp_3 ...))]
  [(subtract scps (Set scp scp_1 ...))
   (subtract scps (Set scp_1 ...))])

(define-metafunction L
  union : scps scps -> scps
  [(union (Set scp_1 ...) (Set scp_2 ...)) (Set scp_1 ... scp_2 ...)])

(define-metafunction L
  bind : Σ id nam -> Σ
  ;; Add a binding using the name and scopes of an identifier, mapping
  ;; them in the store to a given name
  [(bind (Sto number
              (binds_1 ... [nam_1 (StoBind ctx_2 nam_2) ...] binds_2 ...)
              boxes
              def-envs)
         (Stx (Sym nam_1) ctx_1)
         nam_3)
   (Sto number
        (binds_1 ...
         [nam_1 (StoBind ctx_1 nam_3) (StoBind ctx_2 nam_2) ...]
         binds_2 ...)
        boxes
        def-envs)]
  [(bind (Sto number (binds ...) boxes def-envs)
         (Stx (Sym nam_1) ctx_1)
         nam_3)
   (Sto number
        ([nam_1 (StoBind ctx_1 nam_3)] binds ...)
        boxes
        def-envs)])

(define-metafunction L
  resolve : id Σ -> nam
  [(resolve (Stx (Sym nam) ctx) Σ)
   nam_biggest
   (where (Set (StoBind scps_bind nam_bind) ...) (lookup-Σ Σ nam))
   (where scps_biggest (biggest-subset ctx (Set scps_bind ...)))
   (where nam_biggest (binding-lookup (Set (StoBind scps_bind nam_bind) ...)
                                      scps_biggest))]
  [(resolve (Stx (Sym nam) ctx) Σ) nam])

(define-metafunction L
  lookup-Σ : Σ nam -> (Set (StoBind scps nam) ...)
  [(lookup-Σ (Sto number
                      (_ ... [nam (StoBind scps_bind nam_bind) ...] _ ...)
                      _ _)
                 nam)
   (Set (StoBind scps_bind nam_bind) ...)]
  [(lookup-Σ Σ nam) (Set)])

(define-metafunction L
  binding-lookup : (Set (StoBind scps nam) ...) scps -> nam ∪ #f
  [(binding-lookup (Set _ ... (StoBind scps nam) _ ...) scps) nam]
  [(binding-lookup _ scps) #f])

(define-metafunction L
  biggest-subset : scps (Set scps ...) -> scps
  [(biggest-subset scps_ref (Set scps_bind ...))
   scps_biggest
   (where scps_biggest
          ;; The biggest-subset search seems easiest to write in Racket:
          ,(let* ([matching
                   (filter (lambda (scps_bind)
                             (subset? scps_bind (term scps_ref)))
                           (term (scps_bind ...)))]
                  [sorted
                   (sort matching
                         (lambda (a b)
                           (> (length a) (length b))))])
             ;; The binding is ambigious if the first scps in
             ;; `sorted` is not bigger than the others, or if
             ;; some scps in `sorted` is not a subset of the
             ;; first one.
             (if (or (empty? sorted)
                     (and (pair? (rest sorted))
                          (= (length (first sorted))
                             (length (second sorted))))
                     (ormap (lambda (b)
                              (not (subset? b (first sorted))))
                            (rest sorted)))
                 #f
                 (first sorted))))]
  [(biggest-subset _ _) (Set)])

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

(define-metafunction L
  parse : stx Σ -> ast

  [; (lambda (id ...) stx_body)
   (parse (Stx (Cons id_lam (Cons (Stx stl_ids _) (Cons stx_body ()))) ctx) Σ)
   (Fun ((Var (resolve id Σ)) ...) (parse stx_body Σ))

   (where lambda (resolve id_lam Σ))
   (where (id ...) (stl->seq stl_ids))]

  [; (let ([id stx_rhs] ...) stx_body)
   (parse (Stx
           (Cons id_let
                 (Cons (Stx stl_binds ctx_1)
                       (Cons stx_body ()))) ctx_2) Σ)
   (App (Fun ((Var (resolve id Σ)) ...) (parse stx_body Σ))
        (parse stx_rhs Σ) ...)

   (where let (resolve id_let Σ))
   (where (values stl_ids stl_rhs) (unzip stl_binds))
   (where (id ...) (stl->seq stl_ids))
   (where (stx_rhs ...) (stl->seq stl_rhs))]

  [; (quote stx)
   (parse (Stx (Cons id_quote (Cons stx ())) ctx) Σ)
   (strip stx)

   (where quote (resolve id_quote Σ))]

  [; (syntax stx)
   (parse (Stx (Cons id_syntax (Cons stx ())) ctx) Σ)
   stx

   (where syntax (resolve id_syntax Σ))]

  [; (#%app stx_fun stx_arg ...) トップレベルがstx-pair (cdr部もstx)であることに注意
   (parse (Stx (Cons id_app (Stx (Cons stx_fun stl_args) ctx_1)) ctx_2) Σ)
   (App (parse stx_fun Σ) ast_arg ...)

   (where #%app (resolve id_app Σ))
   (where (ast_arg ...) (parse* stl_args Σ))]

  [; (if stx stx stx)
   (parse (Stx (Cons id_if (Cons stx_test
                                 (Cons stx_then (Cons stx_else ())))) ctx) Σ)
   (If (parse stx_test Σ) (parse stx_then Σ) (parse stx_else Σ))

   (where if (resolve id_if Σ))]

  [; reference
   (parse id Σ)
   (Var (resolve id Σ))]

  [; literal
   (parse (Stx atom ctx) Σ)
   atom])

(define-metafunction L
  parse* : stl Σ -> (ast ...)

  [(parse* stx Σ) (parse stx Σ)]

  [(parse* () Σ) ()]

  [(parse* (Cons stx stl) Σ)
   ((parse stx Σ) ast ...)

   (where (ast ...) (parse* stl Σ))])

;; ----------------------------------------
;; Expand-time environment operations:

(define-metafunction L
  lookup-env : env nam -> all-transform
  [(lookup-env ((nam all-transform) any_2 ...) nam) all-transform]
  [(lookup-env (any_1 any_2 ...) nam) (lookup-env (any_2 ...) nam)]
  [(lookup-env () nam) not-found #;nam])

(define-metafunction L
  extend-env : env nam all-transform -> env
  [(extend-env env nam all-transform) ((nam all-transform) . env)])

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

(define-metafunction L
  alloc-name : id Σ -> (values nam Σ)
  [(alloc-name (Stx (Sym nam) ctx) (Sto number (binds ...) boxes def-envs))
   (values ,(string->symbol (format "~a:~a" (term nam) (term number)))
           (Sto ,(add1 (term number)) (binds ...) boxes def-envs))])

(define-metafunction L
  alloc-scope : Σ -> (values scp Σ)
  [(alloc-scope (Sto number any boxes def-envs))
   (values ,(string->symbol (format "scp:~a" (term number)))
           (Sto ,(add1 (term number)) any boxes def-envs))])

(define-metafunction L
  regist-vars : scp stl env Σ -> (values stl env Σ)

  [(regist-vars scp () env Σ) (values () env Σ)]

  [(regist-vars scp (Cons id stl) env Σ)
   (values (Cons id_new stl_reg) env_2 Σ_3)

   (where (values stl_reg env_1 Σ_1) (regist-vars scp stl env Σ))
   (where (values nam_new Σ_2) (alloc-name id Σ_1))
   (where id_new (add id scp))
   (where Σ_3 (bind Σ_2 id_new nam_new))
   (where env_2 (extend-env env_1 nam_new (TVar id_new)))])

;; ----------------------------------------
;; The expander:

(define-term id-kont (Stx (Sym #%kont) (Set)))
(define-term id-seq (Stx (Sym #%seq) (Set)))
(define-term id-snoc (Stx (Sym #%snoc) (Set)))

(define-term stx-nil (Stx () (Set)))

(define ==>c
  (reduction-relation
   L #:domain cfg #:arrow ==>

   ;; lambda
   (==> (name
         s
         (((Stx (Cons id_lam (Cons (Stx stl_args ctx_0)
                                   (Cons stx_body ()))) ctx) env) ∘
          κ
          σ Σ))
        (((add stx_body scp_new) env_new) ∘
         ((Stx (Cons id_lam (Cons (Stx stl_args2 ctx_0)
                                  (Cons hole ()))) ctx) • loc_new)
         σ_1 Σ_2)

        (where lambda (resolve id_lam Σ))
        (where (values scp_new Σ_1) (alloc-scope Σ))
        (where (values stl_args2 env_new Σ_2)
               (regist-vars scp_new stl_args env Σ_1))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-lam-body)

   ;; let
   (==> (name
         s
         (((Stx (Cons id_let
                      (Cons (Stx stl_binds ctx_1)
                            (Cons stx_body ()))) ctx) env) ∘
          κ
          σ Σ))
        (((add stx_body scp_new) env_new) ∘
         ((Stx (Cons id-kont
                     (Cons id_let
                           (Cons (Stx (Cons (Stx stl_vars2 ctx_1)
                                            ((Stx stl_rhs ctx_1) env)) ctx_1)
                                 (Cons hole ())))) ctx)
          ∘ loc_new)
         σ_1 Σ_2)

        (where let (resolve id_let Σ))
        (where (values stl_vars stl_rhs) (unzip stl_binds))
        (where (values scp_new Σ_1) (alloc-scope Σ))
        (where (values stl_vars2 env_new Σ_2)
               (regist-vars scp_new stl_vars env Σ_1))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-let-body)

   (==> (name
         s
         ((Stx (Cons id_kont
                     (Cons id_let
                           (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                            ((Stx stl_rhs ctx_1) env)) ctx_1)
                                 (Cons stx_body ())))) ctx) ∘
          κ σ Σ))
        (((Stx (Cons id-seq (Cons stx-nil stl_rhs)) ctx_1) env) ∘
         ((Stx (Cons id_kont
                     (Cons id_let
                           (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                            hole) ctx_1)
                                 (Cons stx_body ())))) ctx)
          ∘ loc_new)
         σ_1 Σ)

        (where let (resolve id_let Σ))
        (where #%kont (resolve id_kont Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-let-rhs)

   (==> ((Stx (Cons id_kont
                    (Cons id_let
                          (Cons (Stx (Cons (Stx stl_vars ctx_1)
                                           (Stx val_rhs ctx_1)) ctx_1)
                                (Cons stx_body ())))) ctx) ∘
         κ σ Σ)
        ((Stx (Cons id_let
                    (Cons (Stx (zip stl_vars val_rhs ctx_1) ctx_1)
                          (Cons stx_body ()))) ctx) •
         κ σ Σ)

        (where let (resolve id_let Σ))
        (where #%kont (resolve id_kont Σ))
        ex-let-rhs2)

   ;; quote
   (==> (((Stx (Cons id_quote (Cons stx ())) ctx) env) ∘ κ σ Σ)
        ((Stx (Cons id_quote (Cons stx ())) ctx) • κ σ Σ)

        (where quote (resolve id_quote Σ))
        ex-quote)

   ;; syntax
   (==> (((Stx (Cons id_syntax (Cons stx ())) ctx) env) ∘ κ σ Σ)
        ((Stx (Cons id_syntax (Cons stx ())) ctx) • κ σ Σ)

        (where syntax (resolve id_syntax Σ))
        ex-stx)

   ;; macro creation
   (==> (((Stx (Cons id_ls
                     (Cons (Stx (Cons (Stx (Cons
                                            id
                                            (Cons stx_rhs ()))
                                           ctx_0) ()) ctx_1)
                           (Cons stx_body ()))) ctx) env) ∘ κ σ Σ)
        ((Stx (Cons id_ls
                    (Cons (Stx (Cons (Stx (Cons
                                           id
                                           (Cons stx_rhs ()))
                                          ctx_0) ()) ctx_1)
                          (Cons (stx_body env) ()))) ctx) ∘ κ σ Σ)

        (where let-syntax (resolve id_ls Σ))
        ex-env-ls)

   (==> (name
         s
         ((Stx (Cons
                id_ls
                (Cons (Stx (Cons (Stx (Cons
                                       id
                                       (Cons stx_rhs ())) ctx_0) ()) ctx_1)
                      (Cons (stx_body env) ()))) ctx) ∘
          κ σ Σ))
        ((stx_rhs (primitives-env)) ∘
         ((Stx (Cons
                id-kont
                (Cons id_ls
                      (Cons (Stx (Cons (Stx (Cons
                                             id_new
                                             (Cons hole ())) ctx_0) ()) ctx_1)
                            (Cons (stx_body2 env) ())))) ctx)
          ∘ loc_new)
         σ_1 Σ_3)

        (where let-syntax (resolve id_ls Σ))
        (where (values nam_new Σ_1) (alloc-name id Σ))
        (where (values scp_new Σ_2) (alloc-scope Σ_1))
        (where id_new (add id scp_new))
        (where Σ_3 (bind Σ_2 id_new nam_new))
        (where (values loc_new σ_1) (push-cont s κ))
        (where stx_body2 (add stx_body scp_new))
        ex-ls-push-rhs)

   (==> ((Stx
          (Cons id_kont
                (Cons
                 id_ls
                 (Cons (Stx (Cons (Stx (Cons
                                        id_new
                                        (Cons stx_exp ())) ctx_0) ()) ctx_1)
                       (Cons (stx_body2 env) ())))) ctx) ∘
         κ σ Σ)
        (in-eval (((parse stx_exp Σ) ()) • (Heap 0))
                 ((Stx (Cons (Stx (Sym nam_new) (Set))
                             (Cons (stx_body2 env) ())) (Set)) ∘ κ σ Σ))

        (where let-syntax (resolve id_ls Σ))
        (where #%kont (resolve id_kont Σ))
        (where nam_new (resolve id_new Σ))
        ex-ls-eval)

   (==> (in-eval (val • σ_0)
                 ((Stx (Cons (Stx (Sym nam_new) (Set))
                             (Cons (stx_body2 env) ())) (Set)) ∘ κ σ Σ))
        ((stx_body2 env_new) ∘ κ σ Σ)

        (where env_new (extend-env env nam_new val))
        ex-ls-env)

   ;; macro invocation
   (==> ((stx_macapp env) ∘ κ σ Σ)
        (in-eval (((App val (flip (add stx_macapp scp_u) scp_i)) ()) • (Heap 0))
                 (((Stx #f (Set scp_i)) env) ∘ κ σ Σ_2))

        (where (Stx (Cons id_mac stl_args) ctx) stx_macapp)
        (where val (lookup-env env (resolve id_mac Σ)))
        (where (values scp_u Σ_1) (alloc-scope Σ))
        (where (values scp_i Σ_2) (alloc-scope Σ_1))
        ex-macapp-eval)

   (==> (in-eval (stx_exp • σ_0)
                 (((Stx #f (Set scp_i)) env) ∘ κ σ Σ))
        (((flip stx_exp scp_i) env) ∘ κ σ Σ)
        ex-macapp-flip)

   ;; if
   (==> (name s (((Stx (Cons id_if stl_exps) ctx) env) ∘
                 κ σ Σ))
        (((Stx (Cons id-seq (Cons stx-nil stl_exps)) ctx) env) ∘
         ((Stx (Cons id-kont (Cons id_if hole)) ctx) ∘ loc_new)
         σ_1 Σ)

        (where if (resolve id_if Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-if)

   (==> ((Stx (Cons id_kont (Cons id_if (Stx val_exps ctx))) ctx) ∘ κ σ Σ)
        ((Stx (Cons id_if val_exps) ctx) • κ σ Σ)

        (where #%kont (resolve id_kont Σ))
        (where if (resolve id_if Σ))
        ex-if-kont)

   ;; application (non-canonical #%app version)
   (==> (name s (((Stx (Cons id_app (Cons stx_fun stl_args)) ctx) env) ∘
                 κ σ Σ))
        (((Stx (Cons id-seq (Cons stx-nil (Cons stx_fun stl_args))) ctx) env) ∘
         ((Stx (Cons id_app hole) ctx) • loc_new)
         σ_1 Σ)

        (where #%app (resolve id_app Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-#%app)

   ;; application (canonical #%app version)
   (==> (name
         s
         (((Stx (Cons id_app (Stx (Cons stx_fun stl_args) ctx_1)) ctx) env) ∘
          κ σ Σ))
        (((Stx (Cons id-seq (Cons stx-nil (Cons stx_fun stl_args))) ctx) env) ∘
         ((Stx (Cons id_app hole) ctx) • loc_new)
         σ_1 Σ)

        (where #%app (resolve id_app Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-#%app2)

   ;; application
   (==> (name s (((Stx (Cons stx_fun stl_args) ctx) env) ∘ κ σ Σ))
        (((Stx (Cons id-seq (Cons stx-nil (Cons stx_fun stl_args))) ctx) env) ∘
         ((Stx (Cons id_app hole) ctx) • loc_new)
         σ_1 Σ)

        (side-condition
         (or (not (redex-match? L id (term stx_fun)))
             (let ([name (term (resolve stx_fun Σ))])
               (and (redex-match? L not-found (term (lookup-env env ,name)))
                    (not (member name
                                 '(lambda let quote syntax let-syntax if
                                   #%app #%kont #%seq #%ls-kont #%snoc)))))))
        (where id_app (Stx (Sym #%app) ctx))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-app)

   ;; reference
   (==> ((id env) ∘ κ σ Σ)
        (id_new • κ σ Σ)

        (where (TVar id_new) (lookup-env env (resolve id Σ)))
        ex-var)

   ;; literal
   (==> (((Stx atom ctx) env) ∘ κ σ Σ)
        ((Stx (Cons (Stx (Sym quote) ctx) (Cons (Stx atom ctx) ())) ctx)
         • κ σ Σ)

        (side-condition (not (redex-match? L id (term (Stx atom ctx)))))
        ex-lit)

   ;; pop κ
   (==> (stx • (STX ex? loc) σ Σ)
        ((in-hole STX stx) ex? κ σ Σ)

        (where κ (lookup-σ σ loc))
        ex-pop-κ)

   ;; expression sequence
   ;;  (expand (seq (exped ...))) --> (exped ...)
   (==> (((Stx (Cons id_seq (Cons (Stx val_expeds (Set)) ())) ctx) env) ∘
         κ σ Σ)
        ((Stx val_expeds ctx) • κ σ Σ)

        (where #%seq (resolve id_seq Σ))
        ex-seq-nil)

   ;; (expand (seq (done ...) exp0 exp ...)) -->
   ;;   (expand (seq (done ... (expand exp0)) exp ...))
   (==> (name
         s
         (((Stx (Cons id_seq (Cons (Stx val_dones (Set))
                                   (Cons stx_exp0 stl_exps))) ctx) env) ∘
          κ σ Σ))
        ((stx_exp0 env) ∘
         ((Stx (Cons (id_seq env)
                     (Cons (Stx (Cons id-snoc
                                      (Cons (Stx val_dones (Set)) hole)) (Set))
                           stl_exps)) ctx) ∘ loc_new)
         σ_1 Σ)

        (where #%seq (resolve id_seq Σ))
        (where (values loc_new σ_1) (push-cont s κ))
        ex-seq-cons)

   (==> ((Stx (Cons (id_seq env)
                    (Cons (Stx (Cons id_snoc
                                     (Cons (Stx val_exps ctx_1)
                                           (Stx val_exp ctx_2))) (Set))
                          stl_exps)) ctx) ∘
         κ σ Σ)
        (((Stx (Cons id_seq
                     (Cons (Stx val_exps2 ctx_1)
                           stl_exps)) ctx) env) ∘
         κ σ Σ)

        (where #%seq (resolve id_seq Σ))
        (where #%snoc (resolve id_snoc Σ))
        (where val_exps2 (snoc val_exps (Stx val_exp ctx_2)))
        ex-seq-snoc)


   ;; one-step eval (-->c)
   (-->c state
         state_new
         (where (state_new)
                ,(apply-reduction-relation -->c (term state))))

   with
   ((==> (in-eval s1 cfg) (in-eval s2 cfg))
    (-->c s1 s2))))

(define-metafunction L
  expand : stx env Σ -> (values stx Σ)
  [(expand stx env Σ)
   (values stx_new Σ_new)
   (where ((stx_new • • σ_new Σ_new))
          ,(apply-reduction-relation* ==>c (term ((stx env) ∘ • (Heap 0) Σ))))])

;; for debug

(define (step==> form)
  (stepper
   ==>c (term ((,(run form 'read) (primitives-env))
               ∘ • (Heap 0) (init-Σ)))))

(define (trace==> form)
  (traces
   ==>c (term ((,(run form 'read) (primitives-env))
               ∘ • (Heap 0) (init-Σ)))))

(define (expand==> form)
  (apply-reduction-relation*
   ==>c (term ((,(run form 'read) (primitives-env))
               ∘ • (Heap 0) (init-Σ)))))

(define (expand&parse form)
  (let ([r (expand==> form)])
    (and (= (length r) 1)
         (term (parse/values (values ,(caar r) ,(fifth (car r))))))))


;; ----------------------------------------
;; Drivers

(define-metafunction L
  primitives-env : -> env
  [(primitives-env) ()])

(define-metafunction L
  init-Σ : -> Σ
  [(init-Σ) (Sto 0 () () ())])

(define-helpers L (Set)
  reader printer)

(define-metafunction L
  stripper : (values stx Σ) -> val
  [(stripper (values stx Σ)) (strip stx)])

(define-metafunction L
  expander : stx -> (values stx Σ)
  [(expander stx) (expand stx (primitives-env) (init-Σ))])

(define-metafunction L
  parse/values : (values stx Σ) -> ast
  [(parse/values (values stx Σ)) (parse stx Σ)])

(define-runner run
  reader
  expander
  stripper printer
  eval
  parse/values)


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
                        (#%app list #'lambda (#%app datum->syntax #'here (#%app list #'z))
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
                         (#%app list #'lambda (#%app datum->syntax stx (#%app list #'a)) 
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
        ([get-identity (lambda (stx)
                         (#%app datum->syntax
                                stx
                                (#%app list #'lambda
                                       (#%app datum->syntax stx (#%app list #'a))
                                       (#%app datum->syntax
                                              stx
                                              (#%app list #'lambda
                                                     (#%app datum->syntax
                                                            stx
                                                            (#%app list
                                                                   (#%app second (#%app syntax-e stx)) ;; #'a
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


;; ----------------------------------------

(module+ pict-command
  (require "rewrites.rkt"
           "config.rkt")
  (provide (all-defined-out))

  ;; scribbleの本文中で@tm[(Stx 1 ctx)]とか書く．
  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))
  (define (to-pict lw)
    (WR/inline (lw->pict L lw)))
  )

(module+ pict-material
  (require "rewrites.rkt"
           ;redex/pict
           (submod ".." pict-command))
  (provide (all-defined-out))

  ;; 言語のBNF文法．非終端記号単位で選択的表示
  ;; 改行の有無はソースコードに準拠
  (define base-bnf
    (WR (language->pict L #:nts '(ast stx))))

  ;; 一部省略したBNFをつくるためのダミー言語
  (define-extended-language Ldummy L
    (ast ::= .... (Mu x ast)))
  (define bnf-tail
    (WR (language->pict Ldummy)))

  ;; 後ろにを省略したい場合は，desc-... を真似る
  (define bnf-head
    (WR (language->pict L #:nts '(atom))))

  ;; 両方はこんな感じ
  (define-extended-language Ldummy2 L
    (ast ::= .... (Mu x ast) desc-other-atom))
  (define bnf-both
    (WR (language->pict Ldummy2)))


  ;; (or/c 'vertical
  ;;       'compact-vertical    *
  ;;       'vertical-overlapping-side-conditions
  ;;       'horizontal          **
  ;;       'horizontal-left-align
  ;;       'horizontal-side-conditions-same-line
  ;;       (-> (listof rule-pict-info?) pict-convertible?))
  (define base-eval-red
    (parameterize ([arrow-space 4])
      (WR (reduction-relation->pict -->c #:style 'compact-vertical))))

  (define base-expand-red
    (parameterize ([render-reduction-relation-rules '(3)]
                   [arrow-space 4])
      (WR (reduction-relation->pict ==>c #:style 'compact-vertical))))

)


(module+ pict
  (require "rewrites.rkt"
           redex/pict
           ;pict
           "config.rkt")
  (provide (all-defined-out))

  (define base-nts '(ast var val
                     stx id
                     atom
                     sym
                     nam))
  (define eval-language-pict
    (WR (language->pict L #:nts base-nts)))

  ;; evalではなく代わりに -->c を選択的に表示できるべき
  (define eval-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict eval #:contract? #t))))

  (define prim-nts '(prim))
  (define prim-language-pict
    (WR (language->pict L #:nts prim-nts)))
  (define δ-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict δ/stx))))

  (define parse-pict
    (WR (metafunction->pict parse #:contract? #t)))
  (define resolve-nts '(scps ctx
                             Σ
                             scp))
  (define resolve-language-pict
    (WR (language->pict L #:nts resolve-nts)))
  (define resolve-pict
    (vl-append
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (blank 0 (metafunction-gap-space)))
     (WR
      (parameterize ([where-combine (lambda (l r) r)]
                     [metafunction-cases '(0)])
        (metafunction->pict biggest-subset #:contract? #t)))))

  ;; 代わりに ==>cの中から表示する
  (define (make-expand-pict pos [contract? #f] #:narrower? [narrower? #f])
    (println (append
              (if contract? '(#f) '())
              (list (and narrow-mode? narrower?))))
    (parameterize ([metafunction-cases (list pos)]
                   [linebreaks (append
                                (if contract? '(#f) '())
                                (list (and narrow-mode? narrower?)))])
      (WR (metafunction->pict expand #:contract? contract?))))

  ;; (define expand-quote-pict (make-expand-pict 1 #t))
  ;; (define expand-syntax-pict (make-expand-pict 2))
  ;; (define expand-lambda-pict (make-expand-pict 0 #:narrower? #t))
  ;; (define expand-var-pict (make-expand-pict 6))
  ;; (define expand-let-syntax-pict (make-expand-pict 3))
  ;; (define expand-macro-app-pict (make-expand-pict 4))
  ;; (define expand-app-pict
  ;;   (vl-append
  ;;    (make-expand-pict 5 #:narrower? #t)
  ;;    (WR (blank 0 (metafunction-gap-space)))))

  (define expand-nts '(env transform))
  (define expand-language-pict
    (WR (language->pict L #:nts expand-nts)))

  (define add+flip-pict
    (vl-append
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (WR (blank 0 (metafunction-gap-space)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t)))))

  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict L lw)))

  (define all-nts (append base-nts
                          prim-nts
                          resolve-nts
                          expand-nts)))

(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view eval-language-pict
        (hc-append
         40
         #;
         (vl-append
          expand-language-pict
          expand-app-pict)
         parse-pict)
        resolve-pict
        add+flip-pict))

;; Providing this file to `scribble` will render the model.
;; Set the `SCOPE_SETS_TO_PDF` environment variable to get
;; the right scale for PDF output.
(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict))
  (provide doc)
  (define doc
    (make-model-doc
     "Single-Phase"
     (WR (language->pict L #:nts all-nts))
     #;
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict eval #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict strip #:contract? #t)))
     (WR (metafunction->pict expand #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
