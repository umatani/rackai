;; ----------------------------------------
;; stx utils

(: stl->seq : Stl -> (Listof Stx))
(define (stl->seq stl)
  (match stl
    ['() '()]
    [(cons stx stl) (cons stx (stl->seq stl))]))

(: zip : ProperStl ProperStl Ctx -> ProperStl)
(define (zip stl_1 stl_2 ctx)
  (match* (stl_1 stl_2)
    [('() '()) '()]
    [((cons stx_left stl_lefts) (cons stx_right stl_rights))
     (cons (GenStx `(,stx_left ,stx_right) ctx)
           (zip stl_lefts stl_rights ctx))]))

#;
(: unzip : ∀ [A] (Listof (GenStx (List (GenStx A) (GenStx A)))) ->
   (Values (Listof (GenStx A)) (Listof (GenStx A))))
(: unzip : ∀ [A] ProperStl -> (Values ProperStl ProperStl))
(define (unzip stl)
  (match stl
    ['() (values '() '())]
    [`(,(GenStx `[,stx_left ,stx_right] _) ,stl_rest ...)
     (let-values ([(stl_lefts stl_rights) (unzip stl_rest)])
       (values (cons stx_left  stl_lefts)
               (cons stx_right stl_rights)))]))

(: snoc (-> ProperStl Stx ProperStl))
(define (snoc stl stx)
  (cond
    [(null? stl) (list stx)]
    [(list? stl) (cons (car stl) (snoc (cdr stl) stx))]
    [else (error "no such case")]))

(: in-hole-stl : Stl Stx -> Stl)
(define (in-hole-stl stl v)
  (match stl
    ['() '()]
    [(? Stx? stx) (in-hole stx v)]
    [(cons stx stl) (cons (in-hole stx v) (in-hole-stl stl v))]
    [(Hole) v]
    [_ stl]))


;;;; reader & printer


(define-syntax-rule (define-helpers empty-ctx reader printer)
  (...
   (begin
     (define reader : (-> Sexp Stx)
       (letrec ([read-stx : (-> Sexp Stx)
                          (λ (x)
                            (match x
                              [(? Prim?) (GenStx x empty-ctx)]
                              [(? symbol?) (GenStx (Sym x) empty-ctx)]
                              [(? Atom?) (GenStx x empty-ctx)]
                              [(? pair?)
                               (let ([stl (read-stl x)])
                                 (match stl
                                   ['() (GenStx '() empty-ctx)]
                                   [(? Stx? stx) stx]
                                   [(? pair? stl) (GenStx stl empty-ctx)]))]
                              [_ (error 'reader "not supported: ~a" x)]))]
                [read-stl : (-> (U Sexp (Pairof Sexp Sexp)) Stl)
                          (λ (xs)
                            (match xs
                              ['() '()]
                              [(cons y ys)
                               (cons (read-stx y) (read-stl ys))]
                              [(? Atom? atom) (read-stx atom)]))])
         read-stx))

     (: printer : Val -> Sexp)
     (define (printer val)
       (match val
         [(? (λ (v) (or (null? v) (Prim? v) (real? v) (boolean? v)))) val]
         [(VFun `(,(Var nams) ...) ast env) `(VFun ,@(cast nams (Listof Nam)))]
         [(Sym nam) nam]
         [(cons v1 v2) (cons (printer v1) (printer v2))]
         [(GenStx a c) (vector 'Stx (printer a))]
         ;[(𝓁 nam) nam]
         ;[(Defs _ _) '(Defs)]
         )))))

;;;; runner


(define-syntax-rule (define-runner run reader expander stripper printer
                      evaluate parser)
  (begin
    (: run : Sexp Symbol -> (U Ast Sexp))
    (define (run [form : Sexp] [mode : Symbol])
     (if (eq? mode 'none)
         form
         (let ([stx (reader form)])
           (if (eq? mode 'read)
               stx
               (let-values ([(stx2 Σ2) (expander stx)])
                 (if (eq? mode 'expand)
                     stx2 #;(printer (stripper stx2))
                     (let ([ast (parser stx2 Σ2)])
                       (if (eq? mode 'parse)
                           ast
                           (let ([ast2 (evaluate ast)])
                             (if (eq? mode 'eval)
                                 (printer ast2)
                                 (error 'run "unknown mode: ~e" mode)))))))))))))

;;;; Example runner

(: fail-count : (Parameterof Integer))
(define fail-count (make-parameter -1))

(: ex-runner : (-> Sexp Symbol Any) (List Symbol Sexp) Symbol -> Any)
(define (ex-runner run example mode)
  (printf "~a: " (car example))
  (println
   (case mode
     [(raw) (first (call-with-values (λ () (r:eval (cadr example)))
                                     (λ args args)))]
     [(check)
      (fail-count (if (< (fail-count) 0) 0 (fail-count)))
      (let ([result (equal? (run (cadr example) 'eval)
                            (first (call-with-values (λ () (r:eval (cadr example)))
                                                     (λ args args))))])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]
     [else (run (cadr example) mode)])))

(: run-ex (->* ((-> Sexp Symbol Any) (Listof (List Symbol Sexp)) Symbol)
               (Symbol) Any))
(define (run-ex run examples name [mode 'check])
  (let ([example (assoc name examples)])
    (when example (ex-runner run example mode))))

(: run-examples (->* ((-> Sexp Symbol Any) (Listof (List Symbol Sexp)))
                     (Symbol) Any))
(define (run-examples run examples [mode 'check])
  (for ([example (in-list examples)])
    (ex-runner run example mode)))

(: run-all-examples (-> (Listof (List Symbol (-> Sexp Symbol Any)))
                        (Listof (Listof (List Symbol Sexp)))
                        (->* () (Symbol) Any)))
(define ((run-all-examples all-runs all-examples) [mode 'check])
  (parameterize ([fail-count (if (eq? mode 'check) 0 -1)])
    (for ([run (in-list all-runs)]
          [idx (in-naturals 1)])
      (printf "[~a]\n" (car run))
      (for ([i (in-range idx)]
            [examples (in-list all-examples)])
        (run-examples (cadr run) examples mode)))
    (when (>= (fail-count) 0)
      (printf "\nfail-count: ~a\n" (fail-count)))))
