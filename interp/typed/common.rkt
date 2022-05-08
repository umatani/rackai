#lang typed/racket
(require (rename-in typed/racket/base
                    [read r:read]
                    [eval r:eval]
                    [expand r:expand])
         "types.rkt"
         "queue.rkt")
(provide stl->seq zip unzip snoc
         in-hole in-hole-stl
         define-reduction-relation apply-reduction-relation*
         define-helpers define-runner
         run-ex run-examples run-all-examples)


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

(: in-hole : Stx Stx -> Stx)
(define (in-hole stx v)
  (match stx
    [(GenStx (? Atom? atom) ctx) (GenStx atom ctx)]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (in-hole stx v) (in-hole-stl stl v)) ctx)]
    [(Hole) v]
    [_ stx]))

(: in-hole-stl : Stl Stx -> Stl)
(define (in-hole-stl stl v)
  (match stl
    ['() '()]
    [(? Stx? stx) (in-hole stx v)]
    [(cons stx stl) (cons (in-hole stx v) (in-hole-stl stl v))]
    [(Hole) v]
    [_ stl]))

;;;; non-deterministic reduction engine

;; (reduction-relation clause ...) : state -> (Set state ...)

(define-syntax (define-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel A B clause ...)
     (with-syntax
       ([body (let loop ([body #'(ann (set) (Setof A))]
                         [cs (syntax->list #'(clause ...))])
                (if (null? cs)
                    body                                
                    (loop
                     (syntax-case (car cs) ()
                       [(p b)
                        #`(let ([n #,body])
                            (match s
                              [p (set-add n b)]
                              [_ n]))]
                       [(p #:when t b)
                        #`(let ([n #,body])
                            (match s
                              [p (if t (set-add n b) n)]
                              [_ n]))]
                       [(p #:with e k)
                        #`(let ([n #,body])
                            (match s
                              [p (set-union
                                  n
                                  (list->set
                                   (set-map e (λ ([alt : B]) (k alt)))))]
                              [_ n]))])
                     (cdr cs))))])
       #'(begin
           (: rel : (-> A (Setof A)))
           (define (rel s) body)))]))


#;
(define-syntax-rule (define-reduction-relation rel clauses ...)
  (define rel (reduction-relation clauses ...)))

#;
(define-syntax (reduction-relation stx)
  (syntax-case stx ()
    [(reduction-relation) #'(λ ([s : ζ]) : (Setof ζ) (set))]
    [(reduction-relation [pat #:when guard body ...] clauses ...)
     #'(λ ([s : ζ]) : (Setof ζ)
         (let ([nexts : (Setof ζ) ((reduction-relation clauses ...) s)])
           (match s
             [pat
              (if guard
                  (set-add nexts (begin body ...))
                  nexts)]
             [_ nexts])))]
    [(reduction-relation [pat #:with expr kont] clauses ...)
     #'(λ ([s : ζ]) : (Setof ζ)
         (let ([nexts : (Setof ζ) ((reduction-relation clauses ...) s)])
           (match s
             [pat (set-union nexts
                             (for/set ([alt (in-set expr)])
                               (kont alt)))]
             [_ nexts])))]
    [(reduction-relation [pat body ...] clauses ...)
     #'(λ ([s : ζ]) : (Setof ζ)
         (let ([nexts : (Setof ζ) ((reduction-relation clauses ...) s)])
           (match s
             [pat (set-add nexts (begin body ...))]
             [_ nexts])))]))


(: apply-reduction-relation* (∀ [A] (->* ((-> A (Setof A)) A)
                                          (#:steps (Option Natural))
                                          (Listof A))))
(define (apply-reduction-relation* --> s #:steps [steps #f])
  (let ([all-states : (Mutable-HashTable A Boolean) (make-hash)]
        [normal-forms : (Mutable-HashTable A Boolean) (make-hash)]
        [worklist ((inst make-queue A))])
    (define (loop [steps : (Option Natural)]) : Void
      (unless (or (queue-empty? worklist)
                  (and steps (<= steps 0)))
        (let* ([s (dequeue! worklist)]
               [nexts (--> s)])
          (if (set-empty? nexts)
              (hash-set! normal-forms s #t)
              (for ([next (in-set nexts)]
                #:when (not (hash-ref all-states next #f)))
                (hash-set! all-states next #t)
                (enqueue! worklist next))))
        (loop (and steps (sub1 steps)))))
    (enqueue! worklist s)
    (loop steps)
    (if steps
        (hash-keys all-states) ;; for debug
        (hash-keys normal-forms))))


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
         [(VFun `(,(Var #{nams : (Listof Nam)}) ...) ast env) `(VFun ,@nams)]
         [(Sym nam) nam]
         [(𝓁 nam) nam]
         [(Defs _ _) '(Defs)]
         [(cons v1 v2) (cons (printer v1) (printer v2))]
         [(GenStx a c) (vector 'Stx (printer a))]
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
                     stx2 #;(printer (stripper stx2 Σ2))
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
                        (List (Listof (List Symbol Sexp)))
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
