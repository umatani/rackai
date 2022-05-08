#lang racket
(require (rename-in racket/base
                    [read r:read]
                    [eval r:eval]
                    [expand r:expand])
         racket/struct
         data/queue)
(provide r:read r:eval r:expand

         zip unzip snoc
         (struct-out Stx)
         (struct-out Sym)
         (struct-out 𝓁)
         (struct-out Defs)
         id? val? ser? atom? prim?

         (struct-out Clo)
         (struct-out Heap)
         (struct-out κ)
         (struct-out Sto)
         (struct-out StoBind)
         (struct-out Stk)
         (struct-out Hole) hole in-hole

         reduction-relation
         apply-reduction-relation*

         define-helpers define-runner

         run-ex run-examples run-all-examples)


;; ----------------------------------------
;; stx utils

;; zip : stl stl ctx -> stl
(define (zip stl_1 stl_2 ctx)
  (match* (stl_1 stl_2)
    [('() '()) '()]
    [((cons stx_left stl_lefts) (cons stx_right stl_rights))
     (cons (Stx `(,stx_left ,stx_right) ctx)
           (zip stl_lefts stl_rights ctx))]))

;; unzip : stl -> (values stl stl)
(define (unzip stl)
  (match stl
    ['() (values '() '())]
    [`(,(Stx `[,stx_left ,stx_right] _) ,stl_rest ...)
     (let-values ([(stl_lefts stl_rights) (unzip stl_rest)])
       (values (cons stx_left  stl_lefts)
               (cons stx_right stl_rights)))]))

(define (snoc stl stx)
  (if (null? stl)
      (list stx)
      (cons (car stl) (snoc (cdr stl) stx))))

;;;; Structures

(struct Stx (e ctx)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Stx)
      (lambda (obj) (list (Stx-e obj) (Stx-ctx obj)))))])

(struct Sym (nam)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Sym)
      (lambda (obj) (list (Sym-nam obj)))))])

(struct 𝓁 (id)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) '𝓁)
      (lambda (obj) (list (𝓁-id obj)))))])

(struct Defs (scp 𝓁)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Defs)
      (lambda (obj) (list (Defs-scp obj) (Defs-𝓁 obj)))))])

(define primitives (set 'syntax-e
                        'datum->syntax
                        '+ '- '* '/ '< '= 'eq?
                        'cons 'car 'cdr 'list 'second 'third 'fourth
                        'syntax-local-value 'local-expand
                        'syntax-local-identifier-as-binding
                        'box 'unbox 'set-box!
                        'syntax-local-make-definition-context
                        'syntax-local-bind-syntaxes))

(define (id? x)
  (match x
    [(Stx (Sym (? symbol?)) _) #t]
    [_ #f]))
(define (val? x)
  (match x
    [`(Fun (,_ ...) ,_ ,_) #t]
    [(? atom?) #t]
    [(cons (? val?) (? val?)) #t]
    [(Stx _ _) #t]
    [_ #f]))
(define (ser? x) (not (val? x)))
(define (atom? x)
  (or (null? x)
      (Sym? x)
      (prim? x)
      (number? x)
      (boolean? x)
      (𝓁? x)
      (Defs? x)))
(define (prim? x) (set-member? primitives x))


;; eval-time closure (in state)
(struct Clo (ast env)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Clo)
      (lambda (obj) (list (Clo-ast obj) (Clo-env obj)))))])

;; eval-time store
;;     table: loc ↦ u
(struct Heap (size table)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Heap)
      (lambda (obj) (list (Heap-size obj) (Heap-table obj)))))])

;; expand-time continuation
(struct κ (STX ex? 𝓁)
  #:constructor-name mk-κ
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'κ)
      (lambda (obj) (list (κ-STX obj) (κ-ex? obj) (κ-𝓁 obj)))))])

;; expand-time store
;;     binds: (nam ∪ 𝓁) ↦ (Set (StoBind scps nam) ...) ∪ val ∪ ξ
(struct Sto (size binds)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Sto)
      (lambda (obj) (list (Sto-size obj) (Sto-binds obj)))))])

(struct StoBind (scps nam)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'StoBind)
      (lambda (obj) (list (StoBind-scps obj) (StoBind-nam obj)))))])

;; expand-time stack (continuation)
(struct Stk (size frames)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Stk)
      (lambda (obj) (list (Stk-size obj) (Stk-frames obj)))))])

;; substitution

(struct Hole ())
(define hole (Hole))

(define (in-hole C v)
  (match C
    ['() '()]
    [(cons hd tl) (cons (in-hole hd v) (in-hole tl v))]
    [(Stx e ctx) (Stx (in-hole e v) ctx)]
    [(Hole) v]
    [_ C]))


;;;; non-deterministic reduction engine

;; (reduction-relation clause ...) : state -> (Set state ...)
(define-syntax (reduction-relation stx)
  (syntax-case stx (reduction-relation)
    [(reduction-relation) #'(λ (s) (set))]
    [(reduction-relation [pat #:when guard body ...] clauses ...)
     #'(λ (s)
         (let ([nexts ((reduction-relation clauses ...) s)])
           (match s
             [pat
              (if guard
                  (set-add nexts (begin body ...))
                  nexts)]
             [_ nexts])))]
    [(reduction-relation [pat #:with expr kont] clauses ...)
     #'(λ (s)
         (let ([nexts ((reduction-relation clauses ...) s)])
           (match s
             [pat (set-union nexts
                             (for/set ([alt (in-set expr)])
                               (kont alt)))]
             [_ nexts])))]
    [(reduction-relation [pat body ...] clauses ...)
     #'(λ (s)
         (let ([nexts ((reduction-relation clauses ...) s)])
           (match s
             [pat (set-add nexts (begin body ...))]
             [_ nexts])))]))

;; apply-reduction-relation*: rel state -> (state ...)
(define (apply-reduction-relation* --> s #:steps [steps #f])
  (let ([all-states (mutable-set)]
        [normal-forms (mutable-set)]
        [worklist (make-queue)])
    (define (loop steps)
      (unless (or (queue-empty? worklist)
                  (and steps (<= steps 0)))
        (let* ([s (dequeue! worklist)]
               [nexts (--> s)])
          (if (set-empty? nexts)
              (set-add! normal-forms s)
              (for ([next (in-set nexts)]
                #:when (not (set-member? all-states next)))
                (set-add! all-states next)
                (enqueue! worklist next))))
        (loop (and steps (sub1 steps)))))
    (enqueue! worklist s)
    (loop steps)
    (if steps
        (set->list all-states) ;; for debug
        (set->list normal-forms))))


;;;; reader & printer

(define-syntax-rule (define-helpers empty-ctx reader printer)
  (...
   (begin
     ;; reader : any -> stx
     (define (reader any)
       (match any
         [(? symbol? (? (λ (s) (not (set-member? primitives s)))))
          (Stx (Sym any) empty-ctx)]

         [(list any_1 any_2 ...)
          (let ([stx (reader any_2)])
            (Stx (cons (reader any_1) (Stx-e stx)) empty-ctx))]

         [else (Stx any empty-ctx)]))

     ;; printer : val -> any
     (define (printer val)
       (match val
         [`(Fun (,var ...) ,ast) `(Fun (,@var) ,ast)]
         [`(Fun (,var ...) ,ast ,env) `(Fun (,@var) ,ast)]
         [(Sym nam) nam]
         [(Stx val _) (vector 'Stx (printer val))]
         [(cons val_1 val_2) (cons (printer val_1) (printer val_2))]
         [atom atom])))))

;;;; runner

(define-syntax-rule (define-runner run reader expander stripper printer
                      evaluate parser)
  (define (run form mode)
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
                                (error 'run "unknown mode: ~e" mode))))))))))))

;;;; Example runner

(define fail-count (make-parameter #f))

(define (ex-runner run example mode)
  (printf "~a: " (car example))
  (println
   (case mode
     [(raw) (r:eval (cadr example))]
     [(check)
      (fail-count (or (fail-count) 0))
      (let ([result (equal? (run (cadr example) 'eval)
                            (r:eval (cadr example)))])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]
     [else (run (cadr example) mode)])))

(define (run-ex run examples name [mode 'check])
  (let ([example (assoc name examples)])
    (ex-runner run example mode)))

(define (run-examples run examples [mode 'check])
  (for ([example (in-list examples)])
    (ex-runner run example mode)))

(define ((run-all-examples all-runs all-examples) [mode 'check])
  (parameterize ([fail-count (if (eq? mode 'check) 0 #f)])
    (for ([run (in-list all-runs)]
          [idx (in-naturals 1)])
      (printf "[~a]\n" (car run))
      (for ([i (in-range idx)]
            [examples (in-list all-examples)])
        (run-examples (cadr run) examples mode)))
    (when (fail-count)
      (printf "\nfail-count: ~a\n" (fail-count)))))
