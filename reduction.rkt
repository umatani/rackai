#lang racket/base
(require
 (for-syntax racket racket/syntax racket/unit-exptime
             syntax/parse syntax/stx)
 (only-in racket/match match)
 (prefix-in r: racket/set)
 racket/unit
 (only-in "set.rkt" ∅ ∅? ∪ list→set in-set)
 (only-in "queue.rkt"  make-queue queue-empty? dequeue! enqueue!)
 "nondet.rkt")
(provide (all-defined-out)
         (all-from-out "nondet.rkt")
         (for-syntax (all-defined-out)))

;;;; non-deterministic reduction engine

(define enable-tracing (make-parameter #f))

(define-signature red^
  (reducer ;; Param ... → State → (Setof State)
   ))

(begin-for-syntax
  (struct reduction-desc
    (unit-id
     params
     super-id super-args
     import-sig-ids
     do-bodies
     clause-map) #:transparent)

  ;; clause map operations
  (define (make-clause-map clauses)
    (define names (map (compose1 last syntax->datum) clauses))
    (when (check-duplicates names eq?)
      (raise-syntax-error #f "duplicate reduction names" names))
    (for/list ([name   (in-list names)]
               [clause (in-list clauses)])
      (cons name clause)))
  (define (clause-map-rule-names clause-map)
    (map car clause-map))
  (define (clause-map-clauses clause-map)
    (map cdr clause-map))
  (define (clause-map-find clause-map name)
    (define a (assoc name clause-map))
    (and a (cdr a)))
  (define (clause-map-filter pred clause-map)
    (filter (compose1 pred car) clause-map))

  (define (make-match-body bs)
    (syntax-parse bs
      [() #'()]
      [(b) #'((pure b))]
      [(#:when t b ...)
       (with-syntax ([(b′ ...) (make-match-body #'(b ...))])
         #'(#:when t b′ ...))]
      [(#:abort e b ...)
       (with-syntax ([(b′ ...) (make-match-body #'(b ...))])
         #'(#:abort e b′ ...))]
      [(#:abort-if t e b ...)
       (with-syntax ([(b′ ...) (make-match-body #'(b ...))])
         #'(#:abort-if t e b′ ...))]
      [(#:checkpoint b₀ b ...)
       (with-syntax ([(b′ ...) (make-match-body #'(b ...))])
         #'(#:checkpoint b₀ b′ ...))]
      [(x (~or* ≐:assign ≐:elem) e b ...)
       (with-syntax ([(b′ ...) (make-match-body #'(b ...))])
         #'(x ≐ e b′ ...))]
      [(b₀ b ...)
       (with-syntax ([(b′ ...) (make-match-body #'(b ...))])
         #'(b₀ b′ ...))]))

  (define (make-reducer-body ctx red-desc s maybe-args)
    (define (stx-rescope stx)
      (datum->syntax ctx (if (syntax? stx)
                           (syntax->datum stx)
                           stx)))

    (define (m-r-b red-desc maybe-args sub-clause-names)
      (define clause-map (reduction-desc-clause-map red-desc))
      (define-values (body default-clause)
        (let ([super-id   (reduction-desc-super-id   red-desc)]
              [super-args (reduction-desc-super-args red-desc)])
          (if (syntax->datum super-id) ;; not #f
            (m-r-b (syntax-local-value super-id)
                   super-args
                   (append sub-clause-names (clause-map-rule-names clause-map)))
            (values #'∅ #f))))
      (values
       (let* ([args (or maybe-args #'())]
              [params (if maybe-args
                        (reduction-desc-params red-desc)
                        #'())])
         (unless (= (length (syntax->list params))
                    (length (syntax->list args)))
           (raise-syntax-error
            'define-reduction
            (format "red args arity mismatch: ~a ~a"
                    (syntax->datum params) (syntax->datum args))))
         (with-syntax ([(arg   ...) (stx-rescope args)]
                       [(param ...) (stx-rescope params)])
           #`(let-syntax ([param (make-rename-transformer #'arg)] ...)
               #,(for/fold ([body body])
                           ([clause (in-list
                                     (clause-map-clauses
                                      (clause-map-filter
                                       (λ (name)
                                         (and
                                          (not (member name sub-clause-names))
                                          (not (eq? name '#%default))))
                                       clause-map)))])
                   (syntax-case (stx-rescope clause) ()
                     [(p b ... rule-name)
                      #`(let ([nexts #,body])
                          (match #,s
                            [p (when (enable-tracing)
                                 (printf "→[~a]\n" 'rule-name)) 
                               (∪ nexts
                                  (do #,@(make-match-body #'(b ...))))]
                            [_ nexts]))])))))
       (or (clause-map-find clause-map '#%default) default-clause)))
    
    (define-values (body default-clause) (m-r-b red-desc maybe-args '()))
    (if default-clause
      (syntax-case (stx-rescope default-clause) ()
        [(p b ... _rule-name)
         #`(let ([nexts #,body])
             (if (∅? nexts)
               (match #,s
                 [p (do #,@(make-match-body #'(b ...)))]
                 [_ ∅])
               nexts))])
      body))

  (define-syntax-class red-spec
    (pattern name:id
             #:with params #'())
    (pattern (name:id param:id ...)
             #:with params #'(param ...)))

  (define-splicing-syntax-class options-spec
    (pattern
     (~seq (~alt (~optional (~seq #:super s:red-spec)
                            #:name "#:super option")
                 (~optional (~seq #:import [sig-spec ...])
                            #:name "#:import option")
                 (~optional (~seq #:do [body ...])
                            #:name "#:do option")
                 (~optional (~seq #:default (~and [_pat _body ...+]
                                                  clause))
                            #:name "#:default option"))
           ...)
     #:with name      #'(~? s.name         #f)
     #:with args      #'(~? s.params       ())
     #:with sigs      #'(~? (sig-spec ...) ())
     #:with do-bodies #'(~? (body ...)     ())
     #:with default   #'(~? clause         #f)))

  (define (expand-do-bodies do-bodies def-cxt)
    (define (check-duplicates/sub ids)
      (check-duplicates
       (append (syntax->datum ids)
               (map syntax->datum
                    (internal-definition-context-binding-identifiers def-cxt))))
      #;
      (check-duplicate-identifier
       (append (syntax->list ids)
               (internal-definition-context-binding-identifiers def-cxt))))
    (let loop ([def-vals*  '()]
               [def-stxes* '()]
               [exprs*     '()]
               [bodies (syntax->list do-bodies)])
      (if (null? bodies)
        #`(#,def-vals* #,def-stxes* #,exprs*)
        (let* ([body  (car bodies)]
               [body* (local-expand body '()
                                    (list #'define-values #'define-syntaxes)
                                    def-cxt)])
          (syntax-parse body*
            #:literal-sets (kernel-literals)
            [(begin body ...)
             (loop def-vals* def-stxes* exprs*
                   (append (syntax->list #'(body ...)) (cdr bodies)))]
            [(define-values (id:id ...) e:expr)
             (if (check-duplicates/sub #'(id ...))
               (loop def-vals* def-stxes* exprs* (cdr bodies))
               (begin
                 (syntax-local-bind-syntaxes
                  (syntax->list #'(id ...)) #f def-cxt)
                 (with-syntax
                   ([_e* (internal-definition-context-add-scopes
                          def-cxt
                          (local-expand #'e '()
                                        (list #'define-values
                                              #'define-syntaxes)
                                        def-cxt))])
                   (loop (cons body def-vals*)
                         def-stxes*
                         exprs*
                         (cdr bodies)))))]
            [(define-syntaxes (id:id ...) e:expr)
             (if (check-duplicates/sub #'(id ...))
               (loop def-vals* def-stxes* exprs* (cdr bodies))
               (with-syntax
                 ([e* (internal-definition-context-add-scopes
                       def-cxt
                       (local-expand #'e '()
                                     (list #'define-values
                                           #'define-syntaxes)
                                     def-cxt))])
                 (syntax-local-bind-syntaxes
                  (syntax->list #'(id ...)) #'e* def-cxt)
                 (loop def-vals*
                       (cons body def-stxes*)
                       exprs*
                       (cdr bodies))))]
            [_ (loop def-vals* def-stxes* (cons body exprs*)
                     (cdr bodies))])))))

  (define (expand-all-do-bodies do-bodies super-red-id def-cxt)
    (syntax-parse (expand-do-bodies do-bodies def-cxt)
      [((def-val* ...) (def-stx* ...) (expr* ...))
       (if (syntax->datum super-red-id) ;; not #f
         (let* ([super-desc (syntax-local-value super-red-id)]
                [super-do-bodies (reduction-desc-do-bodies super-desc)])
           (with-syntax
             ([((def-val2* ...) (def-stx2* ...) (expr2* ...))
               (expand-all-do-bodies
                super-do-bodies
                (reduction-desc-super-id super-desc)
                def-cxt)])
             #'((def-val2* ... def-val* ...)
                (def-stx2* ... def-stx* ...)
                (expr2* ... expr* ...))))
         #'((def-val* ...) (def-stx* ...) (expr* ...)))]))
  )


(define-syntax (define-reduction stx)
  (syntax-parse stx
    [(_ r:red-spec opts:options-spec
        (~and [_pat _body ...+ _name] clause) ...)
     #:with red-id                 #'r.name
     #:with (param ...)            #'r.params
     #:with red-unit-id            (format-id #'red-id "~a@" #'red-id)
     #:with super-red-id           #'opts.name
     #:with (arg ...)              #'opts.args
     #:with (do-body ...)          #'opts.do-bodies
     #:with (import-signature ...) #'opts.sigs
     #:with default-clause         #'opts.default
     #:with (clause′ ...) (if (syntax->datum #'default-clause)
                            #`((#,@#'default-clause #%default) clause ...)
                            #'(clause ...))
     #:with (import-sig-id ...)    (stx-map
                                    (λ (sig)
                                      (syntax-parse sig
                                        [sig-id:id #'sig-id]
                                        [(only sig-id:id :id ...) #'sig-id]))
                                    #'(import-signature ...))
     #:with ((def-val* ...) (def-stx* ...) (expr* ...))
     (expand-all-do-bodies #'(do-body ...) #'super-red-id
                           (syntax-local-make-definition-context))
     #`(begin
         (define-syntax red-id
           (reduction-desc #'red-unit-id
                           #'(param ...)
                           #'super-red-id
                           #'(arg ...)
                           #'(import-sig-id ...)
                           #'(do-body ...)
                           (make-clause-map
                            (list
                             #'((... ...) clause′) ...))))
         (define-unit red-unit-id
           (import import-signature ...)
           (export red^)

           #,@(datum->syntax #'red-unit-id (syntax->datum #'(def-val* ...)))
           #,@(datum->syntax #'red-unit-id (syntax->datum #'(def-stx* ...)))
           #,@(datum->syntax #'red-unit-id (syntax->datum #'(expr*    ...)))

           (define-signature M^
             ((define-values (-->) (#%reducer))
              (define-syntaxes (#%reducer)
                (λ (stx)
                  #`(λ (param ...)
                      (λ (s)
                        #,(make-reducer-body #'red-id
                                             (syntax-local-value #'red-id)
                                             #'s #f)))))))
           (define-unit M@ (import) (export M^))

           (define reducer (invoke-unit
                            (compound-unit
                             (import) (export)
                             (link (([m : M^]) M@)
                                   (() (unit (import M^) (export)
                                         -->) m)))))
           reducer))]))

(define-syntax (define-unit-from-reduction stx)
  (syntax-parse stx
    [(_ uid:id red-id:id)
     #`(define-syntax uid (make-rename-transformer
                           #'#,(reduction-desc-unit-id
                                (syntax-local-value #'red-id))))]))

(begin-for-syntax
  (define (pair->tagged-sig-id p)
    (if (car p)
      #`(tag #,(datum->syntax (cdr p) (car p)) #,(cdr p))
      (cdr p)))

  (define (unit-static-imports unit-id err-syntax)
    (call-with-values (λ () (unit-static-signatures unit-id err-syntax))
                      (λ (impts _es) (map pair->tagged-sig-id impts))))

  (define (unit-static-exports unit-id err-syntax)
    (call-with-values (λ () (unit-static-signatures unit-id err-syntax))
                      (λ (_is expts) (map pair->tagged-sig-id expts)))))

;; unit utility
(define-syntax (compose-unit stx)
  (syntax-parse stx
    [(_ unit-id ...)
     #:with ((i-id ...) ...) (stx-map (λ (uid)
                                        (unit-static-imports uid stx))
                                      #'(unit-id ...))
     #:with ((i-link ...) ...) (stx-map (λ (i-ids)
                                          (stx-map generate-temporary i-ids))
                                        #'((i-id ...) ...))
     #:with ((e-id ...) ...) (stx-map (λ (uid)
                                        (unit-static-exports uid stx))
                                      #'(unit-id ...))
     #:with ((e-link ...) ...) (stx-map (λ (e-ids)
                                          (stx-map generate-temporary e-ids))
                                        #'((e-id ...) ...))
     #'(compound-unit
        (import [i-link : i-id] ... ...) (export e-link ... ...)
        (link (([e-link : e-id] ...) unit-id i-link ...)
              ...))]))

;; (reducer-of red #:link [unit-id ...]) : State → (SetM State)
(define-syntax (reducer-of stx)
  (syntax-parse stx
    [(_ red-id:id)
     #'(reducer-of red-id #:link [])]
    [(_ red-id:id #:link [unit-id:id ...])
     #:with red-unit-id (reduction-desc-unit-id (syntax-local-value #'red-id))
     #:with (import-sig-id ...) (reduction-desc-import-sig-ids
                                 (syntax-local-value #'red-id))
     #:with (link-id ...) (stx-map generate-temporary #'(import-sig-id ...))
     #:with ((i-id ...) ...) (stx-map (λ (uid) (unit-static-imports uid stx))
                                      #'(unit-id ...))
     #:with ((i-link ...) ...) (stx-map (λ (i-ids)
                                          (stx-map generate-temporary i-ids))
                                        #'((i-id ...) ...))
     #'(invoke-unit (compound-unit
                     (import [i-link : i-id] ... ...) (export)
                     (link (([link-id : import-sig-id] ...)
                            (compose-unit unit-id ...)
                            i-link ... ...)
                           (() red-unit-id link-id ...)))
                    (import i-id ... ...))]))


;; apply-reduction* : (∀ [A] (A → (SetM A)) A → (SetM A))
(define (apply-reduction* --> s)
  (let ([all-states   (r:mutable-set)]
        [irreducibles (r:mutable-set)]
        [worklist     (make-queue)])
    (define (loop)
      (unless (queue-empty? worklist)
        (let* ([s (dequeue! worklist)]
               [ss (--> s)])
          (if (∅? ss)
            (r:set-add! irreducibles (Right s))
            (let ([nexts (results ss)])
              (for ([msg (in-set (aborts ss))])
                (r:set-add! irreducibles (Left msg)))
              (for ([next (in-set nexts)]
                    #:when (not (r:set-member? all-states next)))
                (r:set-add! all-states next)
                (enqueue! worklist next)))))
        (loop)))
    (r:set-add! all-states s)
    (enqueue! worklist s)
    (loop)
    (list→set (r:set->list irreducibles))))

;; apply-reduction* : (∀ [A] (A → (SetM A)) A → (SetM A))
(define (apply-reduction --> s)
  (define irreducibles (r:mutable-set))
  (define (a-r s)
    (do ss := (--> s)
        (if (∅? ss)
          (r:set-add! irreducibles s)
          (for/m+ ([s′ (in-set ss)])
            (a-r s′)))))
  (a-r s)
  (list→set (r:set->list irreducibles)))
