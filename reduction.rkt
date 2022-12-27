#lang racket
(require racket/provide-syntax racket/require-syntax
         "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket racket/syntax
                     syntax/parse syntax/stx syntax/strip-context
                     racket/unit-exptime))
(provide (all-defined-out)
         (all-from-out "nondet.rkt")
         (for-syntax (all-defined-out)))

;;;; non-deterministic reduction engine

(define-signature red^
  (reducer ;; Param ... -> State -> (Setof State)
   ))

(begin-for-syntax
  (struct reduction-desc
    (unit-id
     params
     super-id super-args
     within-sig-ids
     do-bodies
     clause-map) #:transparent)

  ;;; clause map operations
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
  (define (get-clause-body clause-map rule-name)
    (syntax-parse (cdr (assoc rule-name clause-map))
      [(body ...+ rule-name:id)
       #'(body ...)]))
  (define (clause-map-filter pred clause-map)
    (filter (λ (p) (pred (car p) (cdr p))) clause-map))


  (define (make-match-body bs)
    (syntax-parse bs
      [(b) #`((pure b))]
      [(#:when t b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #`(#:abort-if (not t) #f b2 ...))]
      [(#:with x assign-id:assign e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(x assign-id e b2 ...))]
      [(#:with x elem-id:elem e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #`(x elem-id e b2 ...))]
      [(b1 b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(b1 b2 ...))]))

  (define (make-reducer-body ctx red-desc s maybe-args sub-clause-names)
    (define (stx-rescope stx)
      (datum->syntax ctx (if (syntax? stx)
                             (syntax->datum stx)
                             stx)))

    (define body (let ([super-id (reduction-desc-super-id red-desc)]
                       [super-args (reduction-desc-super-args red-desc)]
                       [clause-map (reduction-desc-clause-map red-desc)])
                   (if (syntax->datum super-id) ;; not #f
                       (make-reducer-body
                        ctx ;; super??
                        (syntax-local-value super-id)
                        s
                        super-args
                        (append sub-clause-names
                                (clause-map-rule-names clause-map)))
                       #'(set))))
    (let* ([args (or maybe-args #'())]
           [params (if maybe-args
                       (reduction-desc-params red-desc)
                       #'())]
           [clause-map (reduction-desc-clause-map red-desc)])
      (unless (= (length (syntax->list params))
                 (length (syntax->list args)))
        (raise-syntax-error
         'define-reduction-relation
         (format "red args arity mismatch: ~a ~a"
                 (syntax->datum params)
                 (syntax->datum args))))
      (with-syntax ([(param ...) (stx-rescope params)]
                    [(arg ...)   (stx-rescope args)])
        #`(let-syntax ([param (make-rename-transformer #'arg)]
                       ...)
            #,(for/fold ([body body])
                        ([clause (in-list (clause-map-clauses
                                           (clause-map-filter
                                            (λ (k v)
                                              (not (member k sub-clause-names)))
                                            clause-map)))])
                (syntax-case (stx-rescope clause) ()
                  [(p b ... rule-name)
                   #`(let ([nexts #,body])
                       (match #,s
                         [p (set-union
                             nexts
                             (results (do #,@(make-match-body #'(b ...)))))]
                         [_ nexts]))]))))))

  (define-syntax-class red-spec
    (pattern name:id
             #:with params #'())
    (pattern (name:id param:id ...)
             #:with params #'(param ...)))

  (define-splicing-syntax-class options-spec
    (pattern (~seq (~alt (~optional (~seq #:super s:red-spec)
                                    #:name "#:super option")
                         (~optional (~seq #:within-signatures [sig-spec ...])
                                    #:name "#:within-units option")
                         (~optional (~seq #:do [body ...])
                                    #:name "#:do option"))
                   ...)
             #:with name #'(~? s.name #f)
             #:with args #'(~? s.params ())
             #:with sigs #'(~? (sig-spec ...) ())
             #:with do-bodies #'(~? (body ...) ())))

  (define (expand-do-bodies do-bodies sub-do-ids def-cxt)
    (define (check-duplicate/sub ids)
      (check-duplicates (append (syntax->datum ids) sub-do-ids)))
    (define (splice-binding-identifier id)
      (internal-definition-context-splice-binding-identifier def-cxt id))
    (define-values (ids* def-vals* def-stxes* exprs*)
      (let loop ([ids*       '()]
                 [def-vals*  '()]
                 [def-stxes* '()]
                 [exprs*     '()]
                 [bodies (syntax->list do-bodies)])
        (if (null? bodies)
            (values ids* def-vals* def-stxes* exprs*)
            (let* ([body (car bodies)]
                   [body* (local-expand body '()
                                        (list #'define-values #'define-syntaxes)
                                        def-cxt)])
              ;(printf "do: ~a\n" (syntax->datum body*))
              (syntax-parse body*
                #:literal-sets (kernel-literals)
                [(begin body ...)
                 (loop ids* def-vals* def-stxes* exprs*
                       (append (syntax->list #'(body ...)) (cdr bodies)))]
                [(define-values (id:id ...) e:expr)
                 #:with (id* ...) (stx-map splice-binding-identifier #'(id ...))
                 (if (check-duplicate/sub #'(id* ...))
                     (loop ids* def-vals* def-stxes* exprs* (cdr bodies))
                     (begin
                       (syntax-local-bind-syntaxes
                        (syntax->list #'(id* ...)) #f def-cxt)
                       (with-syntax
                         ([e* (internal-definition-context-introduce
                               def-cxt
                               (local-expand #'e '()
                                             (list #'define-values
                                                   #'define-syntaxes
                                                   #'define-match-expander)
                                             def-cxt))])
                         (loop (append ids* (syntax->list #'(id* ...)))
                               (cons body #;#'(define-values (id* ...) e*)
                                     def-vals*)
                               def-stxes*
                               exprs*
                               (cdr bodies)))))]
                [(define-syntaxes (id:id ...) e:expr)
                 #:with (id* ...) (stx-map splice-binding-identifier #'(id ...))
                 (if (check-duplicate/sub #'(id* ...))
                     (loop ids* def-vals* def-stxes* exprs* (cdr bodies))
                     (begin
                       (with-syntax
                         ([e* (internal-definition-context-introduce
                               def-cxt
                               (local-expand #'e '()
                                             (list #'define-values
                                                   #'define-syntaxes
                                                   #'define-match-expander)
                                             def-cxt))])
                         (syntax-local-bind-syntaxes
                          (syntax->list #'(id* ...)) #'e* def-cxt)
                         (loop (append ids* (syntax->list #'(id* ...)))
                               def-vals*
                               (cons body #;#'(define-syntaxes (id* ...) e*)
                                     def-stxes*)
                               exprs*
                               (cdr bodies)))))]
                [else (loop ids*
                            def-vals*
                            def-stxes*
                            (cons body #;body*
                                  exprs*)
                            (cdr bodies))])))))
    (define check (check-duplicate-identifier ids*))
    (if check
        (raise-syntax-error 'define-reduction
                            (format "duplicate definitions: ~a"
                                    (syntax->datum check)))
        #`(#,ids* #,def-vals* #,def-stxes* #,exprs*)))

  (define (expand-all-do-bodies do-bodies super-red-id sub-do-ids def-cxt)
    (syntax-parse (expand-do-bodies do-bodies sub-do-ids def-cxt)
      [((id* ...) (def-val* ...) (def-stx* ...) (expr* ...))
       (if (syntax->datum super-red-id) ;; not #f
           (let* ([super-desc (syntax-local-value super-red-id)]
                  [super-do-bodies (reduction-desc-do-bodies super-desc)])
             (with-syntax
               ([((def-val2* ...) (def-stx2* ...) (expr2* ...))
                 (expand-all-do-bodies
                  super-do-bodies
                  (reduction-desc-super-id super-desc)
                  (syntax->datum #`(id* ... #,@sub-do-ids))
                  def-cxt)])
               #'((def-val2* ... def-val* ...)
                  (def-stx2* ... def-stx* ...)
                  (expr2* ... expr* ...))))
           #'((def-val* ...) (def-stx* ...) (expr* ...)))])))


(define-syntax (define-reduction stx)
  (syntax-parse stx
    [(_ r:red-spec opts:options-spec
        (~and [pat body ...+ name] clause) ...)
     #:with red-id #'r.name
     #:with red-unit-id (format-id #'red-id "~a@" #'red-id)
     #:with super-red-id #'opts.name
     #:with (param ...) #'r.params
     #:with (arg ...)   #'opts.args
     #:with (within-signature ...) #'opts.sigs
     #:with (within-sig-id ...) (stx-map
                                 (λ (sig)
                                   (syntax-parse sig
                                     [sig-id:id #'sig-id]
                                     [(only sig-id:id :id ...) #'sig-id]))
                                 #'(within-signature ...))
     #:with (do-body ...) #'opts.do-bodies
     #:with ((def-val* ...) (def-stx* ...) (expr* ...))
     (expand-all-do-bodies #'(do-body ...) #'super-red-id
                           '() (syntax-local-make-definition-context))
     (define result-stx
       #`(begin
           (define-syntax red-id
             (reduction-desc
              #'red-unit-id
              #'(param ...)
              #'super-red-id
              #'(arg ...)
              #'(within-sig-id ...)
              #'(do-body ...)
              (make-clause-map (list #'((... ...) clause) ...))))
           (define-unit red-unit-id
             (import within-signature ...)
             (export red^)

             #,@(datum->syntax #'red-unit-id (syntax->datum #'(def-val* ...)))
             #,@(datum->syntax #'red-unit-id (syntax->datum #'(def-stx* ...)))
             #,@(datum->syntax #'red-unit-id (syntax->datum #'(expr* ...)))

             (define-signature M^
               ((define-values (-->) (#%reducer))
                (define-syntaxes (#%reducer)
                  (λ (stx)
                    #`(λ (param ...)
                        (λ (s)
                          #,(make-reducer-body #'red-id
                                               (syntax-local-value #'red-id)
                                               #'s #f '())))))))
             (define-unit M@ (import) (export M^))

             (define reducer (invoke-unit
                              (compound-unit
                               (import) (export)
                               (link (([m : M^]) M@)
                                     (() (unit (import M^) (export)
                                           -->) m)))))
             reducer)))
     ;(pretty-print (syntax->datum result-stx))
     result-stx]))

(define-syntax (reduction->unit stx)
  (syntax-parse stx
    [(_ red-id:id)
     (reduction-desc-unit-id (syntax-local-value #'red-id))]))

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
                      (λ (impts _b) (map pair->tagged-sig-id impts))))

  (define (unit-static-exports unit-id err-syntax)
    (call-with-values (λ () (unit-static-signatures unit-id err-syntax))
                      (λ (_a expts) (map pair->tagged-sig-id expts)))))

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

;; (reducer-of red #:within-units [unit-id ...]) : State -> (Setof State)
(define-syntax (reducer-of stx)
  (syntax-parse stx
    [(_ red-id:id)
     #'(reducer-of red-id #:within-units [])]
    [(_ red-id:id #:within-units [unit-id:id ...])
     #:with red-unit-id (reduction-desc-unit-id (syntax-local-value #'red-id))
     #:with (within-sig-id ...) (reduction-desc-within-sig-ids
                                 (syntax-local-value #'red-id))
     #:with (link-id ...) (stx-map (λ (sig-id)
                                     (generate-temporary sig-id))
                                   #'(within-sig-id ...))
     #:with ((i-id ...) ...) (stx-map (λ (uid) (unit-static-imports uid stx))
                                      #'(unit-id ...))
     #:with ((i-link ...) ...) (stx-map (λ (i-ids)
                                          (stx-map generate-temporary i-ids))
                                        #'((i-id ...) ...))
     #'(invoke-unit (compound-unit
                     (import [i-link : i-id] ... ...) (export)
                     (link (([link-id : within-sig-id] ...)
                            (compose-unit unit-id ...)
                            i-link ... ...)
                           (() red-unit-id link-id ...)))
                    (import i-id ... ...))]))


;(: apply-reduction-relation* (∀ [A] (->* ((-> A (Setof A)) A)
;                                          (#:steps (Option Natural))
;                                          (Setof A))))
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
    (set-add! all-states s)
    (enqueue! worklist s)
    (loop steps)
    (if steps all-states normal-forms)))
