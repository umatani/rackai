#lang racket
(require racket/provide-syntax racket/require-syntax
         "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket racket/syntax
                     syntax/parse syntax/stx
                     racket/unit-exptime))
(provide (all-defined-out)
         (all-from-out "nondet.rkt"))

;;;; non-deterministic reduction engine

(define-signature red^
  (reducer ;; Param ... -> State -> (Setof State)
   ))

(begin-for-syntax
  (define (make-clause-map clauses)
    (define names (map (compose1 last syntax->datum) clauses))
    (when (check-duplicates names eq?)
      (raise-syntax-error #f "duplicate reduction names" names))
    (for/fold ([map    (hasheq)])
              ([name   (in-list names)]
               [clause (in-list clauses)])
      (hash-set map name clause)))

  (define (make-match-body bs)
    (syntax-parse bs
      [(b) #`((pure b))]
      [(#:when t b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #`(#:failif (not t) #f b2 ...))]
      [(#:with x assign-id:assign e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(x assign-id e b2 ...))]
      [(#:with x elem-id:elem e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #`(x elem-id e b2 ...))]
      [(b1 b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(b1 b2 ...))]))

  (struct reduction-desc
    (unit-id
     params
     super-desc super-args
     within-signatures
     clause-map) #:transparent)

  (define (make-reducer-body red red-desc s maybe-args sub-clause-names)
    (define (stx-rescope stx)
      (datum->syntax red (if (syntax? stx) (syntax->datum stx) stx)))

    (define body (let ([super-desc (reduction-desc-super-desc red-desc)]
                       [super-args (reduction-desc-super-args red-desc)]
                       [clause-map (reduction-desc-clause-map red-desc)])
                   (if (syntax->datum super-desc) ;; not #f
                       (make-reducer-body
                        red ;; super??
                        (syntax-local-value super-desc)
                        s
                        super-args
                        (append sub-clause-names (hash-keys clause-map)))
                       #'(set))))
    (let* ([args (or maybe-args #'())]
           [params (if maybe-args
                       (reduction-desc-params red-desc)
                       #'())]
           [clause-map (reduction-desc-clause-map red-desc)])
      (unless (= (length (syntax->list params))
                 (length (syntax->list args)))
        (raise-syntax-error
         'define-parameterized-extended-reduction-relation
         (format "red args arity mismatch: ~a ~a"
                 (syntax->datum params)
                 (syntax->datum args))))
      (with-syntax ([(param ...) (stx-rescope params)]
                    [(arg ...)   (stx-rescope args)])
        #`(let-syntax ([param (make-rename-transformer #'arg)]
                       ...)
            #,(for/fold ([body body])
                        ([clause
                          (for/list
                              ([(k v) (in-hash clause-map)]
                               #:when (not (member k sub-clause-names)))
                            v)])
                (syntax-case (stx-rescope clause) ()
                  [(p b ... rule-name)
                   #`(let ([nexts #,body])
                       (match #,s
                         [p (set-union
                             nexts
                             (car (do #,@(make-match-body #'(b ...)))))]
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
                                    #:name "#:within-units option"))
                   ...)
             #:with name #'(~? s.name #f)
             #:with args #'(~? s.params ())
             #:with sigs #'(~? (sig-spec ...) ())))
  )


(define-syntax (define-reduction stx)
  (syntax-parse stx
    [(_ r:red-spec opts:options-spec
        (~and [pat body ...+ name] clause) ...)
     #:with red-id #'r.name
     #:with red-unit-id (generate-temporary #'red-id)
     #;(format-id #'red-id "~a@" #'red-id)
     #:with super-red-id #'opts.name
     #:with (param ...) #'r.params
     #:with (arg ...)   #'opts.args
     #:with (within-sig-id ...) #'opts.sigs
     (let* ([clause-map (make-clause-map (syntax->list #'(clause ...)))])
       #`(begin
           (define-syntax red-id (reduction-desc #'red-unit-id
                                                 #'(param ...)
                                                 #'super-red-id
                                                 #'(arg ...)
                                                 #'(within-sig-id ...)
                                                 #,clause-map))
           (define-unit red-unit-id
             (import within-sig-id ...)
             (export red^)

             (define-signature M^
               ((define-values (-->) (#%reducer))
                (define-syntaxes (#%reducer)
                  (λ (stx) #`(λ (param ...)
                                (λ (s) #,(make-reducer-body
                                           #'red-id
                                           (syntax-local-value #'red-id)
                                           #'s #f '())))))))
             (define-unit M@ (import) (export M^))

             (define reducer (invoke-unit
                              (compound-unit
                               (import) (export)
                               (link (([m : M^]) M@)
                                     (() (unit (import M^) (export)
                                           -->) m)))))
             reducer)))]))

(define-syntax (reduction->unit stx)
  (syntax-parse stx
    [(_ red-id:id) (reduction-desc-unit-id (syntax-local-value #'red-id))]))


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

;; reductionとは直接関係のない unit の便利ユーティリティ
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
     #:with (within-sig-id ...) (reduction-desc-within-signatures
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
    (enqueue! worklist s)
    (loop steps)
    (if steps all-states normal-forms)))
