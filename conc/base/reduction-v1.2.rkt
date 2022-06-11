#lang racket
(require "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket racket/syntax
                     syntax/parse syntax/stx
                     racket/unit-exptime))
(provide (all-defined-out)
         (all-from-out "nondet.rkt"))

;;;; non-deterministic reduction engine

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

  (struct reduction-desc (params
                          super/meta super-args clause-map) #:transparent)

  (define (make-reducer-body red red-desc s maybe-args sub-clause-names)
    (define (stx-rescope stx)
      (datum->syntax red (if (syntax? stx) (syntax->datum stx) stx)))

    (define body (let ([super/meta (reduction-desc-super/meta red-desc)]
                       [super-args (reduction-desc-super-args red-desc)]
                       [clause-map (reduction-desc-clause-map red-desc)])
                   (if (syntax->datum super/meta) ;; not #f
                       (make-reducer-body
                        red ;; super??
                        (syntax-local-value super/meta)
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
                         (~optional (~seq #:within-signatures [sig-id:id ...])
                                    #:name "#:within-units option"))
                   ...)
             #:with name #'(~? s.name #f)
             #:with args #'(~? s.params ())
             #:with sigs #'(~? (sig-id ...) ())))
  )


(define-syntax (define-reduction stx)
  (syntax-parse stx
    [(_ r:red-spec opts:options-spec
        (~and [pat body ...+ name] clause) ...)
     #:with red #'r.name
     #:with (param ...) #'r.params
     #:with super-red #'opts.name
     #:with (arg ...) #'opts.args
     #:with (sig-id ...) #'opts.sigs
     (with-syntax ([red/meta (format-id #'red "~a/meta" #'red)]
                   [super-red/meta (and (syntax->datum #'super-red)
                                        (format-id #'super-red
                                                   "~a/meta" #'super-red))])
       (let* ([clause-map (make-clause-map (syntax->list #'(clause ...)))])
         #`(define-signature red
             #,@(if (syntax->datum #'super-red)
                    #'(extends super-red)
                    #'())
             ((open sig-id)
              ...
              (define-values (red) (#%reducer))
              (define-syntaxes (red/meta #%reducer)
                (values
                 (reduction-desc #'(param ...)
                                 #'super-red/meta
                                 #'(arg ...) #,clause-map)
                 (λ (stx) #`(λ (param ...)
                               (λ (s) #,(make-reducer-body
                                          #'red
                                          (syntax-local-value #'red/meta)
                                          #'s #f '()))))))))))]))


(begin-for-syntax
  (define (pair->tagged-sig-id p)
    (if (car p)
        #`(tag #,(datum->syntax (cdr p) (car p)) #,(cdr p))
        (cdr p)))

  (define ((prefix-id p) id)
    (format-id id "~a~a" p id))

  (define (unit-static-exports unit-id err-syntax)
    (call-with-values (λ () (unit-static-signatures unit-id err-syntax))
                      (λ (_a expts) (map pair->tagged-sig-id expts))))

  (define (signature-variables sig-id err-syntax)
    (call-with-values (λ () (signature-members sig-id err-syntax))
                      (λ (_a vars _b _c) vars))))

;; (reducer-of red) : state -> (Set state ...)
(define-syntax (reducer-of stx)
  (syntax-case stx ()
    [(_ red)
     #'(reducer-of red #:within-units [])]
    [(_ red #:within-units [unit-id ...])
     (with-syntax*
       ([((sig-id ...) ...)
         (stx-map (λ (unit-id) (unit-static-exports unit-id stx))
                  #'(unit-id ...))]
        [((link-id ...) ...)
         (stx-map (λ (sig-ids) (generate-temporaries sig-ids))
                  #'((sig-id ...) ...))]
        [((pre ...) ...)
         (stx-map (λ (sig-ids) (generate-temporaries sig-ids))
                  #'((sig-id ...) ...))]
        [(((var ...) ...) ...)
         (stx-map (λ (sig-ids)
                    (stx-map (λ (sig-id) (signature-variables sig-id stx))
                             sig-ids))
                  #'((sig-id ...) ...))]
        [((pvar ...) ...)
         (stx-map (λ (pres varss)
                    (append-map (λ (pre vars)
                                  (stx-map (prefix-id pre) vars))
                                (syntax->list pres)
                                (syntax->list varss)))
                  #'((pre ...) ...) #'(((var ...) ...) ...))]
        [((var2 ...) ...)
         (stx-map (λ (varss)
                    (append-map (λ (vars)
                                  (stx-map syntax-local-introduce vars))
                                (syntax->list varss)))
                  #'(((var ...) ...) ...))])
       #`(invoke-unit
          (compound-unit
           (import) (export)
           (link (([link-id : sig-id] ...) unit-id)
                 ...
                 (([r : red]) (unit
                                (import (prefix pre sig-id) ... ...)
                                (export red)
                                (define var2 pvar) ... ...)
                              link-id ... ...)
                 (() (unit (import red) (export) red) r)))))]))


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
