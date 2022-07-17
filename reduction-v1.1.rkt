#lang racket
(require "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket syntax/parse))
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
                             (results (do #,@(make-match-body #'(b ...)))))]
                         [_ nexts]))])))))))

(define-syntax (define-parameterized-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ (red param ...) (super-red arg ...) clause ...)
     (with-syntax ([red/meta (datum->syntax
                              #'red
                              (string->symbol
                               (format "~a/meta" (syntax->datum
                                                  #'red))))]
                   [super-red/meta (and (syntax->datum #'super-red)
                                        (datum->syntax
                                         #'super-red
                                         (string->symbol
                                          (format "~a/meta" (syntax->datum
                                                             #'super-red)))))])
       (let* ([clause-map (make-clause-map (syntax->list #'(clause ...)))])
         #`(define-signature red
             #,@(if (syntax->datum #'super-red)
                    #'(extends super-red)
                    #'())
             ((define-values (red) (#%reducer))
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

;; (reducer-of red) : state -> (Set state ...)
(define-syntax-rule (reducer-of red)
  (invoke-unit (unit (import red) (export) red) (import red)))



(define-syntax (define-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ red super-red clause ...)
     #'(define-parameterized-extended-reduction-relation
         (red) super-red clause ...)]))

(define-syntax (define-parameterized-reduction-relation stx)
  (syntax-case stx ()
    [(_ (red param ...) clause ...)
     #'(define-parameterized-extended-reduction-relation
         (red param ...) (#f)  clause ...)]))

(define-syntax (define-reduction-relation stx)
  (syntax-case stx ()
    [(_ red clause ...)
     #'(define-parameterized-reduction-relation (red) clause ...)]))


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
