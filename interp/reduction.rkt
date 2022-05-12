#lang racket
(require data/queue
         (for-syntax racket syntax/id-table))
(provide define-reduction-relation
         define-extended-reduction-relation
         reducer-of
         apply-reduction-relation*)


;;;; non-deterministic reduction engine

(begin-for-syntax
  (struct reduction-desc (reducer-id) #:transparent
    #:property prop:procedure
    (λ (self stx) ; syntax transformer at phase 1
      (syntax-case stx ()
        [(_ state)
         #`(#,(reduction-desc-reducer-id self) state)])))

  (define (make-clause-map super-clause-map clauses)
    (define names (map (λ (c) (syntax->datum (last (syntax->list c))))
                       clauses))
    (when (check-duplicates names)
      (raise-syntax-error 'define-reduction-relation
                          "duplicate reduction names" names))
    (for/fold ([m (or super-clause-map (hasheq))])
              ([name (in-list names)]
               [clause (in-list clauses)])
      (hash-set m name clause)))

  (define (make-reducer-body s clauses)
    (let loop ([body #`(set)]
               [cs clauses])
      (if (null? cs)
          body                                
          (loop
           (syntax-case (car cs) ()
             [(p b rule-name)
              #`(let ([n #,body])
                  (match #,s
                    [p (set-add n b)]
                    [_ n]))]
             [(p #:when t b rule-name)
              #`(let ([n #,body])
                  (match #,s
                    [p (if t (set-add n b) n)]
                    [_ n]))]
             [(p #:with e k rule-name)
              #`(let ([n #,body])
                  (match #,s
                    [p (set-union
                        n
                        (list->set
                         (set-map e (λ (alt) (k alt)))))]
                    [_ n]))])
           (cdr cs)))))

  (define #%reduction->clause-map (make-free-id-table)))

;; (reducer-of rel) : state -> (Set state ...)
(define-syntax (reducer-of stx)
  (syntax-case stx ()
    [(_ rel)
     (reduction-desc-reducer-id (syntax-local-value #'rel))]))

(define-syntax (define-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel clause ...)
     #'(define-extended-reduction-relation rel #f clause ...)]))

(define-syntax (define-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel super-rel clause ...)
     (with-syntax ([(rel-f) (generate-temporaries #'(rel))])
       (let ([super-clause-map
              (and (syntax->datum #'super-rel)  ;; not #f
                   (dict-ref #%reduction->clause-map
                             #'super-rel
                             (λ () (raise-syntax-error
                                     'define-extended-reduction-relation
                                     "super relation not found"
                                     #'super-rel))))])
         (dict-set! #%reduction->clause-map
                    #'rel (make-clause-map
                           super-clause-map
                           (syntax->list #'(clause ...))))
         #`(begin
             (define (rel-f s)
               #,(make-reducer-body #'s
                                    (hash-values
                                     (dict-ref #%reduction->clause-map #'rel))))
             (define-syntax rel (reduction-desc #'rel-f)))))]))






;; ;; (reduction-relation clause ...) : state -> (Set state ...)
;; (define-syntax (reduction-relation stx)
;;   (syntax-case stx (reduction-relation)
;;     [(reduction-relation) #'(λ (s) (set))]
;;     [(reduction-relation [pat #:when guard body ... name] clauses ...)
;;      #'(λ (s)
;;          (let ([nexts ((reduction-relation clauses ...) s)])
;;            (match s
;;              [pat
;;               (if guard
;;                   (set-add nexts (begin body ...))
;;                   nexts)]
;;              [_ nexts])))]
;;     [(reduction-relation [pat #:with expr kont name] clauses ...)
;;      #'(λ (s)
;;          (let ([nexts ((reduction-relation clauses ...) s)])
;;            (match s
;;              [pat (set-union nexts
;;                              (for/set ([alt (in-set expr)])
;;                                (kont alt)))]
;;              [_ nexts])))]
;;     [(reduction-relation [pat body ... name] clauses ...)
;;      #'(λ (s)
;;          (let ([nexts ((reduction-relation clauses ...) s)])
;;            (match s
;;              [pat (set-add nexts (begin body ...))]
;;              [_ nexts])))]))




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
