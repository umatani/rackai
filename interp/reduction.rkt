#lang racket
(require "queue.rkt"
         (for-syntax racket syntax/id-table))
(provide define-reduction-relation
         define-extended-reduction-relation
         define-parameterized-reduction-relation
         define-parameterized-extended-reduction-relation
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
    (let loop ([body #'(set)]
               [cs clauses])
      (if (null? cs)
          body                                
          (loop
           (syntax-case (car cs) ()
             [(p b rule-name)
              #`(let ([n #,body])
                  (match #,s
                    [p ;(println 'rule-name)
                       (set-add n b)]
                    [_ n]))]
             [(p #:when t b rule-name)
              #`(let ([n #,body])
                  (match #,s
                    [p (if t
                           (begin ;(println 'rule-name)
                                  (set-add n b))
                           n)]
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
     #'(define-parameterized-extended-reduction-relation
         rel super-rel () clause ...)]))

(define-syntax (define-parameterized-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel (param ...) clause ...)
     #'(define-parameterized-extended-reduction-relation
         rel #f (param ...) clause ...)]))

(define-syntax (define-parameterized-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel super-rel (param ...) clause ...)
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
             (define rel-f 
               (let ([f (lambda (param ...)
                          (λ (s)
                            #,(make-reducer-body
                               #'s
                               (hash-values
                                (dict-ref #%reduction->clause-map #'rel)))))])
                 #,(if (null? (syntax->list #'(param ...)))
                       #'(f)
                       #'f)))
             (define-syntax rel (reduction-desc #'rel-f)))))]))


(define (apply-reduction-relation* --> s #:steps [steps #f])
  (let ([all-states (make-hash)]
        [normal-forms (make-hash)]
        [worklist (make-queue)])
    (define (loop steps)
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

(define-parameterized-reduction-relation -->test
  (inc)
  [x
   (inc x)
   hoge])
