#lang typed/racket
(require "queue.rkt"
         (for-syntax racket syntax/id-table))
(provide define-reduction-relation
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

  (define (make-clause-table clauses)
    (define names (map (λ (c) (syntax->datum (last (syntax->list c))))
                       clauses))
    (when (check-duplicates names)
      (raise-syntax-error 'define-reduction-relation
                          "duplicate reduction names" names))
    (for/hasheq ([name (in-list names)]
                 [clause (in-list clauses)])
      (values name clause)))

  (define (make-reducer-body s A B clauses)
    (let loop ([body #`(ann (set) (Setof #,A))]
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
                         (set-map e (λ ([alt : #,B]) (k alt)))))]
                    [_ n]))])
           (cdr cs)))))

  (define #%reduction->clause-map (make-free-id-table)))

(define-syntax (define-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel A B clause ...)
     (with-syntax ([(rel-f) (generate-temporaries #'(rel))])
       (dict-set! #%reduction->clause-map
                  #'rel (make-clause-table (syntax->list #'(clause ...))))
       #`(begin
           (define (rel-f [s : A]) : (Setof A)
             #,(make-reducer-body #'s #'A #'B
                                  (hash-values
                                   (dict-ref #%reduction->clause-map #'rel))
                                  #;(syntax->list #'(clause ...))
                                  ))
           (define-syntax rel (reduction-desc #'rel-f))))]))

;; (reducer-of rel) : state -> (Set state ...)
(define-syntax (reducer-of stx)
  (syntax-case stx ()
    [(_ rel)
     (reduction-desc-reducer-id (syntax-local-value #'rel))]))



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
