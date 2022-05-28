#lang racket
(require "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket syntax/id-table))
(provide (all-defined-out))


;;;; non-deterministic reduction engine

(begin-for-syntax
  (struct reduction-desc (reducer-id clause-map) #:transparent
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
      (hash-set m name (syntax->datum clause))))

  (define (make-match-body bs)
    (syntax-case bs (<- :=)
      [(b) #'((pure b))]
      [(#:when t b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(#:failif (not t) #f b2 ...))]
      [(#:with x := e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(x := e b2 ...))]
      [(#:with x <- e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(x <- e b2 ...))]
      [(b1 b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(b1 b2 ...))]))

  (define (make-reducer-body rel s clauses)
    (let loop ([body #'(set)]
               [cs clauses])
      (if (null? cs)
          body
          (loop
           (syntax-case (datum->syntax rel (car cs)) ()
             [(p b ... rule-name)
              #`(let ([n #,body])
                  (match #,s
                    [p (set-union
                        n
                        (car (do #,@(make-match-body #'(b ...)))))]
                    [_ n]))])
           (cdr cs))))))

(define-syntax (define-parameterized-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel super-rel (param ...) clause ...)
     (with-syntax ([(rel-f) (generate-temporaries #'(rel))])
       (let* ([super-clause-map
               (and (syntax->datum #'super-rel)  ;; not #f
                    (reduction-desc-clause-map
                     (syntax-local-value #'super-rel)))]
              [rel-dict-map (make-clause-map
                             super-clause-map
                             (syntax->list #'(clause ...)))])
         #`(begin
             (define-syntax rel
               (reduction-desc #'rel-f #,rel-dict-map))
             (define rel-f 
               (let ([f (λ (param ...)
                          (λ (s)
                            #,(make-reducer-body #'rel
                               #'s (hash-values rel-dict-map))))])
                 #,(if (null? (syntax->list #'(param ...)))
                       #'(f)
                       #'f))))))]))

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

#;(: apply-reduction-relation* (∀ [A] (->* ((-> A (Setof A)) A)
                                          (#:steps (Option Natural))
                                          (Setof A))))
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
