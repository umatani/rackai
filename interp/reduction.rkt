#lang racket
(require "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket racket/hash))
(provide (all-defined-out))


;;;; non-deterministic reduction engine

(begin-for-syntax
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

  (struct reduction-desc (reducer-id
                          params
                          clause-map super-clause-map) #:transparent)

  (define (make-clause-map clauses)
    (define names (map last clauses))
    (when (check-duplicates names)
      (raise-syntax-error #f "duplicate reduction names" names))
    (for/fold ([m (hasheq)])
              ([name (in-list names)]
               [clause (in-list clauses)])
      (hash-set m name clause)))

  (define (make-reducer-body rel s super-params super-args
                             clause-map super-clause-map)
    (let loop
        ([body
          (with-syntax ([(super-param ...) (datum->syntax rel super-params)]
                        [(super-arg ...) super-args])
            (let sloop ([sbody #'(set)]
                        [scs
                         (for/list
                             ([(k v) (in-hash super-clause-map)]
                              #:when (not (hash-has-key? clause-map k)))
                           v)])
              (if (null? scs)
                  sbody
                  (sloop
                   (syntax-case (datum->syntax rel (car scs)) ()
                     [(p b ... rule-name)
                      #`(let ([nexts #,sbody])
                          (let-syntax ([super-param
                                        (make-rename-transformer #'super-arg)]
                                       ...)
                            (match #,s
                              [p (set-union
                                  nexts
                                  (car (do #,@(make-match-body #'(b ...)))))]
                              [_ nexts])))])
                   (cdr scs)))))]
         [cs (hash-values clause-map)])
      (if (null? cs)
          body
          (loop
           (syntax-case (datum->syntax rel (car cs)) ()
             [(p b ... rule-name)
              #`(let ([nexts #,body])
                  (match #,s
                    [p (set-union
                        nexts
                        (car (do #,@(make-match-body #'(b ...)))))]
                    [_ nexts]))])
           (cdr cs))))))

(define-syntax (define-parameterized-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ (rel param ...) (super-rel arg ...) clause ...)
     (let* ([super-desc (and (syntax->datum #'super-rel)  ;; not #f
                             (syntax-local-value #'super-rel))]
            [super-params (if super-desc
                              (syntax->datum (reduction-desc-params super-desc))
                              '())]
            [_ (unless (= (length super-params)
                          (length (syntax->list #'(arg ...))))
                 (raise-syntax-error
                  'define-parameterized-extended-reduction-relation
                  "super args arity mismatch" #'super-rel))]
            [clause-map (make-clause-map (syntax->datum #'(clause ...)))]
            [super-clause-map
             (if super-desc
                 (hash-union (reduction-desc-super-clause-map super-desc)
                             (reduction-desc-clause-map super-desc)
                             #:combine (λ (sc c) c))
                 (hasheq))])
       (with-syntax ([(rel-f) (generate-temporaries #'(rel))])
         #`(begin
             (define-syntax rel
               (reduction-desc #'rel-f #'(param ...)
                               #,clause-map #,super-clause-map))
             (define (rel-f param ...) 
               (λ (s)
                 #,(make-reducer-body
                    #'rel #'s super-params #'(arg ...)
                    clause-map super-clause-map))))))]))

(define-syntax (define-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel super-rel clause ...)
     #'(define-parameterized-extended-reduction-relation
         (rel) super-rel clause ...)]))

(define-syntax (define-parameterized-reduction-relation stx)
  (syntax-case stx ()
    [(_ (rel param ...) clause ...)
     #'(define-parameterized-extended-reduction-relation
         (rel param ...) (#f)  clause ...)]))

(define-syntax (define-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel clause ...)
     #'(define-parameterized-reduction-relation (rel) clause ...)]))

;; (reducer-of rel) : state -> (Set state ...)
(define-syntax (reducer-of stx)
  (syntax-case stx ()
    [(_ rel) (reduction-desc-reducer-id (syntax-local-value #'rel))]))


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
