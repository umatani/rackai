#lang racket
(require "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket racket/hash
                     syntax/parse))
(provide (all-defined-out)
         (all-from-out "nondet.rkt"))

;;;; non-deterministic reduction engine

(begin-for-syntax

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

  (struct reduction-desc (reducer-id
                          params
                          super-rel super-args
                          clause-map) #:transparent)

  (define (make-clause-map clauses)
    (define names (map (compose1 last syntax->datum) clauses))
    (when (check-duplicates names eq?)
      (raise-syntax-error #f "duplicate reduction names" names))
    (for/fold ([map    (hasheq)])
              ([name   (in-list names)]
               [clause (in-list clauses)])
      (hash-set map name clause)))

  (define (make-reducer-body rel s super-rel super-args clause-map)
    (define (syntax-rescope stx)
      (datum->syntax rel (if (syntax? stx)
                             (syntax->datum stx)
                             stx)))

    (define super-desc (and (syntax->datum super-rel) ;; not #f
                            (syntax-local-value super-rel)))
    (define super-params (if super-desc
                             (reduction-desc-params super-desc)
                             #'()))
    (define super-clause-map (if super-desc
                                 (reduction-desc-clause-map super-desc)
                                 (hasheq)))
    (unless (= (length (syntax->list super-params))
               (length (syntax->list super-args)))
      (raise-syntax-error
       'define-parameterized-extended-reduction-relation
       "super args arity mismatch" #'super-rel))
    
    (let loop
        ([cs (hash-values clause-map)]
         [body
          (with-syntax ([(super-param ...) (syntax-rescope super-params)]
                        [(super-arg ...)   (syntax-rescope super-args)])

            #`(let-syntax ([super-param (make-rename-transformer #'super-arg)]
                           ...)
                #,(let sloop ([sbody #'(set)]
                              [scs (for/list
                                       ([(k v) (in-hash super-clause-map)]
                                        #:when (not (hash-has-key?
                                                     clause-map k)))
                                     v)])
                    (if (null? scs)
                        sbody
                        (sloop
                         (syntax-case (syntax-rescope (car scs)) ()
                           [(p b ... rule-name)
                            #`(let ([nexts #,sbody])
                                (match #,s
                                  [p (set-union
                                      nexts
                                      (car (do #,@(make-match-body #'(b ...)))))]
                                  [_ nexts]))])
                         (cdr scs))))))])
      (if (null? cs)
          body
          (loop
           (cdr cs)
           (syntax-case (syntax-rescope (car cs)) ()
             [(p b ... rule-name)
              (let* ([match-body (make-match-body #'(b ...))])
                #`(let ([nexts #,body])
                    (match #,s
                      [p (set-union
                          nexts
                          (car (do #,@match-body)))]
                      [_ nexts])))]))))))

(define-syntax (define-parameterized-extended-reduction-relation stx)
  (syntax-case stx ()
    [(_ (rel param ...) (super-rel arg ...) clause ...)
     (let ([clause-map (make-clause-map (syntax->list #'(clause ...)))])
       (with-syntax ([(rel-f) (generate-temporaries #'(rel))])
         #`(begin
             (define-syntax rel
               (reduction-desc #'rel-f #'(param ...)
                               #'super-rel #'(arg ...) #,clause-map))
             (define (rel-f param ...) 
               (λ (s)
                 #,(make-reducer-body
                    #'rel #'s #'super-rel #'(arg ...)
                    clause-map))))))]))

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
