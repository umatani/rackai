#lang racket
(require data/queue)
(provide reduction-relation
         define-reduction-relation
         apply-reduction-relation*)


;;;; non-deterministic reduction engine

;; (reduction-relation clause ...) : state -> (Set state ...)
(define-syntax (reduction-relation stx)
  (syntax-case stx (reduction-relation)
    [(reduction-relation) #'(λ (s) (set))]
    [(reduction-relation [pat #:when guard body ... name] clauses ...)
     #'(λ (s)
         (let ([nexts ((reduction-relation clauses ...) s)])
           (match s
             [pat
              (if guard
                  (set-add nexts (begin body ...))
                  nexts)]
             [_ nexts])))]
    [(reduction-relation [pat #:with expr kont name] clauses ...)
     #'(λ (s)
         (let ([nexts ((reduction-relation clauses ...) s)])
           (match s
             [pat (set-union nexts
                             (for/set ([alt (in-set expr)])
                               (kont alt)))]
             [_ nexts])))]
    [(reduction-relation [pat body ... name] clauses ...)
     #'(λ (s)
         (let ([nexts ((reduction-relation clauses ...) s)])
           (match s
             [pat (set-add nexts (begin body ...))]
             [_ nexts])))]))

;; for use in typed/**
(define-syntax (define-reduction-relation stx)
  (syntax-case stx ()
    [(_ rel A B clause ...)
     #'(define rel (reduction-relation clause ...))]))


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
