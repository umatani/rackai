#lang typed/racket
(provide Queueof
         make-queue
         queue-empty?
         enqueue!
         dequeue!)

(struct [A] Queueof ([head : (Listof A)] [tail : (Listof A)])
  #:transparent #:mutable
  #:constructor-name Queue)

(: make-queue : (∀ [A] (-> (Queueof A))))
(define (make-queue)
  (Queue '() '()))

(: queue-empty? : (∀ [A] (Queueof A) -> Boolean))
(define (queue-empty? q )
  (and (null? (Queueof-head q)) (null? (Queueof-tail q))))

(: enqueue! : (∀ [A] (Queueof A) A -> Void))
(define (enqueue! q a)
  (set-Queueof-tail! q (cons a (Queueof-tail q))))

(: dequeue! : (∀ [A] (Queueof A) -> A))
(define (dequeue! q)
  (when (queue-empty? q)
    (error 'dequeue! "queue is empty"))
  (when (null? (Queueof-head q))
    (set-Queueof-head! q (reverse (Queueof-tail q)))
    (set-Queueof-tail! q '()))
  (begin0
      (car (Queueof-head q))
    (set-Queueof-head! q (cdr (Queueof-head q)))))
