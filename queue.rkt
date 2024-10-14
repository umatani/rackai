#lang racket
(provide Queueof
         make-queue
         queue-empty?
         enqueue!
         dequeue!)

(struct Queueof (head tail) #:transparent #:mutable
  #:constructor-name Queue)

(define (make-queue) (Queue '() '()))

(define (queue-empty? q)
  (and (null? (Queueof-head q)) (null? (Queueof-tail q))))

(define (enqueue! q a)
  (set-Queueof-tail! q (cons a (Queueof-tail q))))

(define (dequeue! q)
  (when (queue-empty? q)
    (error 'dequeue! "queue is empty"))
  (when (null? (Queueof-head q))
    (set-Queueof-head! q (reverse (Queueof-tail q)))
    (set-Queueof-tail! q '()))
  (begin0
      (car (Queueof-head q))
    (set-Queueof-head! q (cdr (Queueof-head q)))))
