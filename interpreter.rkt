#lang racket
(require
 (only-in "terms.rkt" lst->list/recur))
(provide (struct-out interpreter)
         make-checking-interpreter
         apply-interpreter)

(struct interpreter (name run delta α ≤a [result #:mutable]) #:transparent)

(define (make-checking-interpreter name run delta α ≤a)
  (interpreter name run delta α ≤a
               (make-hasheq
                '([exact . 0] [inexact . 0] [unsound . 0] [fail . 0]))))

(define (apply-interpreter interp form [mode 'check])
  (match-define (interpreter _name run delta _α _≤a _rlt) interp)
  (lst->list/recur (run delta form mode)))
