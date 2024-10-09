#lang racket
(require
 (only-in "terms.rkt" lst->list/recur))
(provide (struct-out interpreter)
         apply-interpreter)

(struct interpreter (name run delta α ≤a [result #:mutable]) #:transparent)

(define (apply-interpreter interp form [mode 'check])
  (match-define (interpreter _name run delta _α _≤a _rlt) interp)
  (lst->list/recur (run delta form mode)))
