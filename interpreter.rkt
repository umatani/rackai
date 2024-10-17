#lang racket
(require
 (only-in "terms.rkt" lst->list/recur))
(provide (struct-out interpreter)
         apply-interpreter)

(struct interpreter (name run δ α ≤ₐ [result #:mutable]) #:transparent)

(define (apply-interpreter interp form [mode 'check])
  (match-define (interpreter _name run δ _α _≤ₐ _rlt) interp)
  (lst->list/recur (run δ form mode)))
