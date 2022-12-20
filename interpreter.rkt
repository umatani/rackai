#lang racket
(provide (struct-out interpreter))

(struct interpreter (name run delta α ≤a [result #:mutable]) #:transparent)
