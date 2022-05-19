#lang racket
(require (except-in "../core/conf.rkt"
                    empty-ctx))
(provide (all-defined-out))

(define (empty-ctx) '()) ;; TODO: change to hashtable
