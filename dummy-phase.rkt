#lang racket
(require
 (only-in "signatures.rkt" phase^))
(provide phase@)

(define-unit phase@
  (import) (export phase^)
  (define (at-phase . args)   (error "must not be used"))
  (define (prune . args)      (error "must not be used"))
  (define (update-ctx . args) (error "must not be used")))
