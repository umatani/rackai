#lang racket
(require
 (prefix-in ss: (only-in "scopeset.rkt"
                         core:examples phases:examples
                         local:examples defs:examples))
 (prefix-in org: (only-in "original.rkt"
                          core:examples phases:examples full:examples
                          finite:examples))

 (only-in "run.rkt" run-examples)
 (for-syntax (only-in racket/list second third fourth)))
(provide suite
         (rename-out [run-examples run-suite])
         (for-syntax (all-from-out racket/list)))

;;;; Examples

(define suites
  (hasheq 'core   (append ss:core:examples   org:core:examples)
          'phases (append ss:phases:examples org:phases:examples)
          'full   (append ss:local:examples  ss:defs:examples org:full:examples)
          'finite (append org:finite:examples)))

(define (suite name) (hash-ref suites name))
