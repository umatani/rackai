#lang racket
(require (only-in "conc/base/example.rkt"
                  core:examples phases:examples local:examples defs:examples)
         (only-in "conc/base/core/misc.rkt" run-all-examples)

         (only-in "conc/base/core/main.rkt" [run base:core:run])
         (only-in "conc/base/phases/main.rkt" [run base:phases:run])
         (only-in "conc/base/full/main.rkt" [run base:full:run])

         (only-in "conc/base+set-heap/core/main.rkt"
                  [run base+set-heap:core:run])
         (only-in "conc/base+set-heap/phases/main.rkt"
                  [run base+set-heap:phases:run])
         (only-in "conc/base+set-heap/full/main.rkt"
                  [run base+set-heap:full:run])

         ;; (only-in "abs/naive/core.rkt" [run naive:core:run])
         ;; (only-in "abs/naive/phases.rkt" [run naive:phases:run])
         ;; (only-in "abs/naive/full.rkt" [run naive:full:run])

         (for-syntax racket/list))


(define run-all
  (let ([all-runs `([base:core            1 ,base:core:run]
                    [base:phases          2 ,base:phases:run]
                    [base:full            3 ,base:full:run]
                    [base+set-heap:core   1 ,base+set-heap:core:run]
                    [base+set-heap:phases 2 ,base+set-heap:phases:run]
                    [base+set-heap:full   3 ,base+set-heap:full:run]

                    ;; [naive:core           1 ,naive:core:run]
                    ;; [naive:phases         2 ,naive:phases:run]
                    ;; [naive:full           3 ,naive:full:run]
                    )]
        [all-examples (list
                       core:examples
                       phases:examples
                       (append local:examples defs:examples))])
    (run-all-examples all-runs all-examples)))
