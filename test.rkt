#lang racket
(require
 "example.rkt"
 (only-in "conc/base/core/main.rkt"   [run base:core:run])
 (only-in "conc/base/phases/main.rkt" [run base:phases:run])
 (only-in "conc/base/full/main.rkt"   [run base:full:run])
 (only-in "conc/set/core/main.rkt"    [run set:core:run])
 (only-in "conc/set/phases/main.rkt"  [run set:phases:run])
 (only-in "conc/set/full/main.rkt"    [run set:full:run])

 ;; (only-in "abs/naive/core.rkt" [run naive:core:run])
 ;; (only-in "abs/naive/phases.rkt" [run naive:phases:run])
 ;; (only-in "abs/naive/full.rkt" [run naive:full:run])
 )

(define main
  (let ([all-runs `([base:core   1 ,base:core:run]
                    [base:phases 2 ,base:phases:run]
                    [base:full   3 ,base:full:run]
                    [set:core    1 ,set:core:run]
                    [set:phases  2 ,set:phases:run]
                    [set:full    3 ,set:full:run]

                    ;; [naive:core           1 ,naive:core:run]
                    ;; [naive:phases         2 ,naive:phases:run]
                    ;; [naive:full           3 ,naive:full:run]
                    )]
        [all-examples (list
                       core:examples
                       phases:examples
                       (append local:examples defs:examples))])
    (run-all-examples all-runs all-examples)))
