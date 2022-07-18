#lang racket
(require
 "example.rkt"
 (only-in "conc/base/core/main.rkt"
          [run base:core:run] [α base:core:α] [≤a base:core:≤a])
 (only-in "conc/base/phases/main.rkt"
          [run base:phases:run] [α base:phases:α] [≤a base:phases:≤a])
 (only-in "conc/base/full/main.rkt"
          [run base:full:run] [α base:full:α] [≤a base:full:≤a])
 (only-in "conc/set/core/main.rkt"
          [run set:core:run] [α set:core:α] [≤a set:core:≤a])
 (only-in "conc/set/phases/main.rkt"
          [run set:phases:run] [α set:phases:α] [≤a set:phases:≤a])
 (only-in "conc/set/full/main.rkt"
          [run set:full:run] [α set:full:α] [≤a set:full:≤a])
 )

(define ((run-all-examples all-examples all-runs) [mode 'check])
  (parameterize ([fail-count (if (eq? mode 'check) 0 -1)])
    (for ([run-info (in-list all-runs)])
      (match-define (list name num-ex run α ≤a) run-info)
      (printf "[~a]\n" name)
      (for ([i (in-range num-ex)]
            [examples (in-list all-examples)])
        (run-examples run examples mode α ≤a)))
    (when (>= (fail-count) 0)
      (printf "\nfail-count: ~a\n" (fail-count)))))

(define main
  (let ([all-examples (list
                       core:examples
                       phases:examples
                       (append local:examples defs:examples))]
        [all-runs
         `([base:core   1   ,base:core:run   ,base:core:α   ,base:core:≤a]
           [base:phases 2 ,base:phases:run ,base:phases:α ,base:phases:≤a]
           [base:full   3   ,base:full:run   ,base:full:α   ,base:full:≤a]
           [set:core    1    ,set:core:run    ,set:core:α    ,set:core:≤a]
           [set:phases  2  ,set:phases:run  ,set:phases:α  ,set:phases:≤a]
           [set:full    3    ,set:full:run    ,set:full:α    ,set:full:≤a]
           )])
    (run-all-examples all-examples all-runs)))
