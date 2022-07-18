#lang racket
(require
 "example.rkt"
 (prefix-in b:c: "conc/base/core/main.rkt")
 (prefix-in b:p: "conc/base/phases/main.rkt")
 (prefix-in b:f: "conc/base/full/main.rkt")
 (prefix-in s:c: "conc/set/core/main.rkt")
 (prefix-in s:p: "conc/set/phases/main.rkt")
 (prefix-in s:f: "conc/set/full/main.rkt"))

(define ((run-all-examples all-examples all-runs) [mode 'check])
  (parameterize ([fail-count (if (eq? mode 'check) 0 -1)])
    (for ([run-info (in-list all-runs)])
      (match-define (list name num-ex run delta α ≤a) run-info)
      (printf "[~a]\n" name)
      (for ([i (in-range num-ex)]
            [examples (in-list all-examples)])
        (run-examples run delta examples mode α ≤a)))
    (when (>= (fail-count) 0)
      (printf "\nfail-count: ~a\n" (fail-count)))))

(define main
  (let ([all-examples (list
                       core:examples
                       phases:examples
                       (append local:examples defs:examples))]
        [all-runs
         `([base:core   1 ,b:c:run ,b:c:delta ,b:c:α ,b:c:≤a]
           [base:phases 2 ,b:p:run ,b:p:delta ,b:p:α ,b:p:≤a]
           [base:full   3 ,b:f:run ,b:f:delta ,b:f:α ,b:f:≤a]
           [set:core    1 ,s:c:run ,s:c:delta ,s:c:α ,s:c:≤a]
           [set:phases  2 ,s:p:run ,s:p:delta ,s:p:α ,s:p:≤a]
           [set:full    3 ,s:f:run ,s:f:delta ,s:f:α ,s:f:≤a]
           )])
    (run-all-examples all-examples all-runs)))
