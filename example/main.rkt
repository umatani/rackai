#lang racket
(require
 (only-in "run.rkt" fail-count)

 (prefix-in b:c: "../interp-base/core/main.rkt")
 (prefix-in b:p: "../interp-base/phases/main.rkt")
 (prefix-in b:f: "../interp-base/full/main.rkt")
 (prefix-in s:c: "../interp-set/core/main.rkt")
 (prefix-in s:p: "../interp-set/phases/main.rkt")
 (prefix-in s:f: "../interp-set/full/main.rkt")
 (prefix-in a:c: "../abs/core.rkt")
 (prefix-in a:p: "../abs/phases.rkt")
 (prefix-in a:f: "../abs/full.rkt")

 "suites.rkt")


(struct interp (name run delta α ≤a) #:transparent)

(define interpreters
  (list (interp 'base:core   b:c:run b:c:delta b:c:α b:c:≤a)
        (interp 'base:phases b:p:run b:p:delta b:p:α b:p:≤a)
        (interp 'base:full   b:f:run b:f:delta b:f:α b:f:≤a)
        (interp 'set:core    s:c:run s:c:delta s:c:α s:c:≤a)
        (interp 'set:phases  s:p:run s:p:delta s:p:α s:p:≤a)
        (interp 'set:full    s:f:run s:f:delta s:f:α s:f:≤a)
        (interp 'abs:core    a:c:run a:c:delta a:c:α s:c:≤a)
        (interp 'abs:phases  a:p:run a:p:delta a:p:α s:p:≤a)
        (interp 'abs:full    a:f:run a:f:delta a:f:α s:f:≤a)
        ))

(define test-suites
  (hasheq 'base:core   (map suite '(core             finite))
          'base:phases (map suite '(core phases      finite))
          'base:full   (map suite '(core phases full finite))
          'set:core    (map suite '(core             finite))
          'set:phases  (map suite '(core phases      finite))
          'set:full    (map suite '(core phases full finite))
          'abs:core    (map suite '(core))
          'abs:phases  (map suite '(core phases))
          'abs:full    (map suite '(core phases full))
          ))


(define ((run-all interpreters) [mode 'check])
  (parameterize ([fail-count (if (eq? mode 'check) 0 -1)])
    (for ([interpreter (in-list interpreters)])
      (match-define (interp name run delta α ≤a) interpreter)
      (printf "[~a]\n" name)
      (for ([suite (in-list (hash-ref test-suites name))])
        (run-suite run delta suite mode α ≤a)))
    (when (>= (fail-count) 0)
      (printf "\nfail-count: ~a\n" (fail-count)))))

(define main (run-all interpreters))
