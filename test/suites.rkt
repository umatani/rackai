#lang racket
(require
 (prefix-in  ss: (only-in "suites/scopeset.rkt"
                          core:examples
                          phases:examples
                          local:examples
                          defs:examples))
 (prefix-in org: (only-in "suites/original.rkt"
                          core:examples
                          phases:examples
                          full:examples
                          finite:examples))
 (only-in "../interpreter.rkt" get-results)
 (for-syntax (only-in racket/list second third fourth)))
(provide get-suite get-a-test run-a-test run-suite
         (for-syntax (all-from-out racket/list)))

(define suites
  (hasheq 'core   (append ss:core:examples   org:core:examples)
          'phases (append ss:phases:examples org:phases:examples)
          'full   (append ss:local:examples  ss:defs:examples org:full:examples)
          'finite (append org:finite:examples)))

(define (get-suite suite-name) (hash-ref suites suite-name))

(define (get-a-test name [suite-name #f])
  (define all (if suite-name
                (hash-ref suites suite-name)
                (apply append (hash-values suites))))
  (define candidates (filter (λ (x) (eq? (first x) name)) all))
  (if (= (length candidates) 1)
    (second (first candidates))
    (error "cannot identify test: " name)))

(define (run-a-test name interp
                    #:suite    [suite     #f]
                    #:mode     [mode      'eval]
                    #:check    [reference #t])
  (define form (get-a-test name suite))
  (interp form #:mode mode #:check reference))

(define (run-suite suite-name interp
                   #:mode     [mode 'eval]
                   #:check    [reference #t]
                   #:verbose? [verbose? #t])
  (for ([name (in-list (map first (get-suite suite-name)))])
    (define v (run-a-test name interp
                          #:suite suite-name #:mode mode #:check reference))
    (when verbose?
      (printf "  ~a: " name) (pretty-display v))))
