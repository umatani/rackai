#lang racket
(require
 "../interpreter.rkt"

 (prefix-in b:c: "../base/core/main.rkt")
 (prefix-in b:p: "../base/phases/main.rkt")
 (prefix-in b:f: "../base/full/main.rkt")
 (prefix-in m:c: "../mult/core/main.rkt")
 (prefix-in m:p: "../mult/phases/main.rkt")
 (prefix-in m:f: "../mult/full/main.rkt")
 (prefix-in a:c: "../abs/core.rkt")
 (prefix-in a:p: "../abs/phases.rkt")
 (prefix-in a:f: "../abs/full.rkt")
 (prefix-in n:c: "../abs/naive/core.rkt")
 (prefix-in n:p: "../abs/naive/phases.rkt")
 (prefix-in n:f: "../abs/naive/full.rkt")

 "suites.rkt")

(define interpreters
  (list
   b:c:interp b:p:interp b:f:interp     ;; base
   m:c:interp m:p:interp m:f:interp     ;; mult
   a:c:interp a:p:interp a:f:interp     ;; abs
   n:c:interp n:p:interp n:f:interp     ;; abs/naive
   ))

(define test-suites
  (hasheq
   'base:core    (map suite '[core             finite])
   'base:phases  (map suite '[core phases      finite])
   'base:full    (map suite '[core phases full finite])

   'mult:core    (map suite '[core             finite])
   'mult:phases  (map suite '[core phases      finite])
   'mult:full    (map suite '[core phases full finite])

   'abs:core     (map suite '[core                   ])
   'abs:phases   (map suite '[core phases            ])
   'abs:full     (map suite '[core phases full       ])

   'naive:core   (map suite '[core             finite])
   'naive:phases (map suite '[core phases      finite])
   'naive:full   (map suite '[core phases full finite])
   ))


(define ((run-all interpreters) [mode 'check-with-raw])
  (for ([interp (in-list interpreters)])
    (define name (interpreter-name interp))
    (printf "\n[~a]\n" name)

    (when (or (equal? mode 'check) (equal? mode 'check-with-raw))
      (set-interpreter-result!
       interp
       (make-hasheq '([exact . 0] [inexact . 0] [unsound . 0] [fail . 0]))))

    (for ([suite (in-list (hash-ref test-suites name))])
      (run-suite suite interp mode))

    (when (or (equal? mode 'check) (equal? mode 'check-with-raw))
      (let* ([result  (interpreter-result interp)]
             [exact   (hash-ref result 'exact)]
             [inexact (hash-ref result 'inexact)]
             [unsound (hash-ref result 'unsound)]
             [fail    (hash-ref result 'fail)])
        (printf "\n  (Summary)\n  - OK: ~a (~a exact)\n  - NG: ~a (~a fail)\n"
                (+ exact inexact) exact (+ unsound fail) fail))))

  (when (or (equal? mode 'check) (equal? mode 'check-with-raw))
    (for/fold ([exact 0]
               [inexact 0]
               [unsound 0]
               [fail 0]
               #:result
               (begin (printf "\n(Total Summary)\n")
                      (printf "  - OK: ~a (~a exact)\n  - NG: ~a (~a fail)\n"
                              (+ exact inexact) exact (+ unsound fail) fail)))
              ([interp (in-list interpreters)])
      (let* ([result  (interpreter-result interp)]
             [e (hash-ref result 'exact)]
             [i (hash-ref result 'inexact)]
             [u (hash-ref result 'unsound)]
             [f (hash-ref result 'fail)])
        (values (+ exact e) (+ inexact i) (+ unsound u) (+ fail f))))))

(define main (run-all interpreters))

;(main)
