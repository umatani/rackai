#lang racket
(require
 (only-in "../reduction.rkt" enable-tracing)
 (only-in "../nondet.rkt"    enable-checkpoint)
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
 ;; (prefix-in n:f: "../abs/naive/full.rkt")
 "suites.rkt")

(define interpreters
  (list
   (cons 'base:core    b:c:interp)
   (cons 'base:phases  b:p:interp)
   (cons 'base:full    b:f:interp)

   (cons 'mult:core    m:c:interp)
   (cons 'mult:phases  m:p:interp)
   (cons 'mult:full    m:f:interp)

   (cons 'abs:core     a:c:interp)
   (cons 'abs:phases   a:p:interp)
   (cons 'abs:full     a:f:interp)

   (cons 'naive:core   n:c:interp)
   (cons 'naive:phases n:p:interp)
   ;; (cons 'naive:full   n:f:interp)
   ))

(define suites
  (hasheq
   'base:core    '[core             finite]
   'base:phases  '[core phases      finite]
   'base:full    '[core phases full finite]

   'mult:core    '[core             finite]
   'mult:phases  '[core phases      finite]
   'mult:full    '[core phases full finite]

   'abs:core     '[core                   ]
   'abs:phases   '[core phases            ]
   'abs:full     '[core phases full       ]

   'naive:core   '[core             finite]
   'naive:phases '[core phases      finite]
   ;; 'naive:full   '[core phases full finite]
   ))

(define (run-all interpreters suites mode reference verbose?)
  (for ([x (in-list interpreters)])
    (match-define (cons name interpreter) x)

    (when verbose? (printf "\n[~a]\n" name))
    (reset-outcomes interpreter)
    (for ([suite-name (in-list (hash-ref suites name))])
      (run-suite suite-name interpreter
                 #:mode mode #:check reference #:verbose? verbose?))

    (when (or (equal? mode 'check) (equal? mode 'check-with-raw))
      (show-outcomes interpreter)))

  (when (and reference (equal? mode 'eval))
    (when verbose? (printf "\n\n"))
    (for/fold ([exact 0]
               [inexact 0]
               [unsound 0]
               [fail 0]
               #:result
               (begin (printf "\n⟦Total Summary⟧\n")
                      (printf "  OK: ~a (~a exact)\n  NG: ~a (~a fail)\n"
                              (+ exact inexact) exact (+ unsound fail) fail)))
              ([x (in-list interpreters)])
      (match-define (cons name interpreter) x)
      (let* ([outcomes (get-outcomes interpreter)]
             [e (hash-ref outcomes 'exact)]
             [i (hash-ref outcomes 'inexact)]
             [u (hash-ref outcomes 'unsound)]
             [f (hash-ref outcomes 'fail)])
        (printf "[~a Summary]\n" name)
        (printf "  OK: ~a (~a exact)\n  NG: ~a (~a fail)\n"
                (+ e i) e (+ u f) f)        
        (values (+ exact e) (+ inexact i) (+ unsound u) (+ fail f))))))

;; Base evaluator
;;   base-eval: Sexp → (Setof Val)
(define (base-eval form)
  (set (b:f:interp form #:mode 'eval #:check #f)))

(define (test #:mode     [mode      'eval]
              #:ref      [reference raw-eval #;base-eval]
              #:verbose? [verbose?  #f])
  (run-all interpreters suites mode reference verbose?))

;(test)
