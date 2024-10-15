#lang racket
(require profile
         (only-in redex/reduction-semantics
                  caching-enabled? current-cache-all?)
         "../interpreter.rkt"
         (only-in  "../base/full/main.rkt" interp)
         (only-in "../model/scopeset-smallstep/full-machine.rkt" run)
         )

;;;; drivers

(define (iter n f)
  (if (< n 2)
      (f)
      (begin (f)
             (iter (- n 1) f))))

(define-syntax (measure stx)
  (syntax-case stx ()
    [(measure cmd repeat prof)
     #'(if prof
           (profile cmd #:delay 0.001 #:repeat repeat #:threads #t #:order 'self
                    #:use-errortrace? #f)
           (call-with-values
            (λ () (time-apply iter (list repeat (λ () cmd))))
            (λ (rslt cpu real gc) real)))]))

(define (raw-eval sexp #:repeat [repeat 1] #:profile [prof #t])
  (measure (eval sexp) repeat prof))

(define lw-eval-no-cache
  (match-let ([(interpreter _ run delta _ _ _) interp])
    (λ (sexp #:repeat repeat #:profile prof)
      (measure (run delta sexp 'eval) repeat prof))))

(define (rx-eval-no-cache sexp #:repeat [repeat 1] #:profile [prof #t])
  (parameterize ([caching-enabled? #f]
                 [current-cache-all? #t])
    (measure (run sexp 'eval) repeat prof)))

(define (rx-eval sexp #:repeat [repeat 1] #:profile [prof #t])
  (parameterize ([caching-enabled? #t]
                 [current-cache-all? #t])
    (measure (run sexp 'eval) repeat prof)))


(define (run-examples ev examples #:repeat [repeat 1000] #:profile [prof #f])
  (for ([example (in-list examples)])
    (match-define (list name form) example)
    (printf "  ~a: ~a\n" name
            (ev form #:repeat repeat #:profile prof))))

#| Usage

eval2.rkt> (run-examples lw-eval-no-cache scopeset-examples #:repeat 1)
<: 5
eq?: 4
let-x: 6
if-#t: 5
if-#f: 15
simple: 1
reftrans: 5
hyg: 23
thunk: 29
get-identity: 42
prune: 78
gen: 17
local-value: 17
local-expand: 20
local-expand-stop: 23
nested-local-expand: 50
local-binder: 54
box: 14
set-box: 18
defs-shadow: 114
defs-shadow2: 136
defs-local-macro: 143
eval2.rkt> (run-examples rx-eval scopeset-examples #:repeat 1)
  <: 11
  eq?: 9
  let-x: 24
  if-#t: 20
  if-#f: 100
  simple: 9
  reftrans: 38
  hyg: 466
  thunk: 473
  get-identity: 1326
  prune: 14329
  gen: 429
  local-value: 305
  local-expand: 449
  local-expand-stop: 783
  nested-local-expand: 1901
  local-binder: 2278
  box: 240
  set-box: 448
  defs-shadow: 38311
  defs-shadow2: 50246
  defs-local-macro: 67086
eval2.rkt> (run-examples rx-eval-no-cache scopeset-examples #:repeat 1)
  <: 52
  eq?: 39
  let-x: 126
  if-#t: 138
  if-#f: 1475
  simple: 59
  reftrans: 360
  hyg: 6858
  thunk: 6589
  get-identity: 16301
  prune: 69367
  gen: 6075
  local-value: 4542
  local-expand: 7242
  local-expand-stop: 12053
  nested-local-expand: 27701
  local-binder: 43171
  box: 3787
  set-box: 7710
  defs-shadow: 227768
  defs-shadow2: 317159
  defs-local-macro: 396146
|#

;;;; benchmarks

;;;; Example code from ``Binding as Sets of Scopes''

(define ex-<
  '[<
    (< 3 5)])

(define ex-eq?
  '[eq?
    (eq? 'a 'a)])

(define ex-let
  '[let-x
    (let ([x 1]) (+ x 2))])

(define ex-if-#t
  '[if-#t
    (if (< 0 1) 'foo 'bar)])

(define ex-if-#f
  '[if-#f
    (let ([x 3] [y 2])
      (if (< x y) (+ x y) (* x y)))])

(define ex-simple
  '[simple
    (let-syntax ([x (lambda (stx) #'2)])
      (x 1))])

(define ex-reftrans
  '[reftrans
    (let ([z 1])
      ((let-syntax ([x (lambda (stx) #'z)])
         (lambda (z) (x))) 2))])

(define ex-hyg
  '[hyg
    (let ([z 1])
      ((let-syntax
           ([x (lambda (stx)
                 (#%app datum->syntax
                        #'here
                        (#%app list #'lambda (#%app datum->syntax #'here (#%app list #'z))
                               (#%app second (#%app syntax-e stx)))))])
         (x z)) 2))])

(define ex-thunk
  '[thunk
    (let-syntax
        ([thunk (lambda (stx)
                  (#%app datum->syntax
                         stx
                         (#%app list #'lambda (#%app datum->syntax stx (#%app list #'a)) 
                                (#%app second (#%app syntax-e stx)) ;; #'(+ a 1)
                                )))])
      ((let ([a 5])
         (thunk (+ a 1))) 0))])

(define ex-get-identity
  '[get-identity
    (let-syntax
        ([get-identity (lambda (stx)
                         (#%app datum->syntax
                                stx
                                (#%app list #'lambda 
                                       (#%app datum->syntax stx (#%app list #'a))
                                       (#%app datum->syntax
                                              stx
                                              (#%app list #'lambda
                                                     (#%app datum->syntax
                                                            stx
                                                            (#%app list
                                                                   (#%app second (#%app syntax-e stx)) ;; #'a
                                                                   ))
                                                     #'a)))))])
      (((get-identity a) 1) 2))])

(define core:examples
  (list ex-<
        ex-eq?
        ex-let
        ex-if-#t ex-if-#f
        ex-simple
        ex-reftrans
        ex-hyg
        ex-thunk
        ex-get-identity))


(define ex-prune
  ;; This example fails if we make `prune` a no-op
  '[prune
    (let-syntax ([x (lambda (stx)
                      (let ([id1 #'y])   ;; <-- pruned here
                        (let ([id2 #'y]) ;; <-- pruned here
                          (datum->syntax
                           stx
                           (list #'let-syntax
                                 (datum->syntax
                                  stx
                                  (list
                                   (datum->syntax
                                    stx
                                    (list
                                     #'f
                                     (datum->syntax
                                      stx
                                      (list
                                       #'lambda (datum->syntax stx (list id2))
                                       (datum->syntax
                                        stx
                                        (list #'second
                                              (datum->syntax
                                               stx
                                               (list #'syntax-e id1)))))))))) 
                                 #'(f '3))))))])
      (x))])

(define ex-gen
  ;; This example works even without pruning, since
  ;; the extra scopes on `id1` and `id2` are at phase 1,
  ;; and the identifiers are resolved at phase 0
  '[gen
    (let-syntax ([x (lambda (stx)
                      (let ([id1 #'y])
                        (let ([id2 #'y])
                          (datum->syntax
                           stx
                           (list #'lambda
                                 (datum->syntax stx (list id2))
                                 id1)))))])
      ((x) 'FOO))])

(define phases:examples
  (list ex-prune
        ex-gen))


(define ex-local-value
  '[local-value
    (let-syntax ([a '8])
      (let-syntax ([b '9])
        (let-syntax ([x (lambda (stx)
                          (datum->syntax
                           #'here
                           (list #'quote
                                 (datum->syntax
                                  #'here
                                  (syntax-local-value (second (syntax-e stx)))))))])
          (x a))))])

(define ex-local-expand
  '[local-expand
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx) 
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (second (syntax-e (cdr (syntax-e (local-expand
                                                          (second (syntax-e stx))
                                                          'expression
                                                          '()))))))])
        (x (q))))])

(define ex-local-expand-stop
  '[local-expand-stop
    (let-syntax ([p (lambda (stx) '0)])
      (let-syntax ([q (lambda (stx) #'(car 8))])
        (let-syntax ([x (lambda (stx) 
                          ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                          (second (syntax-e (cdr (syntax-e (local-expand
                                                            (second (syntax-e stx))
                                                            'expression
                                                            (list #'p)))))))])
          (x (q)))))])

(define ex-nested-local-expand
  '[nested-local-expand
    (let-syntax ([z (lambda (stx) #''0)])
      (let-syntax ([a (lambda (stx)
                        ;; When `b' forces `a', then `a'
                        ;; drops `z' form the stop list, so it
                        ;; should expand to 0
                        (local-expand (second (syntax-e stx))
                                      'expression
                                      '()))])
        (let-syntax ([b (lambda (stx)
                          (datum->syntax
                           stx
                           (list
                            #'quote 
                            (local-expand (second (syntax-e stx))
                                          'expression
                                          (list #'z)))))])
          (list (b (z)) (b (a (z)))))))])

(define ex-local-binder
  '[local-binder
    (let-syntax ([q (lambda (stx)
                      ;; quotes its argument
                      (datum->syntax
                       stx
                       (list #'quote (second (syntax-e stx)))))])
      (let-syntax ([a (lambda (stx)
                        ;; expands first argument, expected quoted name
                        ;; to use as binder with second arguments body
                        (datum->syntax
                         stx
                         (list
                          #'lambda
                          (datum->syntax
                           stx
                           (list (syntax-local-identifier-as-binding
                                  (second (syntax-e
                                           (local-expand (second (syntax-e stx))
                                                         'expression
                                                         '()))))))
                          (third (syntax-e stx)))))])
        ;; removing the syntax-local-identifier-as-binding call above
        ;; leaves the second `x` as unbound:
        ;; TODO: 実装と不一致．取り除いても↓の実装では unbound にならない
        ((a (q x) x) 'FOOOO)))])

(define local:examples
  (list ex-local-value
        ex-local-expand
        ex-local-expand-stop
        ex-nested-local-expand
        ex-local-binder))


(define ex-box
  '[box
    (let-syntax ([m (lambda (stx)
                      (datum->syntax
                       stx
                       (list
                        #'quote
                        (datum->syntax
                         stx
                         (let ([b (box 0)])
                           (unbox b))))))])
      (m))])

(define ex-set-box
  '[set-box
    (let-syntax ([m (lambda (stx)
                      (datum->syntax
                       stx
                       (list
                        #'quote
                        (datum->syntax
                         stx
                         (let ([b (box 0)])
                           (let ([x (set-box! b 1)])
                             (unbox b)))))))])
      (m))])

(define ex-defs-shadow
  '[defs-shadow
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([p (lambda (stx) #'0)])
         (let-syntax ([q (lambda (stx)
                           (let ([defs (syntax-local-make-definition-context)])
                             (let ([ignored (syntax-local-bind-syntaxes
                                             (list (second (syntax-e stx))) #f defs)])
                               (datum->syntax
                                #'here
                                (list
                                 #'lambda
                                 ; not necessary in this case, but sensible
                                 (datum->syntax
                                  #'here
                                  (list (syntax-local-identifier-as-binding 
                                         (second
                                          (syntax-e
                                           (local-expand (datum->syntax
                                                          #'here
                                                          (list #'quote
                                                                (second (syntax-e stx))))
                                                         'expression
                                                         '()
                                                         defs))))))
                                 (local-expand (third (syntax-e stx))
                                               'expression
                                               (list #'call)
                                               defs))))))])
           ((q p (call p)) (lambda () 'FOOOO)))))])

;; Like the previous example, but using a macro that expands to `quote`:
(define ex-defs-shadow2
  '[defs-shadow2
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([qt (lambda (stx)
                          (datum->syntax
                           #'here
                           (list #'quote (second (syntax-e stx)))))])
         (let-syntax ([p (lambda (stx) #'0)])
           (let-syntax ([q (lambda (stx)
                             (let ([defs (syntax-local-make-definition-context)])
                               (let ([ignored 
                                      (syntax-local-bind-syntaxes
                                       (list (second (syntax-e stx))) #f defs)])
                                 (datum->syntax
                                  #'here
                                  (list
                                   #'lambda
                                   (datum->syntax
                                    #'here
                                    ; necessary in this case
                                    (list (syntax-local-identifier-as-binding
                                           (second
                                            (syntax-e
                                             (local-expand (datum->syntax
                                                            #'here
                                                            (list #'qt
                                                                  (second (syntax-e stx))))
                                                           'expression
                                                           '()
                                                           defs))))))
                                   (local-expand (third (syntax-e stx))
                                                 'expression
                                                 (list #'call)
                                                 defs))))))])
             ((q p (call p)) (lambda () 'BOOOOOON))))))])

(define ex-defs-local-macro
  '[defs-local-macro
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([p (lambda (stx) #'0)])
         (let-syntax ([q (lambda (stx)
                           (let ([defs (syntax-local-make-definition-context) ])
                             (let ([ignored 
                                    (syntax-local-bind-syntaxes
                                     (list (second (syntax-e stx)))
                                     (datum->syntax
                                      #'here
                                      (list #'lambda
                                            (datum->syntax #'here (list #'stx))
                                            (fourth (syntax-e stx))))
                                     defs)])
                               (datum->syntax                             
                                #'here
                                (list
                                 #'lambda
                                 (datum->syntax
                                  #'here
                                  (list
                                   (second
                                    (syntax-e
                                     (local-expand (datum->syntax
                                                    #'here
                                                    (list
                                                     #'quote
                                                     (second (syntax-e stx))))
                                                   'expression
                                                   '()
                                                   defs)))))
                                 (local-expand (third (syntax-e stx))
                                               'expression
                                               '()
                                               defs))))))])
           ((q p (call p) #'13) '0))))])

(define ex-defs-begin-with-defn
  '[defs-begin-with-defn
     (let-syntax ([bwd (lambda (stx)
                         (let ([;; create ctx
                                ctx (syntax-local-make-definition-context)]
                               [; the x in (define x '10)
                                id1 (second (syntax-e (second (syntax-e stx))))]
                               [; the 10 in (define x '10)
                                e1 (third (syntax-e (second (syntax-e stx))))]
                               [; the q in (define-syntax q (lambda (v) ...))
                                id2 (second (syntax-e (third (syntax-e stx))))]
                               [; the (lambda (v) ...) in (define-syntax q (lambda (v) ...))
                                e2 (third (syntax-e (third (syntax-e stx))))]
                               [; the last body expression, expands to (lambda (i) x)
                                e3 (fourth (syntax-e stx))])
                           (let ([; for side-effect of binding x in ctx
                                  ;; bind id1 (i.e., x)
                                  ignored (syntax-local-bind-syntaxes
                                           (list id1) #f ctx) ]
                                 [; for side-effect of binding q in ctx
                                  ;; bind id2 (i.e., q)
                                  ignored2 (syntax-local-bind-syntaxes
                                            (list id2) e2 ctx)]
                                 [; local-expand e3
                                  ;; local-expand e3 (i.e., the body expression):
                                  ee3 (local-expand e3
                                                    'expression
                                                    (list #'lambda)
                                                    ctx)]
                                 [; local-expand id1 (in a syntax form)
                                  ;; local-expand of id1 (to give it context from ctx):
                                  qid1 (local-expand (datum->syntax
                                                      #'here
                                                      (list #'quote
                                                            id1))
                                                     'expression
                                                     (list #'quote)
                                                     ctx)])
                             (let ([; extract expanded id1 from qid1
                                    eid1 (second (syntax-e qid1))])
                               ;; generate ((lambda (eid1) ee3) '10):
                               (datum->syntax
                                #'here
                                (list
                                 (datum->syntax
                                  #'here
                                  (list #'lambda
                                        (datum->syntax
                                         #'here
                                         (list eid1))
                                        ee3))
                                 e1))))))])
       ;; `bwd' is short for `begin-with-definitions', which
       ;; assumes a `define' followed by a `define-syntax' followed
       ;; by a body form
       ((bwd (define x '10)
             (define-syntax q (lambda (v) (syntax (lambda (i) x))))
             #;(lambda i x)
             (q)) 0))])

(define defs:examples
  (list ex-box
        ex-set-box
        ex-defs-shadow
        ex-defs-shadow2
        ex-defs-local-macro
        ;ex-defs-begin-with-defn
        ))

(define scopeset-examples
  (append core:examples phases:examples local:examples defs:examples))
