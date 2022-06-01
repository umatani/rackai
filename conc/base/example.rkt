#lang racket
(require (for-syntax racket/list))
(provide core:examples phases:examples local:examples defs:examples)

;; ----------------------------------------
;; Core Examples:

(define ex-<
  '[<
    (< 3 5)])

(define ex-eq?
  '[eq?
    (eq? 'a 'a)])

(define ex-lam
  '[lam
    ((lambda (lambda) lambda) 'foo)])

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
(define (raw-simple)
  (let-syntax ([x (lambda (stx) #'2)])
    (x 1)))

(define ex-reftrans
  '[reftrans
    (let ([z 1])
      ((let-syntax ([x (lambda (stx) #'z)])
         (lambda (z) (x))) 2))])
(define (raw-reftrans)
  (lambda (z)
    (let-syntax ([x (lambda (stx) #'z)])
      (lambda (z) (x)))))

(define ex-hyg
  '[hyg
    (let ([z 1])
      ((let-syntax
           ([x (lambda (stx)
                 (#%app datum->syntax
                        #'here
                        (#%app list #'lambda
                               (#%app datum->syntax #'here (#%app list #'z))
                               (#%app second (#%app syntax-e stx)))))])
         (x z)) 2))])
(define (raw-hyg)
  (lambda (z)
    (let-syntax ([x (lambda (stx)
                      #`(lambda (z) #,(second (syntax-e stx))))])
      (x z))))


(define ex-thunk
  '[thunk
    (let-syntax
        ([thunk (lambda (stx)
                  (#%app datum->syntax
                         stx
                         (#%app list #'lambda
                                (#%app datum->syntax stx (#%app list #'a)) 
                                (#%app second (#%app syntax-e stx)) ;; #'(+ a 1)
                                )))])
      ((let ([a 5])
         (thunk (+ a 1))) 0))])
(define (raw-thunk)
  (let-syntax ([thunk (lambda (stx)
                        #`(lambda (a)
                            #,(second (syntax-e stx)) ;; #'(+ a 1)
                            ))])
    (((lambda (a) (thunk (+ a 1))) 5) 0)))


(define ex-get-identity
  '[get-identity
    (let-syntax
        ([get-identity
          (lambda (stx)
            (#%app datum->syntax
                   stx
                   (#%app list #'lambda
                          (#%app datum->syntax stx (#%app list #'a))
                          (#%app datum->syntax
                                 stx
                                 (#%app list #'lambda
                                        (#%app
                                         datum->syntax
                                         stx
                                         (#%app list
                                                (#%app
                                                 second
                                                 (#%app syntax-e stx)) ;; #'a
                                                ))
                                        #'a)))))])
      (((get-identity a) 1) 2))])
(define (raw-get-identity)
  (let-syntax ([get-identity (lambda (stx)
                               #`(lambda (a)
                                   (lambda (#,(second (syntax-e stx))) ;; #'a
                                     a)))])
    (get-identity a)))

(define core:examples
  (list ex-<
        ex-eq?
        ex-lam
        ex-let
        ex-if-#t ex-if-#f
        ex-simple
        ex-reftrans
        ex-hyg
        ex-thunk
        ex-get-identity))

;; ----------------------------------------
;; Phases Examples:

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
(define (raw-prune)
  (let-syntax ([x (lambda (stx)
                    (let ([id1 #'y])
                      (let ([id2 #'y])
                        #`(let-syntax ([f (lambda (#,id2)
                                            (second (syntax-e #,id1)))])
                            (f '3)))))])
    (x)))


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
(define (raw-gen)
  (let-syntax ([x (lambda (stx)
                    (let ([id1 #'y])
                      (let ([id2 #'y])
                        #`(lambda (#,id2) #,id1))))])
    (x)))

(define phases:examples
  (list ex-prune
        ex-gen))


;; ----------------------------------------
;; Full Examples:

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
(define (raw-local-expand)
  (let-syntax ([q (lambda (stx) #'(car 8))])
    (let-syntax ([x (lambda (stx)
                      ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                      (let ([stx2 (syntax-e (local-expand
                                             (second (syntax-e stx))
                                             'expression
                                             '()))])
                        (second (syntax-e (cdr stx2)))))])
      (x (q)))))


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
(define (raw-local-expand-stop)
  (let-syntax ([p (lambda (stx) '0)])
    (let-syntax ([q (lambda (stx) #'(car 8))])
      (let-syntax ([x (lambda (stx)
                        ;; Used as (x (q)) => extracts '8 from (<#%app> . <('car '8)>)
                        (let ([stx2 (syntax-e (local-expand
                                               (second (syntax-e stx))
                                               'expression
                                               (list #'p)))])
                          (second (syntax-e (cdr stx2)))))])
        (x (q))))))


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
(define (raw-nested-local-expand)
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
        (list (b (z)) (b (a (z))))))))


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
(define (raw-local-binder)
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
      ;; TODO: 実装と不一致．取り除いても unbound にならない
      (a (q x) x))))

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
(define (raw-defs-shadow)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
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
        ((q p (call p)) (lambda () 'FOOOO))))))


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
(define (raw-defs-shadow2)
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
                            (let ([ignored (syntax-local-bind-syntaxes
                                            (list (second (syntax-e stx)))
                                            #f
                                            defs)])
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
          ((q p (call p)) (lambda () 'BOOOOOON)))))))


(define ex-defs-local-macro
  '[defs-local-macro
     (let-syntax ([call (lambda (stx)
                          (datum->syntax
                           #'here
                           (list (second (syntax-e stx)))))])
       (let-syntax ([p (lambda (stx) #'44)])
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
(define (raw-defs-local-macro)
  (let-syntax ([call (lambda (stx)
                       (datum->syntax
                        #'here
                        (list (second (syntax-e stx)))))])
    (let-syntax ([p (lambda (stx) #'44)])
      (let-syntax ([q (lambda (stx)
                        (let ([defs (syntax-local-make-definition-context)])
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
                                  (datum->syntax
                                   #'here
                                   (list
                                    #'quote
                                    (second (syntax-e stx))))
                                  #;
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
        ((q p (call p) #'13) 0)))))


(define (enriched-defs-local-macro)
  (let-syntax ([call (syntax-rules ()
                       [(call p) (p)])])
    (let-syntax ([p (syntax-rules () [(p) 44])])
      (let-syntax ([q (lambda (stx)
                        (syntax-case stx ()
                          [(q p c e)
                           (let ([defs (syntax-local-make-definition-context)])
                             (let ([ignored
                                    (syntax-local-bind-syntaxes
                                     (list #'p)
                                     #'(lambda (stx) e)
                                     defs)])
                               (let* ([body (local-expand #'c
                                                          'expression
                                                          '()
                                                          defs)]
                                      [e2 #'(quote p)
                                       #;
                                       (local-expand #'(quote p)
                                                     'expression
                                                     '()
                                                     defs)])
                                 (syntax-case e2 ()
                                   [(_ p2)
                                    #`(lambda (p2) #,body)]))))]))])
        ((q p (call p) #'13) 0)))))


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
             (q)) 0))])

(define (raw-defs-begin-with-defn)
  (let-syntax
      ([bwd (lambda (stx)
              (syntax-case stx (define define-syntax)
                [(bwd (define id1 e1)
                      (define-syntax id2 e2)
                      e3)
                 (let* ([;; create ctx
                         ctx (syntax-local-make-definition-context)]
                        [; for side-effect of binding x in ctx
                         ;; bind id1 (i.e., x)
                         ignored (syntax-local-bind-syntaxes (list #'id1) #f ctx)]
                        [; for side-effect of binding q in ctx
                         ;; bind id2 (i.e., q)
                         ignored (syntax-local-bind-syntaxes (list #'id2) #'e2 ctx)]
                        [;; local-expand e3 (i.e., the body expression)
                         ee3 (local-expand #'e3
                                           'expression
                                           (list #'lambda)
                                           ;; 上のストップリストでlambdaで止めないと，
                                           ;; 本体 x の expand をしてしまう．
                                           ;; reference の expand では ξ に TVarとして
                                           ;; 記録された Id に置き換えられるが，
                                           ;; この場合の ξ はどんな情報を？
                                           ctx)]
                        [; local-expand id1 (in a syntax form)
                         ;; local-expand of id1 (to give it context from ctx):
                         qid1 (local-expand (datum->syntax
                                             #'here
                                             (list #'quote
                                                   #'id1))
                                            'expression
                                            (list #'quote)
                                            ctx)]
                        [; extract expanded id1 from qid1
                         eid1 (second (syntax-e qid1))])
                   ;; generate ((lambda (eid1) ee3) e1):
                   #`((lambda (#,eid1) #,ee3) e1))]))])
    ;; `bwd' is short for `begin-with-definitions', which
    ;; assumes a `define' followed by a `define-syntax' followed
    ;; by a body form
    ((bwd (define x '10)
          (define-syntax q (lambda (v) (syntax (lambda (i) x))))
          (q)) 0)))
;; 上のコードの注記: ((bwd ...) 0) ではなく ((q) 0) とするとなぜか
;; q が out-of-contextとエラーになる．
;; ee3 (つまり本体)をつくる local-expand のstop-listが'(lambda)で
;; あるため，subformのexpansionが抑止され，((q) 0)のままだった．

(define defs:examples
  (list ex-box
        ex-set-box
        ex-defs-shadow
        ex-defs-shadow2
        ex-defs-local-macro
        ex-defs-begin-with-defn))
