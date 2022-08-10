#lang racket
(require (for-syntax racket))

(let ([a 100])
  ;; ==> a --> 100
  (let-syntax ([q (lambda (stx)
                    ;; ==> a
                    (car (syntax-e
                          (cdr (syntax-e
                                ;; ==> (#%app . (a))
                                (local-expand
                                 (second (syntax-e stx)) ;; (a)
                                 'expression
                                 (list)))))))])
    (q (a))))

(let ([a 100])
  ;; ==> a --> 100
  (let-syntax ([q (lambda (stx)
                    ;; ==> a
                    (car (syntax-e
                          ;; ==> (a)
                          (local-expand
                           (second (syntax-e stx)) ;; (a)
                           'expression
                           (list #'a)))))])
    (q (a))))


(let ([a 100])
  ;; --> 100
  (let-syntax ([q (lambda (stx)
                    (let ([b
                           ;; ==> a
                           (car (syntax-e
                                 (cdr (syntax-e
                                       ;; ==> (#%app . (a))
                                       (local-expand
                                        (second (syntax-e stx)) ;; (a)
                                        'expression
                                        (list))))))])
                      ;; ==> (lambda (a) a)
                      (datum->syntax
                       #'here
                       (list 'lambda
                             (list #'a) ;; a
                             b ;; a
                             ))))])
    ((q (a)) 1)))

(let ([a 100])
  ;; --> 100
  (let-syntax ([q (lambda (stx)
                    (let ([b
                           ;; ==> a (intro scopeなし？)
                           (car (syntax-e
                                   (local-expand
                                    (second (syntax-e stx))
                                    'expression
                                    (list #'a))))])
                      (datum->syntax
                       #'here
                       (list 'lambda
                             (list #'a)
                             b))))])
    ((q (a)) 1)))

;; stop listを使って展開を止めることで p の unbound を防ぐ
;; --> 1
(let-syntax ([q (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([b (second (syntax-e stx))])
                      (let ([p (car (syntax-e b))])
                        (let ([b2 (car (syntax-e (local-expand
                                                  b
                                                  'expression
                                                  (list p)
                                                  defs)))])
                          (datum->syntax
                           #'here
                           (list 'lambda
                                 (list p)
                                 b2)))))))])
  ((q (a)) 1))

;; stop listではなく，bind-syntaxesを使ってdefsに p を登録することで
;; unbound を防ぐ．展開後コードのbinding identifierには p そのものでなく，
;; defsのscopeを追加した p2 でないといけないことに注意．
;; --> 1
(let-syntax ([q (lambda (stx)
                  (let ([defs (syntax-local-make-definition-context)])
                    (let ([b (second (syntax-e stx))])
                      (let ([p (car (syntax-e b))])
                        (let ([ignored (syntax-local-bind-syntaxes (list p)
                                                                   #f
                                                                   defs)])
                          (let ([p2 (second (syntax-e (local-expand
                                                       (datum->syntax
                                                        #'here
                                                        (list 'quote p))
                                                       'expression
                                                       (list)
                                                       defs)))])
                            (let ([b2 (car
                                       (syntax-e
                                        (cdr (syntax-e (local-expand
                                                        b
                                                        'expression
                                                        (list)
                                                        defs)))))])
                              (datum->syntax
                               #'here
                               (list 'lambda
                                     (list p2)
                                     b2)))))))))])
  ((q (a)) 1))


(let-syntax
    ([m (lambda (stx)
          (let ([defs (syntax-local-make-definition-context)])
            (let ([p (second (syntax-e stx))])
              (let ([ignored (syntax-local-bind-syntaxes
                              (list p) #f defs)])
                (let ([p2 (local-expand
                           p
                           'expression
                           '()
                           defs)])
                  (datum->syntax
                   p2
                   (list 'lambda
                         (list p2)
                         p2)))))))])
  ((m a) 100))

(let-syntax
    ([m (lambda (stx)
          (let ([defs (syntax-local-make-definition-context)])
            (let ([ignored (syntax-local-bind-syntaxes
                            (list #'n) #'(lambda (stx) #'19) defs)])
              (local-expand
               #'(n)
               'expression
               '()
               defs))))])
  (m))

(let-syntax
    ([m (lambda (stx)
          (let ([x (second (syntax-e stx))])
            (let ([defs (syntax-local-make-definition-context)])
              (let ([ignore (syntax-local-bind-syntaxes
                             (list x) #f defs)])
                (let ([ignore2 (syntax-local-bind-syntaxes
                                (list #'n)
                                (datum->syntax
                                 #'here
                                 (list 'lambda
                                       (list 'stx)
                                       (list 'syntax x)))
                                defs)])
                  (let ([x2 (local-expand
                             #'(n)
                             'expression
                             '()
                             defs)])
                    (datum->syntax
                     #'here
                     (list 'lambda
                           (list x2)
                           x2))))))))])
  ((m v) 100))


(let-syntax
    ([bwd (lambda (stx)
            (let ([ctx   (syntax-local-make-definition-context)]
                  [x     (second (syntax-e (second (syntax-e stx))))]
                  [ten   (third (syntax-e (second (syntax-e stx))))]
                  [q     (second (syntax-e (third (syntax-e stx))))]
                  [trans (third (syntax-e (third (syntax-e stx))))]
                  [body  (fourth (syntax-e stx))])
              (let ([ignored (syntax-local-bind-syntaxes
                              (list x) #f ctx) ]
                    [ignored2 (syntax-local-bind-syntaxes
                               (list q) trans ctx)]
                    [body2 (local-expand body
                                         'expression
                                         (list #'lambda)
                                         ctx)]
                    [qx (local-expand (datum->syntax
                                       #'here
                                       (list #'quote
                                             x))
                                      'expression
                                      (list #'quote)
                                      ctx)])
                (let ([x2 (second (syntax-e qx))])
                  (datum->syntax
                   #'here
                   (list
                    (datum->syntax
                     #'here
                     (list #'lambda
                           (datum->syntax
                            #'here
                            (list x2))
                           body2))
                    ten))))))])
  ((bwd (define x '10)
        (define-syntax q (lambda (v) (syntax (lambda (i) x))))
        (q)) 0))
