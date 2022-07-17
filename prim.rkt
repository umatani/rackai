#lang racket
(provide prim? stx-prim?)

(define (prim? x)
  (or (member x '(syntax-e datum->syntax + - * / < = eq?
                           cons car cdr list second third fourth
                           printe ;; for debug
                           ))
      (stx-prim? x)))

(define (stx-prim? x)
  (member x '(syntax-local-value local-expand
                                 syntax-local-identifier-as-binding
                                 box unbox set-box!
                                 syntax-local-make-definition-context
                                 syntax-local-bind-syntaxes)))
