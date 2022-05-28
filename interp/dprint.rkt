#lang racket
(require "set.rkt")
(provide with-dprint dprint)

(define enable-dprint (make-parameter #f))

(define-syntax-rule (with-dprint (model ...) (name ...) body ...)
  (parameterize ([enable-dprint (cons (set 'model ...)
                                      (set 'name ...))])
    body ...))

(define-syntax-rule (dprint model name msg arg ...)
  (let ([ed (enable-dprint)])
    (when (and ed
               (or (and (set-empty? (car ed)) (set-empty? (cdr ed)))
                   (and (set-empty? (car ed))
                        (set-member? (cdr ed) name))
                   (and (set-empty? (cdr ed))
                        (set-member? (car ed) model))
                   (and (set-member? (car ed) model)
                        (set-member? (cdr ed) name))))
      (printf "~a:~a:~a\n" model name (format msg arg ...)))))
