#lang racket
(require (rename-in racket/set [set _set]))
(provide (all-defined-out)
         (except-out (all-from-out racket/set) _set))

(define-match-expander set
  (syntax-rules (... ...)
    [(set p ... q (... ...))
     (and (app set->list (list-no-order p ... q (... ...)))
          #;(app (λ (_) (list->set qs)) q)
          )]
    [(set p ...)
     (app set->list (list-no-order p ...))])
  (syntax-id-rules ()
    [(set p ...) (_set p ...)]
    [set _set]))
