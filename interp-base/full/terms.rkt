#lang racket
(require
 "../../term.rkt"
 (rename-in "../phases/terms.rkt" [#%term-forms phases:#%term-forms]
            [Stxξ% phases:Stxξ%]))
(provide Stxξ% #%term-forms)

;; remove scps from those of phases
(define-term Stxξ     phases:Stxξ   () #:remove [scps])

(define-syntax #%term-forms
  (append '((Stxξ     ph stx ξ))
          (syntax-local-value #'phases:#%term-forms)))
