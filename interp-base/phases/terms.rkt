#lang racket
(require
 "../../term.rkt"
 (rename-in "../../terms.rkt" [#%term-forms super:#%term-forms]
            [Stxξ% super:Stxξ%]))
(provide Stxξ% #%term-forms)

;;; updated (ph scps
(define-term Stxξ super:Stxξ (ph scps))

(define-syntax #%term-forms
  (append '((Stxξ ph stx ξ scps))
          (syntax-local-value #'super:#%term-forms)))
