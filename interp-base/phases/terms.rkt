#lang racket
(require
 "../../term.rkt"
 (rename-in "../core/terms.rkt"
            [#%term-forms core:#%term-forms]
            [Stxξ% core:Stxξ%]))
(provide Stxξ% #%term-forms
         (all-from-out "../core/terms.rkt"))

;;; updated (ph scps
(define-term Stxξ core:Stxξ (ph scps))

(define-syntax #%term-forms
  (append '((Stxξ ph stx ξ scps))
          (syntax-local-value #'core:#%term-forms)))
