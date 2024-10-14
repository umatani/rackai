#lang racket
(require
 (only-in "../../term.rkt" define-term use-term)
 (rename-in (except-in "../core/terms.rkt" Stxξ)
            [#%term-forms core:#%term-forms]
            [Stxξ% core:Stxξ%]))
(provide (all-defined-out)
         (except-out (all-from-out "../core/terms.rkt")
                     core:#%term-forms
                     core:Stxξ%))

;;; updated (ph scps
(define-term Stxξ core:Stxξ (ph scps))

(define-syntax #%term-forms
  (append '((Stxξ ph stx ξ scps))
          (syntax-local-value #'core:#%term-forms)))

(use-term Stxξ)
