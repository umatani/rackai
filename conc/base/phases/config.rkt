#lang racket
(require
 "../../../mix.rkt"
 (only-in "../../../term.rkt" define-term)

 (only-in "../core/config.rkt"
          [config^      core:config^]
          [config@      core:config@]
          [#%term-forms core:#%term-forms]))
(provide config^ config@ #%term-forms)

(define-signature config^ extends core:config^ ())

(define-mixed-unit config@
  (import)
  (export config^)
  (inherit [core:config@
            [core:Stxξ% Stxξ%]
            AstEnv% Store% KApp% KIf% SApp% SIf% SSeq%
            TVar% TStop% Σ% StoBind% κ% InEval% ζ%])
  ;;; updated (ph scps
  (define-term Stxξ core:Stxξ (ph scps)))

(define-syntax #%term-forms
  (append '((Stxξ ph stx ξ scps))
          (syntax-local-value #'core:#%term-forms)))
