#lang racket
(require
 "../../mix.rkt"
 (only-in "../../term.rkt" define-term)

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
            AstEnv% Store% KApp% KIf% SApp% SIf% SSeq%
            TVar% TStop% Σ% StoBind% κ% InEval% ζ%]))

(define-syntax #%term-forms (syntax-local-value #'core:#%term-forms))
