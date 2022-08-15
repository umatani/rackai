#lang racket
(require
 "../../mix.rkt"

 (only-in "../../config.rkt"
          [config^ common:config^]
          [config@ common:config@]
          [#%term-forms common:#%term-forms]))
(provide config^ config@ #%term-forms)

(define-signature config^ extends common:config^ ())

(define-mixed-unit config@
  (import)
  (export config^)
  (inherit [common:config@
            AstEnv% Store% KApp% KIf% SApp% SIf% SSeq%
            TVar% TStop% Σ% StoBind% κ% InEval% ζ%]))

(define-syntax #%term-forms
  (syntax-local-value #'common:#%term-forms))
