#lang racket/base
(require
 racket/unit
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt" enable-tracing)
 "../../test/suites.rkt"
 "units.rkt")
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^)
   (link common@ misc@ domain@ syntax@ env@ store@ eval@ evaluator@
         menv@ mstore@ bind@ parse@ parser@ expand@ expander@
         io@ run@))
  (import) (export domain^ run^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'full   interp)
  (run-suite 'finite interp))
