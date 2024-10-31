#lang racket/base
(require
 racket/unit
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"   enable-tracing)
 (only-in "../../test/suites.rkt" get-suite get-a-test run-suite run-a-test)
 "units.rkt")
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link common@ domain@ syntax@ env@ store@ eval@ evaluator@
         menv@ mstore@ bind@ parse@ parser@ expand@ expander@
         io@ run@ debug@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'finite interp))
