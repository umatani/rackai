#lang racket
(require
 "../../interpreter.rkt"
 (only-in "../../test/suites.rkt" get-suite get-a-test run-suite run-a-test)
 "../../signatures.rkt"
 "units.rkt")
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ id@ mcont@ parse@ parser@ expand@ expander@
         io@ run@ debug@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(require (for-syntax racket/list))
(define (test)
  (run-suite 'core   interp)
  (run-suite 'finite interp))
