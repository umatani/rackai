#lang racket
(require
 "../../interpreter.rkt"
 ;"../../test/suites.rkt"
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

(define interp (interpreter 'mult:full run delta α ≤a #f))

(define (process form [mode 'eval]) ;; mode = read/expand/parse/eval
  (apply-interpreter interp form mode))


#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'finite) mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a)
  (run-suite run delta (suite 'full)   mode α ≤a))
