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

(define interp (interpreter 'mult:phases run δ α ≤ₐ #f))

(define (process form [mode 'eval]) ;; mode = read/expand/parse/eval
  (apply-interpreter interp form mode))


#;
(define (main [mode 'check])
  (run-suite run δ (suite 'core)   mode α set=? #;≤ₐ)
  (run-suite run δ (suite 'finite) mode α set=? #;≤ₐ)
  (run-suite run δ (suite 'phases) mode α set=? #;≤ₐ))
