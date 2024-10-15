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

(define interp (interpreter 'mult:core run delta α ≤a #f))

(define (process form [mode 'eval]) ;; mode = read/expand/parse/eval
  (apply-interpreter interp form mode))

;; run example
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α set=? #;≤a)
  (run-suite run delta (suite 'finite) mode α set=? #;≤a))
