#lang racket
(require
 "../../interpreter.rkt"
 ;"../../test/suites.rkt"
 "../../signatures.rkt"
 ;; Units
 (only-in "../../units.rkt"           io@)
 (only-in "../units.rkt"              domain@ env@ store@ menv@ mstore@ run@)
 (only-in "../../conc/units.rkt"      cont@ mcont@)
 (only-in "../../conc/full/units.rkt" syntax@ debug@)
 (only-in "units.rkt"                 eval@ bind@ parse@ parser@
                                      expand@ expander@))
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parse@ parser@ expand@ expander@
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
