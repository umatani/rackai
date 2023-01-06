#lang racket
(require
 "../../interpreter.rkt"
 ;"../../test/suites.rkt"

 ;; Signatures
 (only-in "../../signatures.rkt" domain^ run^ debug^)

 ;; Units
 (only-in "../../units.rkt"      io@)
 (only-in "../units.rkt"         domain@ env@ store@ menv@ mstore@ run@)
 (only-in "../../conc/units.rkt" cont@ mcont@)
 (only-in "../../conc/full/units.rkt" syntax@ expander@ debug@)
 (only-in "units.rkt"            eval@ bind@ parser@ expand@))
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter 'mult:full run delta α ≤a #f))

#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'finite) mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a)
  (run-suite run delta (suite 'full)   mode α ≤a))