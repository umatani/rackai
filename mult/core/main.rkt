#lang racket
(require
 "../../interpreter.rkt"
 ;"../../test/suites.rkt"
 
 ;;;; Signatures
 (only-in "../../signatures.rkt" domain^ run^ debug^)
 
 ;;;; Units
 (only-in "../../units.rkt"                  io@)
 (only-in "../units.rkt"                     domain@ env@ store@ menv@ mstore@
                                             bind@ run@)
 (only-in "../../conc/units.rkt"      cont@ mcont@)
 (only-in "../../conc/core/units.rkt" syntax@ expander@ debug@)
 (only-in "units.rkt"                        eval@ parser@ expand@))
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter 'mult:core run delta α ≤a #f))

;; run example
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α set=? #;≤a)
  (run-suite run delta (suite 'finite) mode α set=? #;≤a))
