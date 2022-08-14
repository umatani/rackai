#lang racket
(require
 "../../test/suites.rkt"

 ;; Signatures
 (only-in "../../signatures.rkt" run^ domain^ debug^)

 ;; Units
 (only-in "../../units.rkt"      terms-extra@ io@)
 (only-in "../units.rkt"         env@ store@ domain@ menv@ mstore@ run@)
 (only-in "../../interp-base/units.rkt"      cont@ mcont@)
 (only-in "../../interp-base/full/units.rkt" config@ syntax@ expander@ debug@)
 (only-in "units.rkt"                        eval@ bind@ parser@ expand@))
(provide run delta α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ terms-extra@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'finite) mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a)
  (run-suite run delta (suite 'full)   mode α ≤a))
