#lang racket
(require
 "../../test/suites.rkt"
 
 ;;;; Signatures
 (only-in "../../signatures.rkt" run^ domain^ debug^)
 
 ;;;; Units
 (only-in "../../units.rkt"                  io@)
 (only-in "../units.rkt"                     env@ store@ domain@ menv@ mstore@
                                             bind@ run@)
 (only-in "../../interp-base/units.rkt"      cont@ mcont@)
 (only-in "../../interp-base/core/units.rkt" config@ syntax@ expander@ debug@)
 (only-in "units.rkt"                        eval@ parser@ expand@))
(provide run delta α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α set=? #;≤a)
  (run-suite run delta (suite 'finite) mode α set=? #;≤a))
