#lang racket
(require
 "../../test/suites.rkt"

 ;;;; Signatures
 (only-in "../../signatures.rkt" domain^ run^ debug^)

 ;;;; Units
 (only-in "../../units.rkt" io@)
 (only-in "../units.rkt"    domain@ env@ store@ cont@ menv@ mstore@ mcont@
                            bind@ run@)
 (only-in "units.rkt"       syntax@ eval@ parser@ expand@ expander@ debug@))
(provide run delta α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@
         io@ run@ debug@))
  (import) (export domain^ run^ debug^))

;; run example
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α set=? #;≤a)
  (run-suite run delta (suite 'finite) mode α set=? #;≤a))
