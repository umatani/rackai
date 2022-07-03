#lang racket
(require
 "../../../example.rkt"
 
 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ debug^)
 
 ;;;; Units
 (only-in "../../../units.rkt" terms-extra@)
 (only-in "../units.rkt" run@)
 (only-in "../../base/core/units.rkt"
          terms@ syntax@ env@ cont@ menv@ mcont@ delta@ io@ debug@)
 (only-in "units.rkt"
          store@ eval@ mstore@ parser@ expand@))
(provide run)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ mcont@ parser@ expand@ io@ run@ debug@))
  (import) (export run^ debug^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples mode))
