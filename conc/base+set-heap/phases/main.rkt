#lang racket
(require
 "../../../example.rkt"
 
 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ debug^)

 ;; Units
 (only-in "../../../units.rkt" terms-extra@)
 (only-in "../units.rkt" run@)
 (only-in "../../base/core/units.rkt"
          env@ cont@ delta@ menv@ mcont@ io@)
 (only-in "../../base/phases/units.rkt" terms@ syntax@ debug@)
 (only-in "../core/units.rkt" store@ eval@)
 (only-in "units.rkt" mstore@ parser@ expand@))
(provide run)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ mcont@ parser@ expand@ io@ run@ debug@))
  (import) (export run^ debug^))

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))
