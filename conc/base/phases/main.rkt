#lang racket
(require
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ debug^)

 ;;;; Units
 (only-in "../../../units.rkt" terms-extra@ env@ menv@ io@)
 (only-in "../units.rkt"       cont@ store@ delta@ mstore@ mcont@ bind@ run@)
 (only-in "../core/units.rkt"  eval@)
 (only-in "units.rkt"          terms@ syntax@ parser@ expander@ debug@))
(provide run)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))
