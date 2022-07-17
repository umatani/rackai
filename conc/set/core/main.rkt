#lang racket
(require
 "../../../example.rkt"
 
 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ debug^)
 
 ;;;; Units
 (only-in "../../../units.rkt"        terms-extra@ env@ menv@ io@)
 (only-in "../units.rkt"              store@ delta@ mstore@ bind@ run@)
 (only-in "../../base/units.rkt"      cont@ mcont@)
 (only-in "../../base/core/units.rkt" terms@ syntax@ debug@)
 (only-in "units.rkt"                 eval@ parser@ expander@))
(provide run)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples mode))
