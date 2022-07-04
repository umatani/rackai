#lang racket
(require
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ debug^)

 ;;;; Units
 (only-in "../../../units.rkt" terms-extra@)
 (only-in "../units.rkt" bind@ run@)
 (only-in "units.rkt"
          terms@ syntax@ env@ store@ cont@ delta@ eval@
          menv@ mstore@ mcont@ parser@ expander@ io@ debug@))
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
