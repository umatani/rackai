#lang racket
(require
 "../../../example.rkt"

 ;; Signatures
 (only-in "../../../signatures.rkt" run^ debug^)

 ;; Units
 (only-in "../../../units.rkt"        terms-extra@ env@ menv@ io@)
 (only-in "../units.rkt"              store@ delta@ mstore@ run@)
 (only-in "../../base/units.rkt"      cont@ mcont@)
 (only-in "../../base/full/units.rkt" terms@ syntax@ debug@)
 (only-in "units.rkt"                 eval@ bind@ parser@ expander@))
(provide run)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode)
  (run-examples run (append local:examples defs:examples) mode))
