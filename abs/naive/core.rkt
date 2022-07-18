#lang racket
(require
 "../../example.rkt"

 ;;;; Signatures
 (only-in "../../signatures.rkt" run^ debug^)

 ;;;; Units
 ;; reused
 (only-in "../../units.rkt"                env@ menv@ io@)
 (only-in "../../conc/base/units.rkt"      cont@ mcont@)
 (only-in "../../conc/base/core/units.rkt" syntax@ debug@)
 (only-in "../../conc/set/units.rkt"       #;store@ #;mstore@ bind@ run@)
 (only-in "../../conc/set/core/units.rkt"  eval@ parser@ expander@)
 ;; new
 (only-in "core-store.rkt"  store@)
 (only-in "core-mstore.rkt" mstore@)
 (only-in "core-terms.rkt"  terms@ terms-extra@)
 (only-in "delta-unit.rkt"  delta@))
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
