#lang racket
(require
 "../../example.rkt"
 
 ;;;; Signatures
 (only-in "../../signatures.rkt" run^ debug^)

 ;;;; Units
 ;; reused
 (only-in "../../units.rkt"                  terms-extra@ env@ menv@ io@)
 (only-in "../../conc/units.rkt"             delta@)
 (only-in "../../conc/base/units.rkt"        cont@ mcont@)
 (only-in "../../conc/base/phases/units.rkt" terms@ syntax@ debug@)
 (only-in "../../conc/set/units.rkt"         #;store@ #;mstore@ bind@ run@)
 (only-in "../../conc/set/core/units.rkt"    eval@)
 (only-in "../../conc/set/phases/units.rkt"  parser@ expander@)
 ;; new
 (only-in "phases-store.rkt"  store@)
 (only-in "phases-mstore.rkt" mstore@))
(provide run)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))
