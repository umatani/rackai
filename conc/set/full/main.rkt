#lang racket
(require
 "../../../set.rkt"
 "../../../example.rkt"

 ;; Signatures
 (only-in "../../../signatures.rkt" run^ domain^ debug^)

 ;; Units
 (only-in "../../../units.rkt"        terms-extra@ env@ menv@ io@)
 (only-in "../units.rkt"              store@ domain@ mstore@ run@)
 (only-in "../../base/units.rkt"      cont@ mcont@)
 (only-in "../../base/full/units.rkt" terms@ syntax@ debug@)
 (only-in "units.rkt"                 eval@ bind@ parser@ expander@))
(provide run α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ domain^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ domain@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ domain^ debug^))

(define (main [mode 'check])
  (run-examples run core:examples   mode α set=? #;≤a)
  (run-examples run phases:examples mode α set=? #;≤a)
  (run-examples run (append local:examples defs:examples) mode α set=? #;≤a))
