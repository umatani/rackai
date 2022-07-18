#lang racket
(require
 "../../../set.rkt"
 "../../../example.rkt"
 
 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ domain^ debug^)

 ;; Units
 (only-in "../../../units.rkt"          terms-extra@ env@ menv@ io@)
 (only-in "../units.rkt"                store@ domain@ mstore@ bind@ run@)
 (only-in "../../base/units.rkt"        cont@ mcont@)
 (only-in "../../base/phases/units.rkt" terms@ syntax@ debug@)
 (only-in "../core/units.rkt"           eval@)
 (only-in "units.rkt"                   parser@ expander@))
(provide run α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ domain^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ domain@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ domain^ debug^))

(define (main [mode 'check])
  (run-examples run core:examples   mode α set=? #;≤a)
  (run-examples run phases:examples mode α set=? #;≤a))
