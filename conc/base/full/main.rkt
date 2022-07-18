#lang racket
(require
 "../../../set.rkt"
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ domain^ debug^)

 ;;;; Units
 (only-in "../../../units.rkt" terms-extra@ env@ menv@ io@)
 (only-in "../units.rkt"       cont@ store@ domain@ mstore@ mcont@ run@)
 (only-in "units.rkt"          terms@ syntax@ eval@ bind@ parser@
                               expander@ debug@))
(provide run α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ domain^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ domain@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ domain^ debug^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples   mode α set=? #;≤a)
  (run-examples run phases:examples mode α set=? #;≤a)
  (run-examples run (append local:examples defs:examples) mode α set=? #;≤a))
