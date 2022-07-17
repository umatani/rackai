#lang racket
(require
 "../../mix.rkt"
 "../../example.rkt"
 
 ;;;; Signatures
 (only-in "../../signatures.rkt" store^ run^ debug^)
 (only-in "../../conc/base/phases/terms.rkt" terms^)

 ;;;; Units
 ;; reused
 (only-in "../../units.rkt"                  terms-extra@ env@ menv@ io@)
 (only-in "../../conc/units.rkt"             delta@)
 (only-in "../../conc/base/units.rkt"        cont@ mcont@)
 (only-in "../../conc/base/phases/units.rkt" terms@ syntax@ debug@)
 (only-in "../../conc/set/units.rkt"         #;store@ mstore@ bind@ run@)
 (only-in "../../conc/set/core/units.rkt"    eval@)
 (only-in "../../conc/set/phases/units.rkt"  parser@ expander@)
 ;; overridden
 (only-in "../../conc/set/units.rkt"         [store@ super:store@])
 ;; new
 (only-in "alloc.rkt" fin-alloc/store@))
(provide run store@)

(define-mixed-unit store@
  (import)
  (export store^)
  (inherit [super:store@ init-store lookup-store update-store update-store*]
           [fin-alloc/store@ alloc-loc alloc-loc*]))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))
