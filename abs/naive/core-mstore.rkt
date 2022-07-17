#lang racket
(require
 "../../mix.rkt"
 "../../example.rkt"

 ;;;; Signatures
 (only-in "../../signatures.rkt" syntax^ menv^ mstore^ run^ debug^)
 (only-in "../../conc/base/core/terms.rkt" terms^)

 ;;;; Units
 ;; reused
 (only-in "../../units.rkt"                terms-extra@ env@ menv@ io@)
 (only-in "../../conc/units.rkt"           delta@)
 (only-in "../../conc/base/units.rkt"      cont@ mcont@)
 (only-in "../../conc/base/core/units.rkt" terms@ syntax@ debug@)
 (only-in "../../conc/set/units.rkt"       store@ #;mstore@ bind@ run@)
 (only-in "../../conc/set/core/units.rkt"  eval@ parser@ expander@)

 ;; overridden
 (only-in "../../conc/set/units.rkt"       [mstore@ super:mstore@])
 ;; new
 (only-in "alloc.rkt" fin-alloc/mstore@))
(provide run mstore@)


(define-mixed-unit mstore@
  (import)
  (export mstore^)
  (inherit [super:mstore@ init-Σ lookup-Σ update-Σ]
           [fin-alloc/mstore@ alloc-name alloc-scope alloc-𝓁]))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples mode))
