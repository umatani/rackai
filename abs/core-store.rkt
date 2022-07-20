#lang racket
(require
 "../mix.rkt"
 "../example.rkt"

 ;;;; Signatures
 (only-in "../signatures.rkt" store^ domain^ run^ debug^)
 (only-in "../conc/base/core/config.rkt" config^)

 ;;;; Units
 ;; reused
 (only-in "../units.rkt"                terms-extra@ env@ menv@ io@)
 (only-in "../conc/base/units.rkt"      cont@ mcont@)
 (only-in "../conc/base/core/units.rkt" config@ syntax@ debug@)
 (only-in "../conc/set/units.rkt"       domain@ #;store@ mstore@ bind@ run@)
 (only-in "../conc/set/core/units.rkt"  eval@ parser@ expander@)
 ;; overridden
 (only-in "../conc/set/units.rkt"       [store@ super:store@])
 ;; new
 (only-in "alloc.rkt"      fin-alloc/store@))
(provide run store@)

(define-mixed-unit store@
  (import)
  (export store^)
  (inherit [super:store@ init-store lookup-store update-store update-store*]
           [fin-alloc/store@ alloc-loc alloc-loc*]))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ terms-extra@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
(define (main [mode 'check])
  (run-examples run delta core:examples mode))
