#lang racket
(require
 "../../../set.rkt"
 "../../../example.rkt"
 
 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ domain^ debug^)
 
 ;;;; Units
 (only-in "../../../units.rkt"        terms-extra@ env@ menv@ io@)
 (only-in "../units.rkt"              store@ domain@ mstore@ bind@ run@)
 (only-in "../../base/units.rkt"      cont@ mcont@)
 (only-in "../../base/core/units.rkt" config@ syntax@ debug@)
 (only-in "units.rkt"                 eval@ parser@ expander@))
(provide run delta α ≤a)

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
  (run-examples run delta core:examples mode α set=? #;≤a))
