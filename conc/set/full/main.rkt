#lang racket
(require
 "../../../set.rkt"
 "../../../example.rkt"

 ;; Signatures
 (only-in "../../../signatures.rkt" run^ domain^ debug^)

 ;; Units
 (only-in "../../../units.rkt"        terms-extra@ io@)
 (only-in "../units.rkt"              env@ store@ domain@ menv@ mstore@ run@)
 (only-in "../../base/units.rkt"      cont@ mcont@)
 (only-in "../../base/full/units.rkt" config@ syntax@ expander@ debug@)
 (only-in "units.rkt"                 eval@ bind@ parser@ expand@))
(provide run delta α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ terms-extra@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

(define (main [mode 'check])
  (run-examples run delta core:examples   mode α ≤a)
  (run-examples run delta phases:examples mode α ≤a)
  (run-examples run delta (append local:examples defs:examples)
                mode α ≤a))
