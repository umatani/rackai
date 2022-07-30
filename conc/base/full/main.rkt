#lang racket
(require
 "../../../set.rkt"
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../signatures.rkt" run^ domain^ debug^)

 ;;;; Units
 (only-in "../../../units.rkt" terms-extra@ env@ io@)
 (only-in "../units.rkt"       cont@ store@ domain@ menv@ mstore@ mcont@ run@)
 (only-in "units.rkt"          config@ syntax@ eval@ bind@ parser@
                               expander@ debug@))
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
  (run-examples run delta core:examples   mode α set=? #;≤a)
  (run-examples run delta phases:examples mode α set=? #;≤a)
  (run-examples run delta (append local:examples defs:examples)
                mode α set=? #;≤a))
