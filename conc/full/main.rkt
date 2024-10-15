#lang racket
(require
 "../../interpreter.rkt"
 ;"../../test/suites.rkt"
 "../../signatures.rkt"
 ;;;; Units
 (only-in "../../units.rkt" io@)
 (only-in "../units.rkt" domain@ env@ store@ cont@ menv@ mstore@ mcont@ run@)
 (only-in "../phases/units.rkt" bind@)
 (only-in "units.rkt" syntax@ eval@ id@ parse@ parser@ expand@ expander@
          debug@))
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ id@ mcont@ parse@ parser@ expand@ expander@
         io@ run@ debug@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter 'base:full run delta α ≤a #f))

(define (process form [mode 'eval]) ;; mode = read/expand/parse/eval
  (apply-interpreter interp form mode))


;; run example
;; comment-out to avoid cyclic dependency from test/run.rkt
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α set=? #;≤a)
  (run-suite run delta (suite 'finite) mode α set=? #;≤a)
  (run-suite run delta (suite 'phases) mode α set=? #;≤a)
  (run-suite run delta (suite 'full)   mode α set=? #;≤a))
