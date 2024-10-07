#lang racket
(require
 "../../interpreter.rkt"
 "../../test/suites.rkt"

 ;;;; Signatures
 (only-in "../../signatures.rkt" domain^ run^ debug^)

 ;;;; Units
 (only-in "../../units.rkt" io@)
 (only-in    "../units.rkt" domain@ env@ store@ cont@ menv@ mstore@ mcont@
                            bind@ run@)
 (only-in       "units.rkt" syntax@ eval@ parser@ expand@ expander@ debug@))
(provide interp)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@
         io@ run@ debug@))
  (import) (export domain^ run^ debug^))

(define interp (make-checking-interpreter 'base:core run delta α ≤a))

(define (process form [mode 'eval]) ;; mode = read/expand/parse/eval
  (apply-interpreter interp form mode))

;; run examples
(define (main [mode 'check])
  (run-suite (suite 'core)   interp mode)
  (run-suite (suite 'finite) interp mode))
