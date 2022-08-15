#lang racket
(require
 ;"../../test/suites.rkt"

 ;;;; Signatures
 (only-in "../../signatures.rkt" run^ domain^ debug^)

 ;;;; Units
 (only-in "../../units.rkt" io@)
 (only-in "../units.rkt"    env@ store@ cont@ domain@ menv@ mstore@ mcont@
                            run@)
 (only-in "units.rkt"       syntax@ eval@ bind@ parser@
                            expand@ expander@ debug@))
(provide run delta α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
;; comment-out to avoid cyclic dependency from test/run.rkt
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α set=? #;≤a)
  (run-suite run delta (suite 'finite) mode α set=? #;≤a)
  (run-suite run delta (suite 'phases) mode α set=? #;≤a)
  (run-suite run delta (suite 'full)   mode α set=? #;≤a))
