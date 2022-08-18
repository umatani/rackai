#lang racket
(require
 ;"../../test/suites.rkt"

 ;;;; Signatures
 (only-in "../../signatures.rkt" domain^ run^ debug^)

 ;;;; Units
 (only-in "../../units.rkt" io@)
 (only-in "../units.rkt"    domain@ env@ store@ cont@ menv@ mstore@ mcont@
                            run@)
 (only-in "units.rkt"       syntax@ eval@ bind@ parser@
                            expand@ expander@ debug@))
(provide run delta α ≤a)

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export domain^ run^ debug^))

;; run example
;; comment-out to avoid cyclic dependency from test/run.rkt
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α set=? #;≤a)
  (run-suite run delta (suite 'finite) mode α set=? #;≤a)
  (run-suite run delta (suite 'phases) mode α set=? #;≤a)
  (run-suite run delta (suite 'full)   mode α set=? #;≤a))
