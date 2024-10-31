#lang racket/base
(require
 racket/unit
 "../interpreter.rkt"
 "../signatures.rkt"
 (only-in "../reduction.rkt" enable-tracing)
 "../test/suites.rkt"
 "../base/phases/terms.rkt"
 (only-in "../mult/phases/units.rkt"
          common@ bind@ io@ debug@ syntax@ expander@ domain@ env@ menv@ run@
          parse@ parser@ [bind@ mult:bind@] id@)
 (only-in "../mult/phases/units.rkt"  eval@ expand@)
 (only-in "alloc.rkt"                 store@ mstore@)
 (only-in "core.rkt"                  evaluator@))
(provide syntax@ main-minus@ interp)


;;;; Main

(define-compound-unit/infer main-minus@
  (import domain^ eval^ parser^ expand^)
  (export common^ syntax^ env^ store^ evaluator^ menv^ mstore^ bind^ id^
          run^ debug^)
  (link   common@ syntax@ env@ store@ evaluator@ menv@ mstore@ bind@ id@
          expander@ io@ run@ debug@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link main-minus@
         domain@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp))


(module+ test1
  (interp '(let ([z 1])
             ((let-syntax ([x (lambda (stx) #'z)])
                (lambda (z) (x))) 2)))

  (interp '(let ([z 1])
             ((let-syntax ([x (lambda (stx) #'z)])
                (lambda (z) z)) 2))))

(module+ test2
  (interp '((lambda (f x) (f x))
            (lambda (x) x)
            100)))
