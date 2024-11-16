#lang racket/base
(require
 racket/unit
 "../interpreter.rkt"
 "../signatures.rkt"
 "../test/suites.rkt"
 (only-in "../mult/phases/units.rkt"
          common@ misc@ bind@ io@ syntax@ eval@ evaluator@
          expand@ expander@ domain@ env@ menv@ run@ parse@ parser@)
 (only-in "alloc.rkt" store@ mstore@))
(provide main-minus@ interp)


;;;; Main

(define-compound-unit/infer main-minus@
  (import domain^ syntax^ bind^ eval^ parser^ expand^)
  (export common^ misc^ env^ store^ evaluator^ menv^ mstore^ run^)
  (link   common@ misc@ env@ store@ evaluator@ menv@ mstore@
          expander@ io@ run@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^)
   (link main-minus@
         domain@ syntax@ bind@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^))

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
